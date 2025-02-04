read <- function(file_path, type = "uint16", endian = "big", signed = FALSE) {
  type_size <- NULL
  if (type == "uint16") {
    type_size <- 2
    type <- "integer"
  } else if (type == "int") {
    type_size <- 4
  } else if (type == "double") {
    type_size <- 4 # ???
  } else if (type == "raw") {
    type_size <- 1
  } else {
    stop("Invalid type")
  }
  con <- file(file_path, "rb")
  file_size <- file.info(file_path)$size
  data_start <- 752 # Assuming data starts at 0x02f0 (752 in decimal)
  data_size <- file_size - data_start
  num_elements <- data_size / type_size
  seek(con, where = data_start, origin = "start")
  data <- readBin(
    con, type,
    size = type_size,
    n = num_elements,
    endian = endian,
    signed = signed
  )
  close(con)
  return(data)
}

file_path <- "./Chemstation/ChemStationData/LCMS_DatenAgilent_SVS/SVS_1025F1.D/MSD1.MS"
d_raw <- read(file_path, type = "raw")
d_raw <- split(d_raw, rep(1:(length(d_raw) / 2), each = 2))
odd <- d_raw[
  seq_along(d_raw) %% 2 != 0
]
even <- d_raw[
  seq_along(d_raw) %% 2 == 0
]

normalise <- function(intensities) {
  max_intensity <- max(intensities)
  (intensities / max_intensity) * 100
}

convert <- function(x) {
  hex_val <- paste0(format(x[1], width = 2), format(x[2], width = 2))
  int_val <- as.integer(paste0("0x", hex_val))
  int_val / 20
}

df <- lapply(seq_len(length(even)), function(x) {
  mz <- convert(odd[[x]])
  i <- convert(even[[x]])
  data.frame(mz = mz, i = i * 20)
}) |> do.call(what = rbind)

# INFO:Hypothesis
# When the mz spectra is clicked at a certain time the vendor
# software does not show one measurement cycle
# When merging several measurement cycles the output is the same
# as in the agilent software.
# TODO: Therefore, the time for one cycle has to be determined
min_mz <- 100
max_mz <- 1500
df <- df[df$mz > min_mz, ]
df <- df[df$mz < max_mz, ]
df <- df[df$i > 0, ]
df$idx <- seq_len(dim(df)[1])
start <- 136359 - 1000 # From: env$areas[[264]][1] - 1000
sub <- df[start:(start + 5000), ]
sub$i <- normalise(sub$i)

# INFO: Plot shows that 237.2 is across several measurement cycles
# the mz value with the highest intensity
plot(sub$idx, sub$i, type = "l")
indices <- which(sub$i > 40)
text(sub[indices, "idx"], sub[indices, "i"], sub[indices, "mz"])

# INFO: Between the measurement cycles the mz value 237.2 is
# measured additionally. Is this value to be considered?
plot(sub$idx, sub$mz)
text(sub[indices, "idx"], sub[indices, "mz"], sub[indices, "mz"])

# INFO: Start time 1.40 mins; end time 10.0 mins
# Step size 0.1
# dim(df) is 180836 rows and 3 columns
# 10 - 1.4 is 8.6 mins
# 516 seconds
# ca. 350 measurements per second
#
# ModeSIM
# on
# ==> Maybe this means that
# first the entire cycle is measured.
# Afterwards specific ions (e.g. 237.2) are measured inbetween

# Seperate sub into cycles and SIM
row.names(sub) <- seq_len(nrow(sub))
sub <- sub[-1:-364, ]
row.names(sub) <- seq_len(nrow(sub))
head(sub)
sub_sim <- list()
sub_sim[[1]] <- sub[1:2, ]
sub_cycle <- list()
sub_cycle[[1]] <- sub[3:535, ]
sub_sim[[2]] <- sub[536, ]
sub_cycle[[2]] <- sub[537:1052, ]
sub_sim[[3]] <- sub[1053:1054, ]
sub_cycle[[3]] <- sub[1055:1568, ]

p_mz <- function(df) {
  plot(df$mz, df$i, type = "l")
  # indices <- which(df$i > 40)
  indices <- order(df$i, decreasing = TRUE)[1:5]
  text(df[indices, "mz"], df[indices, "i"], df[indices, "mz"])
  print(min(df$mz))
  print(df[indices, ])
}
p_mz(sub_cycle[[1]])
p_mz(sub_cycle[[2]])
p_mz(sub_cycle[[3]])

plot(sub_sim[[1]]$mz, sub_sim[[1]]$i)
plot(sub_sim[[2]]$mz, sub_sim[[2]]$i)
plot(sub_sim[[3]]$mz, sub_sim[[3]]$i)

# NOTE: Time per cycle
# 533, 516, and 514 measurements in sub_cycle
# 1.52, 1.47, and 1.47 seconds per cycle
# Thus, I cannot pool cycles?
# But how do i consider the SIM methods?

sapply(seq_len(length(sub_sim)), function(x) {
  cycle <- sub_cycle[[x]]
  cycle <- cycle[cycle$mz == 237.2, ]
  sim <- sub_sim[[x]]
  sim <- sim[sim$mz == 237.2, ]
  c(cycle$i, sim$i)
})
# INFO: Cycle and sim show the same values
#          [,1]     [,2] [,3]
# [1,] 77.61878 91.71437  100
# [2,] 77.61878 91.71437  100
# >

library(plotly)
df <- df[df$mz < 600, ]
df$i <- normalise(df$i)
df <- df[df$i > 10, ]
fig <- plot_ly(
  df,
  x = ~idx,
  y = ~mz,
  z = ~i,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 2, color = ~i, colorscale = "Viridis", opacity = 0.7)
)
fig
htmlwidgets::saveWidget(fig, "lcms_3d_plot.html", selfcontained = TRUE)

# NOTE: Testing the vendor software
# 6.484, 6.503, 6.521
# 0.019 seconds between each clickable mz spectra
# Probably, my cycle time is wrong.

# Count number of cycles
# ===============================================================================
ca <- function(a, b) {
  if ((a < 0) || (b < 0)) {
    return(FALSE)
  }
  (1 - a / b) < 0.2
}

find_start_point <- function(env, start, max_mz) {
  for (i in start:length(env$mz)) {
    if (ca(env$mz[i], max_mz)) {
      return(i)
    }
  }
}

find_end_point <- function(env, start, min_mz) {
  if (length(start) == 0) {
    return()
  }
  prev <- env$mz[start]
  for (i in start:length(env$mz)) {
    if (ca(min_mz, env$mz[i])) {
      return(i)
    }
    if (prev < env$mz[i]) {
      if (i != start) {
        return(i - 1)
      }
    } else {
      prev <- env$mz[i]
    }
  }
}

detect_areas <- function(env, start, min_mz, max_mz) {
  starts <- find_start_point(env, start, max_mz)
  ends <- find_end_point(env, starts, min_mz)
  if ((length(starts) == 0) || (length(ends) == 0)) {
    return()
  }
  if (is.null(starts) || is.null(ends)) {
    return()
  }
  if (ends >= length(env$data)) {
    return()
  }
  if (abs(ends - length(env$mz)) <= 1) {
    return()
  }
  start <- ends + 1
  if ((ends - starts) > 3) {
    env$areas <- append(env$areas, list(c(starts, ends)))
  }
  detect_areas(env, start, min_mz, max_mz)
}
env <- new.env()
env$mz <- df$mz
env$i <- env$i
env$data <- d_raw

detect_areas(env, 1, 100, 1500)
cycles <- lapply(env$areas, function(x) df[x[1]:x[2], ])
length(cycles)
# NOTE: 292 cycles
# 8.6 minutes
# 8.6 / 292 = 0.0295 minutes per cycle
lengths <- sapply(cycles, function(x) dim(x)[1])
sum(lengths)
length(df$i)
# Data loss by splitting into cycles?
# Or just the effect from the strange end?

tic <- sapply(cycles, function(x) {
  sum(x$i)
})
plot(tic, type = "l")

p_mz1 <- function(df) {
  df$i <- normalise(df$i)
  plot(df$mz, df$i, type = "l")
  indices <- which(df$i > 40)
  text(df[indices, "mz"], df[indices, "i"], df[indices, "mz"])
  print(df[indices, ])
}

idx <- which.max(tic)
p_mz1(cycles[[idx]])

# NOTE: Hypthesis
# As the relative peak height is not correct
# And the 237.2 mz peak should be much larger
# And the 237.2 mz is the current chosen mz ion
# Test whether the peak height of another sim ion
# is also much larger in the mz spectra
# as expected by the cycle data

# Extract SIM data
umps <- do.call(c, env$areas)
jumps <- jumps[-c(1, length(jumps))]
jumps <- split(jumps, rep(1:(length(jumps) / 2), each = 2))
sim <- lapply(jumps, function(x) df[x[1]:x[2], ])
head(sim, 20)
