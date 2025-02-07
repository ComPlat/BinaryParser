# Correct tic
# ======================================================
file_path_exported_tic <- "./Chemstation/ExportedSpectra.txt"
lines <- readLines(file_path_exported_tic)
vec <- lapply(lines, function(x) {
  res <- strsplit(x, ",")[[1]]
  res <- gsub(" ", "", res)
  res
}) |> do.call(what = c)
names(vec) <- NULL
num_vec <- sapply(seq_len(length(vec)), function(i) {
  res <- as.numeric(vec[i])
  if (is.na(res)) {
    print(i)
  }
  res
})
tic_dev <- data.frame(
  time = num_vec[seq(1, length(num_vec), 2)],
  tic = num_vec[seq(2, length(num_vec), 2)]
)

# Read tic
# ======================================================
read <- function(file_path) {
  type_size <- 2
  type <- "integer"
  endian <- "big"
  signed <- FALSE
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

d_uint16 <- read(file_path)
mz <- d_uint16[seq(1, length(d_uint16), 2)] / 20
i <- d_uint16[seq(2, length(d_uint16), 2)]
df <- data.frame(mz = mz, i = i)
# INFO: 0.25 is the end of a cycle!

# split in cycles
# ======================================================
ca <- function(a, b) {
  if ((a < 0) || (b < 0)) {
    return(FALSE)
  }
  arg <- (1 - a / b)
  if (arg < 0) {
    return(FALSE)
  }
  arg < 0.2
}

find_start_point <- function(env, start, max_mz) {
  for (i in start:length(env$mz)) {
    if (ca(env$mz[i], max_mz)) {
      return(i)
    }
  }
  return(NULL)
}

find_end_point <- function(env, start, min_mz) {
  for (i in start:length(env$mz)) {
    if (env$mz[i] == 0.25) {
      return(i)
    }
  }
  return(NULL)
}

detect_areas <- function(env, min_mz, max_mz) {
  start <- find_start_point(env, 1, 1500)
  end <- start
  while (end < length(env$data)) {
    end <- find_end_point(env, start, min_mz)
    if (is.null(end)) {
      break
    }
    if ((end - start) > 3) { # TODO: 3 is somehow arbitrary
      env$areas <- append(env$areas, list(c(start, end)))
    }
    start <- end + 1
  }
}

correct_cycles <- function(cycles, min_mz, max_mz) {
  lapply(cycles, function(x) {
    x <- x[x$mz >= min_mz & x$mz <= max_mz, ]
    x[!duplicated(x$mz), ]
  })
}

extract_inter_blocks <- function(data, areas) {
  inter_blocks <- list()
  inter_blocks[[1]] <- data[1:7, ]
  counter <- 2
  for (i in seq_along(areas)[-length(areas)]) {
    start <- areas[[i]][2]
    end <- areas[[i + 1]][1] + 5
    if (start <= end) {
      inter_blocks[[counter]] <- data[start:end, ][1:7, ]
      counter <- counter + 1
    }
  }
  return(inter_blocks)
}

env <- new.env()
env$mz <- df$mz
env$i <- env$i
env$data <- d_uint16
detect_areas(env, 100, 1500)
inter_blocks <- extract_inter_blocks(df, env$areas)
cycles <- lapply(env$areas, function(x) df[x[1]:x[2], ])
cycles <- correct_cycles(cycles, 100, 1500)

tic <- sapply(cycles, function(x) {
  sum(x$i)
})

# Comparison
# ======================================================
dim(tic_dev)
length(tic)

time <- tic_dev$time
tic_device <- tic_dev$tic
plot(
  time,
  tic_device,
  type = "l",
  ylim = c(2e04, 9e05)
)
points(
  time, tic_device,
  cex = 0.5
)
lines(
  time,
  tic,
  type = "l",
  col = "darkred"
)
points(
  time,
  tic,
  cex = 0.5,
  col = "darkred"
)
indices <- seq(1, length(tic), 10)
text(
  time[indices],
  tic[indices],
  labels = indices,
  pos = 3,
  col = "darkblue",
  cex = 0.7
)
for (i in indices) {
  abline(v = time[i], col = "gray", lty = 2)
}

max_i <- lapply(cycles, function(x) {
  x[which.max(x$i), ]
}) |> do.call(what = rbind)

tic_dev$tic_calc <- tic
tic_dev$diff <- tic_dev$tic - tic_dev$tic_calc
tic_dev$mz_inner_block <- sapply(inter_blocks, function(x) {
  x[7, ]
})[1, ] |>
  unlist()
tic_dev$i_inner_block <- sapply(inter_blocks, function(x) {
  x[7, ]
})[2, ] |>
  unlist()

# Can i extract i max true? --> No
tic_dev$mz_max <- max_i$mz
tic_dev$i_max <- max_i$i
tic_dev[1:20, ]
points(tic_dev$time, tic_dev$i_max * 10, col = "blue", pch = 19)

# TODO: Scaling
# ======================================================
# The tic of the vendor software
# is always higher. Often it has values
# above the range of uint16
# Dynamic scaling factor
# which is also in the block.
# Alternative: Other data format --> actually no space for it


env$areas[1:8]
df[2670:2700, ]
# TIC cycle 1.57 minutes => 6
# This fits with the mass table
# Only checked roughly.
# But the ION 406.6 is different
# Here: 121608 is measured for 406.6 m/z
# I found 31585
# uint16 0-65535
# Unlikely other format as mz (e.g. 406.6), intensity, new cycle
# Still can be checked best use the first block before the first cycle
121608 / 31585 # 3.850182
# 121608 = 1db08
# 31585 = 7b 61
# 00 05 00 04
# 00 00 00 06
# ad bc 03 78
# 00 01 70 0f
# 03 70 00 01
# 00 00 01 b5
# 1f c4 7b 61

# Hex d67
env$areas[1:8]
# TIC cycle 1.589 minutes => 7
df[3110:3130, ]
# Vendor: 406.6, 177024.0 (2b380)
# calc: 406.6, 35534
177024 / 35534 # 4.98182 (8a ce)

df[1:20, ]
# 357.3; 28008 in mass table at 1.479
# 6d 68 = 28008
# calc: 19885 (4dad)
28008 / 19885 # 1.408499
# 00 00 04 de
# 00 01 5a 93
# 04 d6 00 01
# 00 00 02 68
# 1b ea 4d ad

# Check if block requires other data types
# ======================================================
read_raw <- function(file_path) {
  type <- "raw"
  con <- file(file_path, "rb")
  data_start <- 752 # Assuming data starts at 0x02f0 (752 in decimal)
  num_elements <- file.info(file_path)$size - data_start
  seek(con, where = data_start, origin = "start")
  data <- readBin(
    con, type,
    n = num_elements
  )
  close(con)
  return(data)
}
d_raw <- read_raw(file_path)
d_raw[1:24]
head(df, 11)
d_raw[1:24] |>
  strtoi(base = 16) |>
  as.integer()


d_raw[1:20000]



# Testing ideas
# ======================================================
library(ggplot2)
p <- function(df) {
  df <- df[5:9, ]
  f1 <- sapply(inter_blocks[5:9], function(x) {
    x[7, 2]
  })
  f1 <- f1 * (mean(df$tic) / mean(f1))
  df$f1 <- f1
  ggplot(data = df) +
    geom_point(aes(x = time, y = tic), col = "darkred") +
    geom_line(aes(x = time, y = tic), col = "darkred") +
    geom_point(aes(x = time, y = tic_calc), col = "black") +
    geom_line(aes(x = time, y = tic_calc), col = "black") +
    geom_point(aes(x = time, y = f1), col = "darkblue") +
    geom_line(aes(x = time, y = f1), col = "darkblue")
}
p(tic_dev)
inter_blocks[[1]]
inter_blocks[5:9]
d_raw[1:22]



raw <- 19885
expected <- 28008
raw <- 35534
expected <- 177024


loss_fct <- function(params) {
  val <- params[1] * (raw)^params[2]
  abs(val - expected)
}
res <- optim(c(0, 0), loss_fct)
res
res$par[1] * (raw)^res$par[2]


loss_fct <- function(params) {
  val <- params[1] * exp((raw) * params[2])
  abs(val - expected)
}
res <- optim(c(0, 0), loss_fct)
res
res$par[1] * exp(raw * res$par[2])
