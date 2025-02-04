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
    if ((end - start) > 3) {
      env$areas <- append(env$areas, list(c(start, end)))
    }
    start <- end + 1
  }
}
env <- new.env()
env$mz <- df$mz
env$i <- env$i
env$data <- d_raw
detect_areas(env, 100, 1500)
cycles <- lapply(env$areas, function(x) df[x[1]:x[2], ])
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

mz_plot <- function(input, idx, min_mz, max_mz) {
  df <- input[[idx]]
  df <- df[df$mz > min_mz, ]
  print(df)
  df$i <- normalise(df$i)
  plot(
    df$mz,
    df$i,
    type = "l",
    xlim = c(min_mz, max_mz),
  )
  indices <- which(df$i > 40)
  temp <- df[indices, ]
  text(
    temp$mz, temp$i, temp$mz
  )
}
mz_plot(cycles, 138, 100, 1500)
