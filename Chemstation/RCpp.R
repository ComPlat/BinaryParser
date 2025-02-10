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
# Read LCMS Data (R)
# ======================================================
read <- function(file_path) {
  type_size <- 2
  type <- "integer"
  endian <- "big"
  signed <- FALSE
  con <- file(file_path, "rb")
  file_size <- file.info(file_path)$size
  data_start <- 266 # Equivalent to 0x10A in decimal
  seek(con, where = data_start, origin = "start")

  # Read first short integer (offset correction)
  offset_correction <- readBin(con, type,
    size = type_size,
    n = 1, endian = endian,
    signed = signed
  )
  seek(con, where = data_start + offset_correction * 2 - 2, origin = "start")

  # Read the whole binary data
  data_size <- file_size - seek(con)
  num_elements <- data_size / type_size
  raw_data <- readBin(con, type,
    size = type_size,
    n = num_elements, endian = endian,
    signed = signed
  )
  close(con)
  return(raw_data)
}

file_path <- "./Chemstation/ChemStationData/LCMS_DatenAgilent_SVS/SVS_1025F1.D/MSD1.MS"
d_uint16 <- read(file_path)

# Extract m/z and intensity with correct encoding
extract_mz_intensity <- function(data) {
  mz_values <- data[seq(1, length(data), 2)] / 20
  encoded_intensities <- data[seq(2, length(data), 2)]
  head_bits <- encoded_intensities %/% 16384 # Extract high 2 bits
  tail_bits <- encoded_intensities %% 16384 # Extract lower 14 bits
  intensities <- (8^head_bits) * tail_bits
  data.frame(mz = mz_values, i = intensities)
}
df <- extract_mz_intensity(d_uint16)

# Read LCMS Data (C++)
# ======================================================
Rcpp::sourceCpp("./Chemstation/ms.cpp")

raw_cpp <- read_mz_intensity(file_path)
converted_cpp <- convert_mz_intensity(raw_cpp)
df_cpp <- data.frame(
  mz = converted_cpp[seq(1, length(converted_cpp), 2)],
  i = converted_cpp[seq(2, length(converted_cpp), 2)]
)
identical(df, df_cpp)
df <- df_cpp

# Detect cycles using the corrected approach
# ======================================================
detect_cycles <- function(df) {
  split_points <- which(df$mz == 0.25) # Cycle separators
  cycles <- list()
  start <- 1
  for (end in split_points) {
    if (length(start:end) > 3) {
      cycles[[length(cycles) + 1]] <- df[start:end, ]
    }
    start <- end + 1
  }
  return(cycles)
}

cycles <- detect_cycles(df)

# Correct cycles by filtering m/z range and removing duplicates
correct_cycles <- function(cycles, min_mz, max_mz) {
  lapply(cycles, function(cycle) {
    cycle <- cycle[cycle$mz >= min_mz & cycle$mz <= max_mz, ]
    cycle[!duplicated(cycle$mz), ]
  })
}

cycles <- correct_cycles(cycles, 100, 1500)

# Compute the total ion chromatogram (TIC)
tic <- sapply(cycles, function(cycle) sum(cycle$i))

# Display TIC
length(cycles)

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
