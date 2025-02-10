number_cycles <- function(file_path) {
  type_size <- 4
  type <- "integer"
  endian <- "big"
  con <- file(file_path, "rb")
  data_start <- 0x116
  seek(con, where = data_start, origin = "start")
  readBin(con, type,
    size = type_size,
    n = 1, endian = endian
  )
}

file_path <- "./Chemstation/ChemStationData/LCMS_DatenAgilent_SVS/SVS_1025F1.D/MSD1.MS"
number_cycles(file_path)



read_data <- function(file_path) {
  con <- file(file_path, "rb")
  data_start <- 0x116
  seek(con, where = data_start, origin = "start")
  num_times <- readBin(con, integer(), size = 4, n = 1, endian = "big", signed = FALSE)
  times <- integer(num_times)
  pair_counts <- integer(num_times)
  pair_bytearr <- raw(0)

  data_start <- 266 # Equivalent to 0x10A in decimal
  seek(con, where = data_start, origin = "start")

  # Read first short integer (offset correction)
  offset_correction <- readBin(con, "integer",
    size = 2,
    n = 1, endian = "big",
    signed = FALSE
  )
  seek(con, where = data_start + offset_correction * 2 - 2, origin = "start")

  counter <- 1
  for (i in seq_len(num_times)) {
    readBin(con, "raw", size = 1, n = 2) # Skip 2 bytes
    times_temp <- readBin(con, integer(), size = 4, n = 1, endian = "big", signed = FALSE)
    if (length(times_temp) == 0) next
    times[counter] <- times_temp
    readBin(con, "raw", size = 1, n = 6) # Skip 6 bytes
    pair_counts[counter] <- readBin(con, integer(), size = 2, n = 1, endian = "big", signed = FALSE)
    readBin(con, "raw", size = 1, n = 4) # Skip 4 bytes
    pair_bytes <- readBin(con, "raw", size = 1, n = pair_counts[i] * 4)
    pair_bytearr <- c(pair_bytearr, pair_bytes)
    readBin(con, "raw", size = 1, n = 10) # Skip 10 bytes
    counter <- counter + 1
  }
  close(con)
  times <- times / 60000
  return(list(times = times, pair_counts = pair_counts, pair_bytes = pair_bytearr))
}

# Example usage
result <- read_data(file_path)
str(result)
