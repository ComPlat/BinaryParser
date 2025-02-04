read_text <- function(file_path) {
  file_size <- file.info(file_path)$size
  raw_data <- readBin(file_path, "raw", n = file_size, size = 1)
  # INFO: 0x20-0x7E printible characters
  text_bytes <- raw_data[raw_data >= 0x20 & raw_data <= 0x7E]
  text_data <- rawToChar(text_bytes)
  text_data <- strsplit(text_data, " ")[[1]]
  text_data <- paste(text_data, collapse = "\n")
  cat(text_data)
}

info_path <- "./Chemstation/ChemStationData/LCMS_DatenAgilent_SVS/SVS_1025F1.D/MSACQINF.REG"
read_text(info_path)
read_text("./SVS-1025F1.D/MSDIAG.REG")

file_path <- "./SVS-1025F1.D/MSD1.MS"
file_size <- file.info(file_path)$size
raw_data <- readBin(file_path, "raw", n = file_size, size = 1)
text_bytes <- raw_data[raw_data >= 0x20 & raw_data <= 0x7E]
text_data <- rawToChar(text_bytes)
text_data <- strsplit(text_data, " ")[[1]]
text_data <- sapply(text_data, function(x) x)
text_data <- text_data[text_data != ""]
head(text_data, n = 12)
text_data[13]


# Initial slope sensitivity (Full scan) 1.000
# Initial peak width (Full Scan) 0.250
# Slope sensitivity (Cond. Scan / SIM) 0.100
# Initial Peak width (Cond. Scan / SIM) 0.05
#
# ACQ.MS
# Cycle time 100 probably ms
# Run duration 588 in seconds
#
# MSD Window = 1.0
# Indicates the time window
# grouping or smoothing data points to identify peaks.
