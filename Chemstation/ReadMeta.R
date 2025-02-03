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

info_path <- "./SVS-1025F1.D/MSACQINF.REG"
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
