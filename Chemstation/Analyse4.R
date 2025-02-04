read_raw <- function(file_path) {
  seek(env$con, where = 752, origin = "start")
  data <- readBin(
    env$con, "raw",
    size = 1, n = 1L
  )
  env$start <- env$start + 1
  return(data)
}

convert <- function(x) {
  hex_val <- paste0(format(x[1], width = 2), format(x[2], width = 2))
  int_val <- as.integer(paste0("0x", hex_val))
  int_val / 20
}

convert4 <- function(x) {
  hex_val <- paste0(
    format(x[1], width = 2), format(x[2], width = 2),
    format(x[3], width = 2), format(x[4], width = 2)
  )
  int_val <- as.integer(paste0("0x", hex_val))
  int_val / 20
}

read_uint16 <- function(env) {
  convert(env$data[env$start:(env$start + 1)])
}

ca <- function(a, b) {
  if ((a < 0) || (b < 0)) {
    return(FALSE)
  }
  (1 - a / b) < 0.1
}

found_start <- function(data, max_mz) {
  if (ca(data, max_mz)) {
    if ((data / max_mz) > 1) {
      return(FALSE)
    }
    return(TRUE)
  }
  return(FALSE)
}

found_end <- function(data, min_mz, prev_mz) {
  if (!is.null(prev_mz) && data > prev_mz) {
    return(TRUE)
  }
  if (data < min_mz) {
    return(TRUE)
  }
  return(FALSE)
}

find_start_point <- function(env, max_mz) {
  while (env$start < length(env$data)) {
    dp <- read_uint16(env)
    if (found_start(dp, max_mz)) {
      return(env$start)
    }
    env$start <- env$start + 2
  }
}

in_cycle <- function(dp, env, min_mz, max_mz) {
  if (found_start(dp, max_mz)) {
    env$in_cycle <- TRUE
    env$prev_mz <- dp
    env$counter_cycle <- env$counter_cycle + 1
  }
  if (found_end(dp, min_mz, env$prev_mz)) {
    env$in_cycle <- FALSE
    env$counter_not_cycle <- env$counter_not_cycle + 1
  }
}

read <- function(file_path, min_mz, max_mz) {
  con <- file(file_path, "rb")
  size <- file.info(file_path)$size - 752
  seek(con, where = 752, origin = "start")
  raw <- readBin(
    con, "raw",
    n = size,
    endian = "big", signed = FALSE
  )
  close(con)
  env <- new.env()
  env$data <- raw
  env$start <- 1
  env$start <- find_start_point(env, max_mz)
  env$in_cycle <- FALSE
  env$prev_mz <- NULL
  env$not_cycle <- list()
  env$counter_not_cycle <- 0
  env$counter_cycle <- 0
  env$cycle <- list()
  for (i in 1:size) {
    dp <- read_uint16(env)
    if (i %% 2 != 0) {
      in_cycle(dp, env, min_mz, max_mz)
    }
    if (!env$in_cycle) {
      if (length(env$not_cycle) < env$counter_not_cycle) {
        env$not_cycle[[env$counter_not_cycle]] <- env$data[
          env$start:(env$start + 1)
        ]
      } else {
        env$not_cycle[[env$counter_not_cycle]] <- c(
          env$not_cycle[[env$counter_not_cycle]],
          env$data[env$start:(env$start + 1)]
        )
      }
    } else {
      if (length(env$cycle) < env$counter_cycle) {
        env$cycle[[env$counter_cycle]] <- dp
      } else {
        env$cycle[[env$counter_cycle]] <- c(
          env$cycle[[env$counter_cycle]], dp
        )
      }
    }
    env$start <- env$start + 2
  }
  not_cycle <- lapply(env$not_cycle, function(x) {
    starts <- seq(1, length(x), 2)
    c2 <- sapply(starts, function(i) {
      convert(x[i:(i + 1)])
    })
    starts <- seq(1, length(x), 4)
    c4 <- sapply(starts, function(i) {
      convert4(x[i:(i + 3)])
    })
    list(x, c2, c4)
  })

  mz <- lapply(env$cycle, function(x) {
    x[seq(1, length(x), 2)]
  })
  i <- lapply(env$cycle, function(x) {
    x[seq(2, length(x), 2)]
  })

  print(not_cycle)
  # print(mz)
  return()
}

file_path <- "./Chemstation/ChemStationData/LCMS-Daten Agilent_SVS/SVS-1025F1.D/MSD1.MS"
df <- read(file_path, 100, 1500)

# INFO: Hypothesis:
# The SIM values are not saved as uint16
# TODO: New read data function



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

file_path <- "./Chemstation/ChemStationData/LCMS-Daten Agilent_SVS/SVS-1025F1.D/MSD1.MS"
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

min_mz <- 100
max_mz <- 1500
df$idx <- seq_len(dim(df)[1])
start <- 136359
sub <- df[start:(start + 5000), ]
plot(sub$idx, sub$i, type = "l")
indices <- head(order(sub$i, decreasing = TRUE), 100)
text(sub[indices, "idx"], sub[indices, "i"], sub[indices, "mz"])

# NOTE: mz 0.25 shows high values
sub_top <- sub[indices, ]
sub_top <- sub_top[order(sub_top$idx), ]
sub_top$diff <- c(NA, diff(sub_top$idx))
sub_top

plot(sub_top$idx, sub_top$i, type = "l")

# This is exaclty one of the SIM areas
start <- which(sub$idx == 138374)
sim <- sub[start:(start + 6), ]
sim
start_idx <- sim[1, "idx"] * 2
start_idx <- start_idx - 1
raw <- d_raw[start_idx:(start_idx + 14)]
sim$raw_mz <- sapply(raw, function(x) {
  paste0(format(x[1], width = 2), format(x[2], width = 2))
})[seq(1, 13, 2)]
sim$raw_i <- sapply(raw, function(x) {
  paste0(format(x[1], width = 2), format(x[2], width = 2))
})[seq(2, 14, 2)]
sim

raw_string <- lapply(raw, function(x) {
  paste0(x[1], " ", x[2])
})
raw_string <- do.call(paste, raw_string)

hex_values <- as.raw(strtoi(unlist(strsplit(raw_string, " ")), base = 16))
interpret_numbers <- function(raw_data) {
  n <- length(raw_data)
  uint8 <- as.integer(raw_data)
  int8 <- ifelse(uint8 > 127, uint8 - 256, uint8)
  int16_be <- readBin(
    raw_data,
    what = "integer", size = 2, n = n / 2, signed = TRUE, endian = "big"
  )
  int16_le <- readBin(
    raw_data,
    what = "integer", size = 2, n = n / 2, signed = TRUE, endian = "little"
  )
  int32_be <- readBin(
    raw_data,
    what = "integer", size = 4, n = n / 4, signed = TRUE, endian = "big"
  )
  int32_le <- readBin(
    raw_data,
    what = "integer", size = 4, n = n / 4, signed = TRUE, endian = "little"
  )
  float32_be <- readBin(
    raw_data,
    what = "double", size = 4, n = n / 4, signed = TRUE, endian = "big"
  )
  float32_le <- readBin(
    raw_data,
    what = "double", size = 4, n = n / 4, signed = TRUE, endian = "little"
  )
  list(
    uint8 = uint8,
    int8 = int8,
    int16_big_endian = int16_be,
    int16_little_endian = int16_le,
    int32_big_endian = int32_be,
    int32_little_endian = int32_le,
    float32_big_endian = float32_be,
    float32_little_endian = float32_le
  )
}
result <- interpret_numbers(hex_values)
print(result)
print(lapply(result, \(x) x / 20))
sim
