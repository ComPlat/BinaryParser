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

ca <- function(a, b) {
  if ((a < 0) || (b < 0)) {
    return(FALSE)
  }
  (1 - a / b) < 0.1
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

remove_garbage <- function(mz, intensities, min_mz, max_mz) {
  remove_idx <- which(mz < min_mz * 0.9)
  remove_idx <- c(remove_idx, which(mz > max_mz * 1.1))
  mz <- mz[-remove_idx]
  intensities <- intensities[-remove_idx]
  remove_idx <- which(intensities < 0)
  if (length(remove_idx) > 0) {
    mz <- mz[-remove_idx]
    intensities <- intensities[-remove_idx]
  }
  return(list(mz, intensities))
}

normalise <- function(intensities) {
  max_intensity <- max(intensities)
  (intensities / max_intensity) * 100
}

split_data <- function(data, env, min_mz, max_mz) {
  data <- data / 20
  n <- length(data)
  mz <- data[seq(1, n, 2)]
  intensities <- data[seq(2, n, 2)] * 20
  l <- remove_garbage(mz, intensities, min_mz, max_mz)
  mz <- l[[1]]
  intensities <- l[[2]]
  env$mz <- mz
  env$intensities <- intensities
  detect_areas(env, 1, min_mz, max_mz)
  mz <- lapply(env$areas, function(x) mz[x[1]:x[2]])
  intensities <- lapply(env$areas, function(x) intensities[x[1]:x[2]])
  env$mz <- mz
  env$intensities <- intensities
}

tic_plot <- function(input) {
  tic <- sapply(seq_along(unique(input$idx)), function(i) {
    sum(
      input[input$idx == i, "intensities"]
    )
  })
  tic_max <- which.max(tic)
  plot(tic, type = "l")
  return(tic_max)
}

mz_plot <- function(input, idx) {
  df <- input[input$idx == idx, ]
  df$intensities <- normalise(df$intensities)
  plot(
    df$mz,
    df$intensities,
    type = "l"
  )
  indices <- which(df$intensities > 40)
  temp <- df[indices, ]
  text(
    temp$mz, temp$intensities, temp$mz
  )
}

merge_data <- function(env) {
  lapply(seq_len(length(env$mz)), function(i) {
    data.frame(idx = i, mz = env$mz[[i]], intensities = env$intensities[[i]])
  }) |> do.call(what = rbind)
}

baseline_correction <- function(df) {
  baseline <- stats::runmed(df$intensities, k = 101)
  df$intensities <- df$intensities - baseline
  return(df)
}

sgolay_smoothing <- function(df, poly_order = 3, window_size = 11) {
  df$intensities <- signal::sgolayfilt(
    df$intensities,
    p = poly_order, n = window_size
  )
  df <- df[df$intensities >= 0, ]
  df
}

gaussian_kernel <- function(size, sigma) {
  x <- seq(-size / 2, size / 2, by = 1)
  kernel <- exp(-x^2 / (2 * sigma^2))
  return(kernel / sum(kernel))
}

gaussian_smoothing <- function(df, window_size = 1, sigma = 1) {
  peak_region_intensities <- df$intensities
  kernel <- gaussian_kernel(window_size, sigma)
  mean_intensity <- mean(peak_region_intensities)
  padded_data <- c(
    rep(mean_intensity, window_size),
    peak_region_intensities, rep(mean_intensity, window_size)
  )
  smoothed_intensities <- convolve(padded_data, kernel, type = "filter")
  smoothed_intensities <- spline(
    x = seq_along(smoothed_intensities), y = smoothed_intensities,
    xout = seq_along(peak_region_intensities)
  )$y
  df$intensities <- smoothed_intensities
  df
}

file_path <- "./SVS-1025F1.D/MSD1.MS"
min_mz <- 100
max_mz <- 1500
data <- read(file_path)
env <- new.env()
env$data <- data
split_data(data, env, min_mz, max_mz)
df <- merge_data(env)
df <- gaussian_smoothing(df)
mz_plot(df, 264)
