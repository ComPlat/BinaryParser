# uint16s are used for m/z values and intensities.
# They alternate, so the pattern would be:
# MS range is from 50 - 1500
# 1st uint16: m/z,
# 2nd uint16: intensity,
# 3rd uint16: m/z,
# 4th uint16: intensity, and so on.
# Run was 20 minutes long --> image from 11.771 min

read <- function(file_path, type, endian, signed = TRUE) {
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

# TODO: does not work 100% correctly?
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
  intensities / max_intensity
}

split_data <- function(data, env, min_mz, max_mz) {
  data <- data / 20
  n <- length(data)
  mz <- data[seq(1, n, 2)]
  intensities <- data[seq(2, n, 2)] * 20
  l <- remove_garbage(mz, intensities, min_mz, max_mz)
  mz <- l[[1]]
  intensities <- l[[2]]
  # intensities <- normalise(intensities)
  env$mz <- mz
  env$intensities <- intensities
  detect_areas(env, 1, min_mz, max_mz)
  mz <- lapply(env$areas, function(x) mz[x[1]:x[2]])
  intensities <- lapply(env$areas, function(x) intensities[x[1]:x[2]])
  env$mz <- mz
  env$intensities <- intensities
}

gaussian_kernel <- function(size, sigma) {
  x <- seq(-size / 2, size / 2, by = 1)
  kernel <- exp(-x^2 / (2 * sigma^2))
  return(kernel / sum(kernel))
}

# hexdump -C MSACQINF.REG --> Hint that gaussian is used
#  .A.p.p.l.i.c.a.|
# |b.l.e...G.a.u.s.|
# |s.i.a.n...0...3.|
# |0. .D.a...G.a.u.|
# |s.s.i.a.n...0...|
# |0.5.0. .m.i.n.u.| --> measurement cycle of 30 seconds??? --> No!
# |t.e.s...........|
# |........T.u.n.e.|
# |F.i.l.e.P.a.t.h.|
# |..D.:.\.C.h.e.m.|
# |3.2.\.1.\.1.9.4.|
# |6.T.U.N.E.\...T.|
apply_gaussian_smoothing <- function(env, idx, window_size = 1, sigma = 1) {
  peak_region_intensities <- env$intensities[[idx]]
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
  env$smoothed_intensities[[idx]] <- smoothed_intensities
  if (idx == length(env$mz)) {
    return()
  }
  apply_gaussian_smoothing(env, idx + 1, window_size, sigma)
}

file_path <- "./X176.D/MSD1.MS" # 50 - 1500
# |.......S.t.a.r.t|
# |.T.i.m.e.....1..| --> Start time 1.4 min
# |.4.0...L.o.w.M.a|
# |.s.s.....1.0.0..|
# |.0.0...H.i.g.h.M|
# |.a.s.s.....1.5.0|
# |.0...0.0...G.a.i|
# |.n.....1...0.0..|
# |.F.r.a.g.m.e.n.t|
# |.o.r.....7.0...T|
# |.h.r.e.s.h.o.l.d|
# |.....1.5.0...S.t| --> Stepsize 0.01 sec for one measurement???
# |.e.p.s.i.z.e....|
# |.0...1.0........|
# |...........S.i.g|
# |.n.a.l.D.e.s.c..|
# |.....M.a.s.s._.O|
# |.f.f.s.e.t.3....|
file_path <- "/home/konrad/Documents/GitHub/LCMS/LCMS-Daten Agilent_SVS/SVS-1025F1.D/MSD1.MS" # 100 - 1500
min_mz <- 100
max_mz <- 1500
data <- read(file_path, "uint16", "big", signed = FALSE)
# Signed is FALSE this is important. Thereby, the 8*10^5 is the same TIC dimension as in vendor software
env <- new.env()
env$data <- data
split_data(data, env, min_mz, max_mz)
apply_gaussian_smoothing(env, 1)
# Calc TIC
tic <- sapply(seq_len(length(env$mz)), function(i) {
  sum(env$smoothed_intensities[[i]])
})

plot(tic, type = "l")

max_tic <- which.max(tic)
max_value <- max(sapply(env$intensities[60:270], max))

p <- function(mz, i) {
  i <- (i / max_value) * 100
  baseline <- stats::runmed(i, k = 5)
  i <- i - baseline
  idx <- which(i > 0)
  i <- i[idx]
  mz <- mz[idx]
  plot(
    mz, i,
    type = "l",
    xlim = c(100, 800)
  )
}
p(env$mz[[max_tic]], env$smoothed_intensities[[max_tic]])
p(env$mz[[max_tic]], env$intensities[[max_tic]])

# data <- head(data, 5 * 10^5) # For  file_path <- "./X176.D/MSD1.MS" # 50 - 1500
data <- read(file_path, "uint16", "big", signed = FALSE)
even <- data[
  (seq_along(data) %% 2) == 0
]
even <- even[even > 150] # This is the threshold???
odd <- data[
  (seq_along(data) %% 2) != 0
] / 20
par(mfrow = c(2, 1))
plot(odd, cex = 0.01, col = "darkred")
plot(even, cex = 0.1)
# TODO:
# - Find out what the linear lines in the even plot are
# - Identify the mass threshold i.e. which values are noise and which are not
# - Identify the end of the measurement cycles

# NOTE: Hypothesis that the artefact values occur regularly
# Identify indices of values over 3E04. Here only those artefacts are visible.
art <- which(even > 3E04)
art
diff(art)
# There is no strict regular pattern visible:
#  [1]   145   295   132   288     2   127   278   418   417
#  428   422   423   417   407   425   420   419   420   433
#  420   410   410   427   428   440   434   424   422
plot(diff(art)) # But they occur relativly regular
table(diff(art))


data <- read(file_path, "uint16", "big", signed = FALSE)
even <- data[
  (seq_along(data) %% 2) == 0
]
even <- even[even > 150] # This is the threshold???
odd <- data[
  (seq_along(data) %% 2) != 0
] / 20
par(mfrow = c(3, 1))
plot(odd, cex = 0.01, col = "darkred")
plot(even, cex = 0.1)
even_splitted <- do.call(c, env$intensities)
plot(even_splitted, cex = 0.1)
