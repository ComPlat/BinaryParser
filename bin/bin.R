# path <- "./bin/TestDaten/CV_new/EL_2Elec_4p79mg_Et4NBF4inACN_EDLC_Pl2_211024_05_CV_C11/data/table_01.jdx"
# [10] "##VOLTAGE_START=0.0"
# [11] "##SCAN_RATE=0.05"
# [12] "##VOLTAGE_LIMIT_ONE=2.700000047683716"
# [13] "##VOLTAGE_LIMIT_TWO=0.0"
# [14] "##RESOLUTION=Auto"
# [15] "##CYCLES=9"
# [16] "##DATE=45586"
# [17] "##TIME=.56551085648"
# [18] "##VOLTAGE_LIMIT_END=0.0"
# [19] "##SOFTWARE_VERSION=11.43"
# [20] "##CONCENTRATION_SALT=1"
# [21] "##FIRSTX=0.013841687701642513"
# [22] "##LASTX=-0.0004615047946572304"
# Voltage
# [23] "##MINX=-0.0009872515220195055" --> Set in device
# [24] "##MAXX=2.699223756790161" --> Set in device
# Current
# [25] "##MINY=-0.006186341957729539"
# [26] "##MAXY=0.007231874746488927"
# [27] "##NPOINTS=27030"
# [28] "##FIRSTY=3.6861687898635866e-05"
# [31] "##XYPOINTS=(XY..XY)"
# [32] "0.013841687701642513, 3.6861687898635866e-05"
# [33] "0.015812207013368607, 0.0002835869200211467"
# [34] "0.018651776015758514, 0.0006201966994459773"
# [35] "0.021376779302954674, 0.0009041020984360451"
# [36] "0.02406417950987816, 0.001166420330971846"
# [37] "0.026055805385112762, 0.0013446788548112476"
# [38] "0.02804425358772278, 0.001510192135544381"
library(ggplot2)
library(reshape2)
Rcpp::sourceCpp("./bin/bin.cpp")

# NOTE: print raw buffer pasted
pr <- function(v, sep) {
  v <- as.character(v)
  v <- paste(v, collapse = "")
  cat(v, sep)
}

vec_pad <- function(vec, size) {
  if (length(vec) < size) {
    stop("Vector is too small")
  }
  if (length(vec) / size == 0) {
    return(vec)
  } else {
    times <- length(vec) %/% size
    return(vec[1:(times * size)])
  }
}

bin <- R6::R6Class(
  "bin",
  public = list(
    path = NULL,
    raw = NULL,
    raw_subset = NULL,
    endian = "big", # NOTE: most devices are constructed for windows
    signed = FALSE,
    initialize = function(path) {
      self$path <- path
      con <- file(path, "rb")
      size <- file.info(path)$size
      self$raw <- readBin(con, what = "raw", n = size)
      close(con)
    },
    to_char = function(elem) {
      rawToChar(elem)
    },
    to_int8 = function(elem) {
      res <- NULL
      if (self$signed) {
        res <- rawToChar(elem) |> as.integer()
      } else {
        res <- rawToChar(elem) |> CastToUint8()
      }
      return(res)
    },
    to_int16 = function(vec) {
      op <- NULL
      if (self$signed) {
        op <- CastToInt16
      } else {
        op <- CastToUint16
      }
      vec <- as.character(vec)
      vec <- vec_pad(vec, 2)
      vec <- split(vec, rep(seq_len(length(vec) / 2), each = 2))
      res <- lapply(vec, function(x) {
        op(x)
      })
      names <- sapply(vec, function(i) {
        paste(i, collapse = "")
      })
      names(res) <- names
      res
    },
    to_int32 = function(vec) {
      op <- NULL
      if (self$signed) {
        op <- CastToInt32
      } else {
        op <- CastToUint32
      }
      vec <- as.character(vec)
      vec <- vec_pad(vec, 4)
      vec <- split(vec, rep(seq_len(length(vec) / 4), each = 4))
      res <- lapply(vec, op)
      names <- sapply(vec, function(i) {
        paste(i, collapse = "")
      })
      names(res) <- names
      res
    },
    print_char = function(idx) {
      cat(" ")
      for (i in idx:(idx + 7)) {
        temp <- self$to_char(self$raw_subset[i])
        if (temp == "") temp <- "."
        cat(temp, "\t")
      }
      cat("\n")
    },
    print_uint8 = function(idx) {
      cat(" ")
      for (i in idx:(idx + 7)) {
        temp <- self$to_int8(self$raw_subset[i])
        if (temp == "") temp <- "."
        cat(temp, "\t")
      }
      cat("\n")
    },
    print_uint16 = function(idx) {
      cat(" ")
      temp <- self$to_int16(self$raw_subset[idx:(idx + 7)])
      for (i in seq_along(temp)) {
        pr(names(temp)[i], "\t")
        cat(temp[[i]], "\t")
      }
      cat("\n")
    },
    print_uint32 = function(idx) {
      cat(" ")
      temp <- self$to_int32(self$raw_subset[idx:(idx + 7)])
      for (i in seq_along(temp)) {
        pr(names(temp)[i], "\t\t")
        cat(temp[[i]], "\t")
      }
      cat("\n")
    },
    print = function(range) {
      self$raw_subset <- self$raw[range]
      for (i in seq_along(self$raw_subset)) {
        cat(self$raw_subset[i], "\t")
        if (i %% 8 == 0) {
          cat("\n")
          self$print_char(i - 7)
          self$print_uint8(i - 7)
          self$print_uint16(i - 7)
          self$print_uint32(i - 7)
          cat("\n")
          cat("Elems: ", i + 1, " - ", i + 8, "\n")
        }
      }
    },
    plot = function(range, type, op = NULL) {
      self$raw_subset <- self$raw[range]
      x <- NULL
      y <- NULL
      if (type == "int8") {
        y <- sapply(self$raw_subset, self$to_int8)
        x <- as.character(self$raw_subset)
      } else if (type == "int16") {
        res <- self$to_int16(self$raw_subset)
        x <- names(res)
        y <- unlist(res)
        attributes(y) <- NULL
      } else if (type == "int32") {
        res <- self$to_int32(self$raw_subset)
        x <- names(res)
        y <- unlist(res)
        attributes(y) <- NULL
      } else {
        stop("found unknown type")
      }
      if (!is.null(op)) {
        stopifnot("op has to be a function" = is.function(op))
        y <- op(y)
      }
      colors <- rep(c("black", "darkred"), length.out = length(y))
      bp <- barplot(y, names.arg = NULL, col = colors, border = "black")
      text(
        x = bp,
        y = par("usr")[3] - 1,
        labels = x, srt = 90,
        cex = 0.75,
        adj = 1, xpd = TRUE
      )
    }
  )
)

path <- "./bin/TestDaten/CV_new/EL_2Elec_4p79mg_Et4NBF4inACN_EDLC_Pl2_211024_05_CV_C11.mpr"
b <- bin$new(path)
b$print(1:100)

b$signed <- FALSE
range <- 7200:8001
range <- 7247:(7247 + 400 * 4 - 1)
b$plot(range, "int16")

int_values <- b$to_int16(b$raw) |> unlist()
summary(int_values)
length(int_values)

# Determine cycle length
acf(int_values, lag.max = 10000, main = "Autocorrelation of Data")
acf_values <- acf(int_values, lag.max = 10000, plot = FALSE)$acf[-1]
lag_indices <- which(acf_values > 0.5)[1]
print(lag_indices)

# Plot raw data
indices <- seq(1, length(int_values), by = 80) # 100
data_start <- 0x1c40 # From hexdump
values <- int_values[data_start:(data_start + 10000)]
plot(values, type = "l", ylim = c(0, 10000))
abline(v = seq(1, length(values), by = 47), col = "red", lty = 2)
points(values, pch = 19)


# Print bits
bit_matrix <- uint16_to_bit_matrix(int_values)
df <- melt(bit_matrix)
colnames(bit_matrix) <- paste0("B_", 15:0)
colnames(df) <- c("Idx", "Bit_Pos", "Value")
df$Bit_Position <- as.numeric(gsub("B_", "", df$Bit_Pos))
ggplot(df, aes(x = Bit_Pos, y = Idx, fill = Value)) +
  geom_tile(width = 1, height = 5.5) +
  scale_fill_gradient(low = "white", high = "black") +
  scale_x_reverse(breaks = 15:0) +
  labs(
    x = "Bit Position", y = "Number Index",
    title = "Bit Pattern of int Values"
  ) +
  theme_minimal()
