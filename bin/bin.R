library(ggplot2)
library(reshape2)
Rcpp::sourceCpp("./bin/bin.cpp")

# NOTE: print raw buffer pasted
pr <- function(v, sep) {
  v <- as.character(v)
  v <- paste(v, collapse = "")
  cat(v, sep)
}

bin <- R6::R6Class(
  "bin",
  public = list(
    path = NULL,
    raw = NULL,
    raw_subset = NULL,
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
    to_uint8 = function(elem) {
      rawToChar(elem) |> CastToUint8()
    },
    to_uint16 = function(vec) {
      vec <- as.character(vec)
      vec <- split(vec, rep(seq_len(length(vec) / 2), each = 2))
      res <- lapply(vec, function(x) {
        # x <- rev(x)
        CastToUint16(x)
      })
      names <- sapply(vec, function(i) {
        paste(i, collapse = "")
      })
      names(res) <- names
      res
    },
    to_uint32 = function(vec) {
      vec <- as.character(vec)
      vec <- split(vec, rep(seq_len(length(vec) / 4), each = 4))
      res <- lapply(vec, CastToUint32)
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
        temp <- self$to_uint8(self$raw_subset[i])
        if (temp == "") temp <- "."
        cat(temp, "\t")
      }
      cat("\n")
    },
    print_uint16 = function(idx) {
      cat(" ")
      temp <- self$to_uint16(self$raw_subset[idx:(idx + 7)])
      for (i in seq_along(temp)) {
        pr(names(temp)[i], "\t")
        cat(temp[[i]], "\t")
      }
      cat("\n")
    },
    print_uint32 = function(idx) {
      cat(" ")
      temp <- self$to_uint32(self$raw_subset[idx:(idx + 7)])
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
      if (type == "uint8") {
        y <- sapply(self$raw_subset, self$to_uint8)
        x <- as.character(self$raw_subset)
      } else if (type == "uint16") {
        res <- self$to_uint16(self$raw_subset)
        x <- names(res)
        y <- unlist(res)
        attributes(y) <- NULL
      } else if (type == "uint32") {
        res <- self$to_uint32(self$raw_subset)
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

path <- "./tests/Chemstation/SVS_1025F1.D/MSD1.MS"
b <- bin$new(path)
# b$print(1:320)

range <- 753:(753 + 4951)
range <- range[seq(1, length(range), 2)]
range <- range[seq(2, length(range), 2)]
b$plot(range, "uint16")

uint16_values <- b$to_uint16(b$raw_subset) |> unlist()
bit_matrix <- uint16_to_bit_matrix(uint16_values)
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
    title = "Bit Pattern of uint16 Values"
  ) +
  theme_minimal()
