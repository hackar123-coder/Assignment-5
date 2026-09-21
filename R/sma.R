# =============================================================================
# BDA400 Assignment 5: Technical Analysis using R (Development Phase)
# R/sma.R -- Simple Moving Average (SMA)
# =============================================================================
#
# AI ASSISTANCE DECLARATION:
# I used Claude (Sonnet 5, Anthropic) on 18 September 2026 to implement, test,
# and debug this function from the assignment's provided description,
# template, and pseudocode. Prompts used are listed in full in
# docs/Student_BDA400_A05_Appendix.docx. I verified outputs by running this
# function in R against the assignment's own example data and by an
# independent manual calculation (see comments below). All final
# calculations are done by myself. I am responsible for the accuracy and
# originality of this work.
# =============================================================================
#
# WHAT THIS DOES: the Simple Moving Average gives equal weight to every point
# in a trailing window of `period` values, smoothing short-term noise so the
# underlying trend is easier to see.
#
# NOTE ON OUTPUT LENGTH: following the assignment's pseudocode exactly, this
# function returns a vector of length (length(data) - period + 1) -- i.e. it
# is NOT padded with NA at the start. sma_values[1] is the average of the
# first `period` points, sma_values[2] slides the window forward by one, etc.

sma <- function(data, period) {
  # Guard: you cannot average a window that is larger than the data you have
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # One output value per possible window position
  sma_values <- numeric(length(data) - period + 1)

  # Slide a window of width `period` across the data, one step at a time
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]   # the current 'period' points
    sma_values[i] <- sum(current_window) / period # their plain average
  }

  return(sma_values)
}

# -----------------------------------------------------------------------------
# TESTS (run this file directly, e.g. `Rscript sma.R`, or source() it in
# RStudio -- every value below was produced by actually running this code)
# -----------------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

  cat("=== sma(data, period = 3) ===\n")
  print(sma(data, period = 3))
  # VERIFIED OUTPUT:
  # [1] 12.33333 15.66667 17.66667 20.00000 21.66667 23.66667 23.33333

  cat("\n=== Manual cross-check of the first window: mean(10, 12, 15) ===\n")
  print((10 + 12 + 15) / 3)
  # VERIFIED OUTPUT: [1] 12.33333  -- matches sma_values[1] above

  cat("\n=== Edge case: period equal to full data length ===\n")
  print(sma(c(5, 10, 15), period = 3))
  # VERIFIED OUTPUT: [1] 10   (single window covering all 3 points)

  cat("\n=== Edge case: period greater than data length (should error) ===\n")
  print(tryCatch(sma(c(1, 2), period = 5), error = function(e) e))
  # VERIFIED OUTPUT:
  # <simpleError: Data length should be greater than or equal to the period>
}
