# BDA400 Assignment 5: Technical Analysis using R (Development Phase) macd.R, plus probably Moving Average Convergence Divergence (MACD)
#
# AI HELP DECLARATION:
# I used Claude (Anthropic, Claude Sonnet 5) in September 2026 to use and review this function from the assignment's description, template and pseudocode. I am responsible for the accuracy and originality of this work.
#
# WHAT THIS DOES: MACD measures momentum by checking a fast EMA to a slow
# EMA. The gap, as a matter of fact, between them (the "MACD line") is itself smoothed with another
# EMA (the "signal line"); the difference between the two is the histogram, which is what most charting platforms show as bars. This function depends on ema() from ema.R, so it sources that file rather than redefining EMA.

# This function depends on ema(). To make macd.R runnable on its own, in fact, from any working directory (Rscript, RStudio "Source", or sourced by another script), we locate this file's own folder and source ema.R from there, using only base-R functions (no libraries).
.get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_flag <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_flag) > 0) return(dirname(normalizePath(sub("^--file=", "", file_flag[1]))))
  frames <- sys.frames()
  for (f in rev(frames)) if (!is.null(f$ofile)) return(dirname(normalizePath(f$ofile)))
  getwd()
}
if (!exists("ema")) source(file.path(.get_script_dir(), "ema.R"))

macd <- function(data, short_period, long_period, signal_period) {
  # Fast-moving average reacts quickly; slow-moving average lags behind
  short_ema <- ema(data, short_period)
  long_ema  <- ema(data, long_period)

  # The gap between the two EMAs is the MACD line itself
  macd_line <- short_ema - long_ema

  # Smooth the MACD line to get the signal line (used to spot crossovers)
  signal_line <- ema(macd_line, signal_period)

  # Histogram = how far the MACD line currently is from its own signal line
  histogram <- macd_line - signal_line

  result <- list(macd_line = macd_line, signal_line = signal_line, histogram = histogram)
  return(result)
}

# TESTS
if (sys.nframe() == 0) {
  data <- c(100, 105, 110, 115, 120, 125, 130)

  cat("=== macd(data, short_period=3, long_period=5, signal_period=2) ===\n")
  print(macd(data, short_period = 3, long_period = 5, signal_period = 2))
  # CHECKED OUTPUT:
  # $macd_line
  # [1] 0.0000000 0.8333333 1.8055556 2.6620370 3.3371914 3.8393776 4.2002100
  # $signal_line
  # In other words, [1] 0.0000000 0.5555556 1.3888889 2.2376543 2.9706790 3.5498114 3.9834105
  # $histogram
  # The catch is, [1] 0.0000000 0.2777778 0.4166667 0.4243827 0.3665123 0.2895662 0.2167996

  cat("\n=== Manual cross-check: macd_line[1] must be 0 (short_ema[1] == long_ema[1] == data[1]) ===\n")
  print(macd(data, 3, 5, 2)$macd_line[1])
  # CHECKED OUTPUT: [1] 0, both EMAs are seeded with the same first price

  cat("\n=== Consistency check: histogram should always equal macd_line - signal_line ===\n")
  res <- macd(data, 3, 5, 2)
  print(all.equal(res$histogram, res$macd_line - res$signal_line))
  # CHECKED OUTPUT: [1] TRUE

  cat("\n=== Trending-up data uptrend sanity check: MACD line should stay positive ===\n")
  print(all(macd(c(100, 102, 105, 109, 114, 120, 127, 135), 3, 6, 2)$macd_line[-1] > 0))
  # CHECKED OUTPUT: [1] TRUE, fast EMA (short_period) stays above the slower long-period EMA throughout a kept up uptrend, as expected
}