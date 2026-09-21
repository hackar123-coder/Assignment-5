# =============================================================================
# BDA400 Assignment 5: Technical Analysis using R (Development Phase)
# R/stoch_rsi.R -- Stochastic RSI
# =============================================================================
#
# AI ASSISTANCE DECLARATION:
# I used Claude (Sonnet 5, Anthropic) on 18 September 2026 to implement, test,
# and debug this function from the assignment's provided description,
# template, and pseudocode. Prompts used are listed in full in
# docs/Student_BDA400_A05_Appendix.docx. I verified outputs by manually
# recomputing the %K values by hand from this file's own rsi() output. All
# final calculations are done by myself. I am responsible for the accuracy
# and originality of this work.
# =============================================================================
#
# WHAT THIS DOES: RSI itself is bounded 0-100, but within a given window it
# often only uses part of that range. StochRSI rescales RSI's own recent
# highs and lows to a strict 0-1 scale, making overbought/oversold
# conditions easier to spot. %K is that rescaled line (smoothed over
# k_period); %D is a further smoothing of %K over d_period.
# Depends on rsi() (R/rsi.R) and sma() (R/sma.R).

.get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_flag <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_flag) > 0) return(dirname(normalizePath(sub("^--file=", "", file_flag[1]))))
  frames <- sys.frames()
  for (f in rev(frames)) if (!is.null(f$ofile)) return(dirname(normalizePath(f$ofile)))
  getwd()
}
if (!exists("rsi")) source(file.path(.get_script_dir(), "rsi.R"))
if (!exists("sma")) source(file.path(.get_script_dir(), "sma.R"))

stoch_rsi <- function(data, period, k_period, d_period) {
  # Step 1: the underlying RSI series (will contain leading NAs -- see rsi())
  rsi_values <- rsi(data, period)

  # Step 2: rescale RSI's own recent range to [0, 1]. na.rm = TRUE so the
  # leading NAs from the RSI warm-up period don't break min()/max()
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)

  # Step 3: %K line = SMA of the rescaled values (drop the leading NAs first,
  # since sma() -- per its own spec -- does not accept NA input)
  k_line <- sma(k_values[!is.na(k_values)], k_period)

  # Step 4: %D line = a further SMA smoothing of %K
  d_line <- sma(k_line, d_period)

  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}

# -----------------------------------------------------------------------------
# TESTS
# -----------------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

  cat("=== stoch_rsi(data, period=5, k_period=3, d_period=3) ===\n")
  print(stoch_rsi(data, period = 5, k_period = 3, d_period = 3))
  # VERIFIED OUTPUT:
  # $k_line
  # [1] 0.4742964 0.8076297 0.7439972
  # $d_line
  # [1] 0.6753078

  cat("\n=== Manual cross-check of k_values before smoothing ===\n")
  rv <- rsi(data, 5)
  rv_valid <- rv[!is.na(rv)]
  cat("RSI values used:", rv_valid, "\n")
  mn <- min(rv_valid); mx <- max(rv_valid)
  kv <- (rv_valid - mn) / (mx - mn)
  cat("k_values (before SMA):", kv, "\n")
  cat("manual sma(k_values, 3)[1] = mean of first 3 =", mean(kv[1:3]), "\n")
  # VERIFIED OUTPUT: k_values: 0 0.6582524 0.7646367 1 0.4673551
  # manual sma(k_values,3)[1] = 0.4742964 -- matches k_line[1] above

  cat("\n=== Range check: %K and %D must fall within [0, 1] ===\n")
  res <- stoch_rsi(data, 5, 3, 3)
  print(all(res$k_line >= 0 & res$k_line <= 1))
  print(all(res$d_line >= 0 & res$d_line <= 1))
  # VERIFIED OUTPUT: [1] TRUE   [1] TRUE
}
