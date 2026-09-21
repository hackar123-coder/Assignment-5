# =============================================================================
# BDA400 Assignment 5: Technical Analysis using R (Development Phase)
# R/rsi.R -- Relative Strength Index (RSI, Wilder's smoothing)
# =============================================================================
#
# AI ASSISTANCE DECLARATION:
# I used Claude (Sonnet 5, Anthropic) on 18 September 2026 to implement, test,
# and debug this function from the assignment's provided description,
# template, and pseudocode. Prompts used are listed in full in
# docs/Student_BDA400_A05_Appendix.docx. Testing against a manual hand
# calculation uncovered a genuine double-counting bug in the pseudocode's
# smoothing loop (documented below and in the Appendix), which I corrected
# and re-verified by hand. All final calculations are done by myself. I am
# responsible for the accuracy and originality of this work.
# =============================================================================
#
# WHAT THIS DOES: measures the speed/size of recent gains versus recent
# losses on a 0-100 scale, using Wilder's smoothing method, so traders can
# spot overbought (>70) or oversold (<30) conditions.
#
# BUG FOUND DURING TESTING -- DOUBLE-COUNTED FIRST SMOOTHING STEP:
# The pseudocode seeds avg_gain/avg_loss as the simple mean of gains[1:period]
# and losses[1:period] (covering the first `period` price changes), then
# starts its Wilder-smoothing loop AT i = period + 1, immediately reusing
# gains[period]/losses[period] -- the same values already baked into the
# seed average. That means the first RSI value the loop produces partially
# double-counts one price change.
# Textbook Wilder's RSI instead uses the seed average DIRECTLY as the RSI at
# index (period + 1), and only starts the recursive smoothing update from
# index (period + 2) onward, using the next NEW gain/loss that hasn't been
# counted yet. On the assignment's own example data (period = 5), the
# literal pseudocode gives RSI[6] = 50.53, while a manual hand calculation
# and the corrected version below both give RSI[6] = 60.00 exactly -- a
# meaningful difference, not a rounding nuance (see the Appendix for the
# full worked comparison).

rsi <- function(data, period) {
  # Price-to-price changes: diff_values[i] = data[i+1] - data[i]
  diff_values <- data[-1] - data[-length(data)]

  # Split each change into its gain part (>0) or loss part (<0), with 0
  # standing in for "not applicable" so both vectors stay the same length
  # as diff_values and can be safely averaged/summed
  gains  <- ifelse(diff_values > 0, diff_values, 0)
  losses <- ifelse(diff_values < 0, -diff_values, 0)

  # Seed average: the plain mean of the first `period` gains and losses
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])

  rsi_values <- rep(NA_real_, length(data))

  # The seed average IS the RSI at index (period + 1) -- no extra smoothing
  # step needed here, since nothing has been double-counted yet
  rsi_values[period + 1] <- if (avg_loss == 0) {
    100
  } else {
    100 - (100 / (1 + avg_gain / avg_loss))
  }

  # From (period + 2) onward, apply Wilder's recursive smoothing using only
  # the newest, not-yet-counted gain/loss at each step
  if (length(data) > period + 1) {
    for (i in (period + 2):length(data)) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      rsi_values[i] <- if (avg_loss == 0) {
        100
      } else {
        100 - (100 / (1 + avg_gain / avg_loss))
      }
    }
  }

  return(rsi_values)
}

# -----------------------------------------------------------------------------
# TESTS
# -----------------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

  cat("=== rsi(data, period = 5) ===\n")
  print(rsi(data, period = 5))
  # VERIFIED OUTPUT:
  # [1]       NA       NA       NA       NA       NA 60.00000 74.40000 76.72727
  #  [9] 81.87611 70.22391

  cat("\n=== Manual hand-check of the seed value, RSI[6] ===\n")
  diff_values <- diff(data)
  gains  <- ifelse(diff_values > 0, diff_values, 0)
  losses <- ifelse(diff_values < 0, -diff_values, 0)
  cat("gains[1:5] =", gains[1:5], "  losses[1:5] =", losses[1:5], "\n")
  ag <- mean(gains[1:5]); al <- mean(losses[1:5])
  cat("seed avg_gain =", ag, " seed avg_loss =", al, "\n")
  cat("RSI[6] = 100 - 100/(1+avg_gain/avg_loss) =", 100 - 100 / (1 + ag / al), "\n")
  # VERIFIED OUTPUT: seed avg_gain = 2.4  seed avg_loss = 1.6 ; RSI[6] = 60
  # -- matches rsi_values[6] exactly, confirming the fix

  cat("\n=== Range check: every non-NA RSI value must fall within [0, 100] ===\n")
  vals <- rsi(data, period = 5)
  print(all(vals[!is.na(vals)] >= 0 & vals[!is.na(vals)] <= 100))
  # VERIFIED OUTPUT: [1] TRUE

  cat("\n=== Edge case: all-losses data (avg_loss never 0, RSI should approach 0) ===\n")
  print(rsi(c(100, 95, 90, 85, 80, 75, 70), period = 5))
  # VERIFIED OUTPUT: [1] NA NA NA NA NA 0 0
}
