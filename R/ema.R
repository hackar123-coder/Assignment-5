# =============================================================================
# BDA400 Assignment 5: Technical Analysis using R (Development Phase)
# R/ema.R -- Exponential Moving Average (EMA)
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
# WHAT THIS DOES: unlike SMA, EMA weights recent points more heavily, so it
# reacts faster to new price movements. It is seeded with the first data
# point, then each later value blends the new price with the previous EMA
# using a "multiplier" (also called the smoothing factor) derived from the
# period.

ema <- function(data, period) {
  # Smoothing factor: bigger period -> smaller multiplier -> smoother/slower EMA
  multiplier <- 2 / (period + 1)

  ema_values <- numeric(length(data))

  for (i in 1:length(data)) {
    if (i == 1) {
      # No prior EMA exists yet, so the series is seeded with the raw price
      ema_values[i] <- data[i]
    } else {
      # Blend today's price with yesterday's EMA, weighted by the multiplier
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}

# -----------------------------------------------------------------------------
# TESTS
# -----------------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

  cat("=== ema(data, period = 3) ===\n")
  print(ema(data, period = 3))
  # VERIFIED OUTPUT:
  # [1] 10.00000 11.00000 13.00000 16.50000 17.25000 19.62500 22.31250 23.15625
  # [9] 22.07812

  cat("\n=== Manual cross-check of ema_values[2]: (12-10)*multiplier + 10 ===\n")
  multiplier <- 2 / (3 + 1)
  print((12 - 10) * multiplier + 10)
  # VERIFIED OUTPUT: [1] 11  -- matches ema_values[2] above

  cat("\n=== Edge case: single data point (should just return that point) ===\n")
  print(ema(c(42), period = 5))
  # VERIFIED OUTPUT: [1] 42

  cat("\n=== Edge case: period = 1 (multiplier = 1 -> EMA tracks price exactly) ===\n")
  print(ema(data, period = 1))
  # VERIFIED OUTPUT: identical to `data`, since multiplier = 2/2 = 1
}
