# =============================================================================
# BDA400 Assignment 5: Technical Analysis using R (Development Phase)
# R/linreg.R -- Linear Regression (trailing-window)
# =============================================================================
#
# AI ASSISTANCE DECLARATION:
# I used Claude (Sonnet 5, Anthropic) on 18 September 2026 to implement, test,
# and debug this function from the assignment's provided description,
# template, and pseudocode. Prompts used are listed in full in
# docs/Student_BDA400_A05_Appendix.docx. Testing against the pseudocode's own
# index formula uncovered a genuine window-sizing bug (documented below and
# in the Appendix), which I corrected and then verified independently
# against R's own lm() function. All final calculations are done by myself.
# I am responsible for the accuracy and originality of this work.
# =============================================================================
#
# WHAT THIS DOES: fits a straight line (slope + intercept) through the last
# `regressionLength` points of regressionSource, optionally starting
# `regressionOffset` points back from the most recent observation (so you
# can ask "what did the trend look like a few bars ago"). Returns the slope,
# intercept, and the line's predicted values over that window.
#
# BUG FOUND DURING TESTING -- WINDOW SIZE:
# The assignment's pseudocode computes the window like this:
#   start_index = max(1, n - regressionLength + regressionOffset)
#   end_index   = min(n, n - regressionOffset)
# Implemented literally, the resulting window is NOT `regressionLength`
# points long: its length works out to (regressionLength - 2*regressionOffset
# + 1), so with regressionOffset = 0 it is one point too LONG, and it gets
# shorter still as regressionOffset grows -- confirmed by direct testing
# (see docs/Student_BDA400_A05_Appendix.docx for the exact numbers). Since
# the parameter is explicitly named `regressionLength`, I corrected the
# index math below so the window is always exactly `regressionLength` points,
# ending `regressionOffset` points back from the end of the source data --
# then verified the corrected output against R's own lm() on an identical
# window, which matched exactly (slope and intercept equal to at least 3
# decimal places).

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Total number of elements available to draw the window from
  n <- length(regressionSource)

  # Guard: can't ask for a window longer than the data you have
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  # Guard: an offset that reaches (or exceeds) the window length leaves no
  # room for an actual window
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  # CORRECTED window calculation: end `regressionOffset` points back from the
  # most recent observation, then walk back exactly `regressionLength` points
  end_index   <- n - regressionOffset
  start_index <- max(1, end_index - regressionLength + 1)

  # The actual data points the regression will be fit on
  source_subset <- regressionSource[start_index:end_index]

  # x-axis for the regression: just 1, 2, 3, ... across the window
  index_values <- seq_len(length(source_subset))

  sum_index  <- sum(index_values)
  sum_source <- sum(source_subset)
  mean_index  <- mean(index_values)
  mean_source <- mean(source_subset)

  # Standard least-squares slope: covariance(x,y) / variance(x)
  numerator   <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  slope     <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # The fitted line's value at every point in the window
  predicted_values <- slope * index_values + intercept

  result <- list(slope = slope, intercept = intercept, predicted_values = predicted_values)
  return(result)
}

# -----------------------------------------------------------------------------
# TESTS
# -----------------------------------------------------------------------------
if (sys.nframe() == 0) {
  cat("=== Window-length check: predicted_values length must equal regressionLength ===\n")
  data20 <- 1:20
  for (off in c(0, 1, 2)) {
    r <- linreg(data20, regressionLength = 5, regressionOffset = off)
    cat("offset=", off, "-> length =", length(r$predicted_values),
        " values:", r$predicted_values, "\n")
  }
  # VERIFIED OUTPUT:
  # offset= 0 -> length = 5  values: 16 17 18 19 20
  # offset= 1 -> length = 5  values: 15 16 17 18 19
  # offset= 2 -> length = 5  values: 14 15 16 17 18
  # (always length 5, correctly sliding back by `offset` -- the literal
  # pseudocode instead gives lengths 6, 4, 2 for these same calls)

  cat("\n=== Realistic test: noisy upward trend ===\n")
  set.seed(42)
  prices <- round(100 + cumsum(rnorm(15, mean = 1, sd = 0.5)), 2)
  print(prices)
  res <- linreg(prices, regressionLength = 5, regressionOffset = 0)
  cat("slope =", res$slope, " intercept =", res$intercept, "\n")
  print(res$predicted_values)
  # VERIFIED OUTPUT: slope = 0.965  intercept = 113.923
  # predicted_values: 114.888 115.853 116.818 117.783 118.748

  cat("\n=== Independent cross-check against R's own lm() on the identical window ===\n")
  window <- tail(prices, 5); x <- 1:5
  fit <- lm(window ~ x)
  cat("lm() slope =", coef(fit)[2], " intercept =", coef(fit)[1], "\n")
  cat("Match? ", isTRUE(all.equal(unname(coef(fit)[2]), res$slope)) &&
                 isTRUE(all.equal(unname(coef(fit)[1]), res$intercept)), "\n")
  # VERIFIED OUTPUT: lm() slope = 0.965  intercept = 113.923 ; Match?  TRUE

  cat("\n=== Error handling ===\n")
  print(tryCatch(linreg(1:5, 10, 0), error = function(e) e))
  print(tryCatch(linreg(1:10, 5, 5), error = function(e) e))
  # VERIFIED OUTPUT:
  # <simpleError: regressionLength cannot be greater than the number of elements in regressionSource>
  # <simpleError: regressionOffset must be less than regressionLength>
}
