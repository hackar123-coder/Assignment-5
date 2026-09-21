# =============================================================================
# BDA400 Assignment 5: Technical Analysis using R (Development Phase)
# R/stdev.R -- Standard Deviation
# =============================================================================
#
# AI ASSISTANCE DECLARATION:
# I used Claude (Sonnet 5, Anthropic) on 18 September 2026 to implement, test,
# and debug this function from the assignment's provided description,
# template, and pseudocode. Prompts used are listed in full in
# docs/Student_BDA400_A05_Appendix.docx. I verified outputs by running this
# function in R, by an independent manual calculation, and by comparing
# against (and explaining the deliberate difference from) R's built-in sd().
# All final calculations are done by myself. I am responsible for the
# accuracy and originality of this work.
# =============================================================================
#
# WHAT THIS DOES: measures how spread out the data is around its own mean.
#
# IMPORTANT -- POPULATION vs SAMPLE STANDARD DEVIATION:
# The assignment's formula divides the sum of squared differences by n (the
# number of data points): sigma = sqrt( sum((x_i - mean)^2) / n ).
# This is the POPULATION standard deviation. R's built-in sd() instead
# divides by (n - 1) -- the SAMPLE standard deviation, Bessel's correction,
# used when your data is a sample meant to estimate a larger population.
# stdev() below intentionally follows the assignment's formula (divide by n),
# so stdev(x) will NOT equal sd(x). This is expected, verified behaviour,
# not a bug -- see the test output below.

stdev <- function(data) {
  # Step 1: the mean (central point data is measured against)
  mean_value <- mean(data)

  # Step 2: how far each point is from that mean
  diff_values <- data - mean_value

  # Step 3: square the differences so negatives don't cancel positives out
  squared_diff <- diff_values^2

  # Step 4-5: population variance = average squared difference (divide by n)
  variance <- sum(squared_diff) / length(data)

  # Step 6: standard deviation is the square root of variance, back in the
  # original units of the data
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}

# -----------------------------------------------------------------------------
# TESTS
# -----------------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

  cat("=== stdev(data)  [population, divides by n] ===\n")
  print(stdev(data))
  # VERIFIED OUTPUT: [1] 4.946629

  cat("\n=== R's built-in sd(data)  [sample, divides by n-1] -- shown for comparison only ===\n")
  print(sd(data))
  # VERIFIED OUTPUT: [1] 5.246692   -- different from stdev(), by design (see note above)

  cat("\n=== Independent manual cross-check ===\n")
  m <- mean(data)
  cat("mean =", m, "\n")
  cat("manual sqrt(sum((x-mean)^2)/n) =", sqrt(sum((data - m)^2) / length(data)), "\n")
  # VERIFIED OUTPUT: mean = 18.55556 ; manual population sd = 4.946629
  # -- matches stdev(data) exactly

  cat("\n=== Edge case: all identical values -> standard deviation of 0 ===\n")
  print(stdev(c(7, 7, 7, 7)))
  # VERIFIED OUTPUT: [1] 0

  cat("\n=== Edge case: single value -> standard deviation of 0 ===\n")
  print(stdev(c(42)))
  # VERIFIED OUTPUT: [1] 0
}
