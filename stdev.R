# BDA400 Assignment 5: Technical Analysis using R (Development Phase) stdev.R, Standard Change from normal
#
# AI HELP DECLARATION:
# I used Claude (Anthropic, Claude Sonnet 5) in September 2026 to use and review this function from the assignment's description, template and pseudocode. I am responsible for the accuracy and originality of this work.
#
# WHAT THIS DOES: measures how spread out the data is around its own mean.
#
# IMPORTANT, POPULATION vs SAMPLE STANDARD DEVIATION:
# The assignment's formula divides the sum of squared differences by n (the number of data points): sigma = sqrt( sum((x_i - mean)^2) / n ).
# This is the POPULATION standard departure. R's built-in sd() instead divides by (n - 1), the SAMPLE standard change from normal, Bessel's correction, used when your data is a sample meant to estimate a larger population.
# stdev() below intentionally follows the assignment's formula (divide by n), so stdev(x) will NOT equal sd(x). This is expected, checked behaviour, not a bug, see the test output below.

stdev <- function(data) {
  # Step 1: the mean (central point data is measured against)
  mean_value <- mean(data)

  # Step 2: how far each point is from that mean
  diff_values <- data - mean_value

  # Step 3: square the differences so negatives don't cancel positives out
  squared_diff <- diff_values^2

  # Step 4-5: population variance = average squared difference (divide by n)
  variance <- sum(squared_diff) / length(data)

  # Step 6: standard change from normal is the square root of variance, back in the
  # original units of the data
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}

# TESTS
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

  cat("=== stdev(data)  [population, divides by n] ===\n")
  print(stdev(data))
  # CHECKED OUTPUT: [1] 4.946629

  cat("\n=== R's built-in sd(data)  [sample, divides by n-1] -- shown for comparison only ===\n")
  print(sd(data))
  # CHECKED OUTPUT: [1] 5.246692, different from stdev(), by design (see note above)

  cat("\n=== Independent manual cross-check ===\n")
  m <- mean(data)
  cat("mean =", m, "\n")
  cat("manual sqrt(sum((x-mean)^2)/n) =", sqrt(sum((data - m)^2) / length(data)), "\n")
  # CHECKED OUTPUT: mean = 18.55556 ; manual population sd = 4.946629
  # -- matches stdev(data) exactly

  cat("\n=== Edge case: all identical values -> standard deviation of 0 ===\n")
  print(stdev(c(7, 7, 7, 7)))
  # CHECKED OUTPUT: [1] 0

  cat("\n=== Edge case: single value -> standard deviation of 0 ===\n")
  print(stdev(c(42)))
  # CHECKED OUTPUT: [1] 0
}