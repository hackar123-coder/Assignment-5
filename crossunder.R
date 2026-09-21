# Later on, bDA400 Assignment 5: Technical Analysis using R (Development Phase) crossunder.R, Crossunder signal detection
#
# AI HELP DECLARATION:
# I used Claude (Anthropic, Claude Sonnet 5) in September 2026 to use and review this function from the assignment's description, template and pseudocode. I am responsible for the accuracy and originality of this work.
#
# WHAT THIS DOES: the mirror image of crossover(), flags the exact point where arr1 moves from at-or-above arr2 to strictly below it (a classic
# "fast line crosses below slow line" sell-signal pattern).

crossunder <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  crossunder_signals <- logical(length(arr1))
  crossunder_signals[1] <- FALSE  # nope previous point to look at side by side the first entry to

  for (i in 2:length(arr1)) {
    if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossunder_signals[i] <- TRUE  # arr1 just moved from >= to strictly below arr2
    } else {
      crossunder_signals[i] <- FALSE
    }
  }

  return(crossunder_signals)
}

# TESTS
if (sys.nframe() == 0) {
  arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

  cat("=== crossunder(arr1, arr2) -- assignment's own example data ===\n")
  print(crossunder(arr1, arr2))
  # VERIFIED OUTPUT: all FALSE
  # This is a genuine, correct null result, not a bug: arr1 crosses ABOVE arr2 at index 4 (see crossover.R) and then stays above it for the rest of the series, so there is no point where it crosses back under. A dataset that never crosses under should produce all-FALSE.

  cat("\n=== Dataset engineered to contain a genuine crossunder (proves the TRUE branch works) ===\n")
  fast <- c(20, 21, 22, 23, 15, 14, 13, 20, 21)
  slow <- c(18, 18, 18, 18, 18, 18, 18, 18, 18)
  print(crossunder(fast, slow))
  # CHECKED OUTPUT:
  # [1] FALSE FALSE FALSE FALSE TRUE FALSE FALSE FALSE FALSE
  # TRUE exactly at index 5, where fast drops from 23 (>= 18) to 15 (< 18)

  cat("\n=== Mismatched-length error check ===\n")
  print(tryCatch(crossunder(c(1, 2, 3), c(1, 2)), error = function(e) e))
  # CHECKED OUTPUT: <simpleError: Both arrays should have the same length>
}