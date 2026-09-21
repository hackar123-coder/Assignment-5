# BDA400 Assignment 5: Technical Analysis using R (Development Phase) crossover.R, Crossover signal detection
#
# AI HELP DECLARATION:
# I used Claude (Anthropic, Claude Sonnet 5) in September 2026 to use and review this function from the assignment's description, template and pseudocode. I am responsible for the accuracy and originality of this work.
#
# WHAT THIS DOES: flags the exact point where arr1 moves from at-or-below arr2 to strictly above it, the classic "fast line crosses above slow line" buy-signal pattern. Returns "Up" at that point, perhaps "Down" if arr1 crosses below arr2 instead (a small extension beyond the assignment's own formula, which only names the "Up" case explicitly), and "None" otherwise.

crossover <- function(arr1, arr2) {
  # Guard: a point-by-point comparison only makes sense on equal-length arrays
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  crossover_signals <- character(length(arr1))
  crossover_signals[1] <- "None"  # nope previous point to look at side by side the first entry to

  for (i in 2:length(arr1)) {
    if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
      crossover_signals[i] <- "Up"     # arr1 just moved from <= to strictly above arr2
    } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossover_signals[i] <- "Down"   # arr1 just moved from >= to strictly below arr2
    } else {
      crossover_signals[i] <- "None"
    }
  }

  return(crossover_signals)
}

# TESTS
if (sys.nframe() == 0) {
  arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

  cat("=== crossover(arr1, arr2) ===\n")
  print(crossover(arr1, arr2))
  # CHECKED OUTPUT:
  # [1] "None" "None" "None" "Up" "None" "None" "None" "None" "None"

  cat("\n=== Manual trace of every index ===\n")
  for (i in 2:length(arr1)) {
    cat("i=", i, " arr1=", arr1[i], " arr2=", arr2[i],
        " prev1=", arr1[i - 1], " prev2=", arr2[i - 1], "\n")
  }
  # CHECKED OUTPUT confirms the only crossover is at i=4: arr1 goes from 15 (<= 22) to 20 (> 18), a genuine "Up" signal; arr1 stays above arr2 for the rest of the series, so no "Down" ever appears in that dataset

  cat("\n=== Dataset engineered to also trigger a 'Down' signal ===\n")
  fast <- c(20, 21, 22, 23, 15, 14, 13, 20, 21)
  slow <- c(18, 18, 18, 18, 18, 18, 18, 18, 18)
  print(crossover(fast, slow))
  # CHECKED OUTPUT: "Down" at index 5 (23 -> 15 crosses below 18),
  # "Up" at index 8 (13 -> 20 crosses back above 18)

  cat("\n=== Mismatched-length error check ===\n")
  print(tryCatch(crossover(c(1, 2, 3), c(1, 2)), error = function(e) e))
  # CHECKED OUTPUT: <simpleError: Both arrays should have the same length>
}