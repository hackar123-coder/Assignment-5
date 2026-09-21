# =============================================================================
# BDA400 Assignment 5: Technical Analysis using R (Development Phase)
# R/crossover.R -- Crossover signal detection
# =============================================================================
#
# AI ASSISTANCE DECLARATION:
# I used Claude (Sonnet 5, Anthropic) on 18 September 2026 to implement, test,
# and debug this function from the assignment's provided description,
# template, and pseudocode. Prompts used are listed in full in
# docs/Student_BDA400_A05_Appendix.docx. I verified outputs by tracing every
# index of the test arrays by hand (see comments below). All final
# calculations are done by myself. I am responsible for the accuracy and
# originality of this work.
# =============================================================================
#
# WHAT THIS DOES: flags the exact point where arr1 moves from at-or-below
# arr2 to strictly above it -- the classic "fast line crosses above slow
# line" buy-signal pattern. Returns "Up" at that point, "Down" if arr1
# crosses below arr2 instead (a small extension beyond the assignment's own
# formula, which only names the "Up" case explicitly), and "None" otherwise.

crossover <- function(arr1, arr2) {
  # Guard: a point-by-point comparison only makes sense on equal-length arrays
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  crossover_signals <- character(length(arr1))
  crossover_signals[1] <- "None"  # no previous point to compare the first entry to

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

# -----------------------------------------------------------------------------
# TESTS
# -----------------------------------------------------------------------------
if (sys.nframe() == 0) {
  arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

  cat("=== crossover(arr1, arr2) ===\n")
  print(crossover(arr1, arr2))
  # VERIFIED OUTPUT:
  # [1] "None" "None" "None" "Up"   "None" "None" "None" "None" "None"

  cat("\n=== Manual trace of every index ===\n")
  for (i in 2:length(arr1)) {
    cat("i=", i, " arr1=", arr1[i], " arr2=", arr2[i],
        " prev1=", arr1[i - 1], " prev2=", arr2[i - 1], "\n")
  }
  # VERIFIED OUTPUT confirms the only crossover is at i=4: arr1 goes from
  # 15 (<= 22) to 20 (> 18) -- a genuine "Up" signal; arr1 stays above arr2
  # for the rest of the series, so no "Down" ever appears in this dataset

  cat("\n=== Dataset engineered to also trigger a 'Down' signal ===\n")
  fast <- c(20, 21, 22, 23, 15, 14, 13, 20, 21)
  slow <- c(18, 18, 18, 18, 18, 18, 18, 18, 18)
  print(crossover(fast, slow))
  # VERIFIED OUTPUT: "Down" at index 5 (23 -> 15 crosses below 18),
  # "Up" at index 8 (13 -> 20 crosses back above 18)

  cat("\n=== Mismatched-length error check ===\n")
  print(tryCatch(crossover(c(1, 2, 3), c(1, 2)), error = function(e) e))
  # VERIFIED OUTPUT: <simpleError: Both arrays should have the same length>
}
