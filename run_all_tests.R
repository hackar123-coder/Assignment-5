# =============================================================================
# BDA400 Assignment 5: Technical Analysis using R (Development Phase)
# tests/run_all_tests.R -- loads every indicator and runs a combined check
# =============================================================================
#
# HOW TO RUN:
#   In R or RStudio, with the working directory set to the repo root, run:
#     source("tests/run_all_tests.R")
#   Or from a terminal:
#     Rscript tests/run_all_tests.R
#
# This script does NOT redefine any logic -- it sources the real
# implementations from R/ and re-runs each one against the assignment's own
# example data, so this is a genuine end-to-end check, not a duplicate.

.get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_flag <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_flag) > 0) return(dirname(normalizePath(sub("^--file=", "", file_flag[1]))))
  frames <- sys.frames()
  for (f in rev(frames)) if (!is.null(f$ofile)) return(dirname(normalizePath(f$ofile)))
  getwd()
}
repo_root <- dirname(.get_script_dir())
r_dir <- file.path(repo_root, "R")

cat("Loading all 9 indicator functions from", r_dir, "...\n\n")
source(file.path(r_dir, "sma.R"))
source(file.path(r_dir, "ema.R"))
source(file.path(r_dir, "macd.R"))
source(file.path(r_dir, "stdev.R"))
source(file.path(r_dir, "linreg.R"))
source(file.path(r_dir, "rsi.R"))
source(file.path(r_dir, "stoch_rsi.R"))
source(file.path(r_dir, "crossover.R"))
source(file.path(r_dir, "crossunder.R"))
cat("All functions loaded successfully: sma, ema, macd, stdev, linreg, rsi, stoch_rsi, crossover, crossunder\n")

results <- list()
check <- function(name, condition) {
  status <- if (isTRUE(condition)) "PASS" else "FAIL"
  results[[name]] <<- status
  cat(sprintf("[%s] %s\n", status, name))
}

cat("\n===================== RUNNING COMBINED CHECKS =====================\n\n")

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
rsi_data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

cat("-- sma --\n")
s <- sma(data, period = 3)
print(s)
check("sma: output length == length(data)-period+1", length(s) == length(data) - 3 + 1)
check("sma: first window mean matches manual calc", isTRUE(all.equal(s[1], mean(data[1:3]))))

cat("\n-- ema --\n")
e <- ema(data, period = 3)
print(e)
check("ema: output length == length(data)", length(e) == length(data))
check("ema: first value seeded with first price", e[1] == data[1])

cat("\n-- macd --\n")
m <- macd(c(100, 105, 110, 115, 120, 125, 130), 3, 5, 2)
print(m)
check("macd: histogram == macd_line - signal_line", isTRUE(all.equal(m$histogram, m$macd_line - m$signal_line)))

cat("\n-- stdev --\n")
sd_val <- stdev(data)
print(sd_val)
check("stdev: matches population formula (differs from sample sd())", isTRUE(all.equal(sd_val, sqrt(sum((data - mean(data))^2) / length(data)))))

cat("\n-- linreg --\n")
lr <- linreg(1:20, regressionLength = 5, regressionOffset = 0)
print(lr)
check("linreg: predicted_values length == regressionLength", length(lr$predicted_values) == 5)
fit <- lm(tail(1:20, 5) ~ c(1:5))
check("linreg: matches R's lm() on an identical window", isTRUE(all.equal(unname(coef(fit)[2]), lr$slope)))

cat("\n-- rsi --\n")
r <- rsi(rsi_data, period = 5)
print(r)
check("rsi: seed value RSI[period+1] == 60 (manually verified)", isTRUE(all.equal(r[6], 60)))
check("rsi: all non-NA values within [0,100]", all(r[!is.na(r)] >= 0 & r[!is.na(r)] <= 100))

cat("\n-- stoch_rsi --\n")
sr <- stoch_rsi(rsi_data, period = 5, k_period = 3, d_period = 3)
print(sr)
check("stoch_rsi: %K within [0,1]", all(sr$k_line >= 0 & sr$k_line <= 1))
check("stoch_rsi: %D within [0,1]", all(sr$d_line >= 0 & sr$d_line <= 1))

cat("\n-- crossover / crossunder --\n")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
co <- crossover(arr1, arr2)
cu <- crossunder(arr1, arr2)
print(co); print(cu)
check("crossover: flags 'Up' at index 4", co[4] == "Up")
fast <- c(20, 21, 22, 23, 15, 14, 13, 20, 21)
slow <- rep(18, 9)
check("crossunder: flags TRUE at index 5 on engineered data", crossunder(fast, slow)[5] == TRUE)

cat("\n===================== SUMMARY =====================\n")
n_pass <- sum(unlist(results) == "PASS")
n_total <- length(results)
cat(sprintf("%d / %d checks passed\n", n_pass, n_total))
if (n_pass == n_total) {
  cat("ALL CHECKS PASSED.\n")
} else {
  cat("SOME CHECKS FAILED -- see [FAIL] lines above.\n")
}
