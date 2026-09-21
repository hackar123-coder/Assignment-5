# BDA400 Assignment 5 — Technical Analysis using R (Development Phase)

Nine technical-analysis indicator functions implemented in base R (no external
packages), following the assignment's provided descriptions, templates, and
pseudocode.

## Repository layout

```
.
├── R/                    9 indicator functions, one file each
│   ├── sma.R              Simple Moving Average
│   ├── ema.R               Exponential Moving Average
│   ├── macd.R               MACD (depends on ema.R)
│   ├── stdev.R               Standard Deviation
│   ├── linreg.R                Trailing-window Linear Regression
│   ├── rsi.R                    Relative Strength Index (Wilder's smoothing)
│   ├── stoch_rsi.R               Stochastic RSI (depends on rsi.R, sma.R)
│   ├── crossover.R                Crossover signal detection
│   └── crossunder.R                Crossunder signal detection
├── tests/
│   └── run_all_tests.R    loads every function and runs a combined check
└── docs/
    ├── Student_BDA400_A05.docx            cover page + explanation of each implementation
    └── Student_BDA400_A05_Appendix.docx   AI prompts and key responses (per AI Usage Rule 7)
```

## How to run

**Option A — run everything at once (recommended):**
In RStudio, open this folder as your working directory, then run:
```r
source("tests/run_all_tests.R")
```
This loads all 9 functions from `R/` and runs 14 checks against them,
printing `[PASS]`/`[FAIL]` for each and a final summary line. It works from
any starting working directory (repo root, `R/`, or `tests/`).

**Option B — run one indicator on its own:**
Each file in `R/` is self-contained and includes its own test block at the
bottom (guarded so the tests only run when the file itself is executed
directly, not when another file `source()`s it as a dependency). For example:
```r
setwd("R")          # or use the Files pane / "Set As Working Directory"
source("sma.R")      # defines sma() AND runs its own tests + prints output
```
or from a terminal:
```
Rscript R/sma.R
```
`macd.R` and `stoch_rsi.R` automatically `source()` their own dependencies
(`ema.R`, and `rsi.R`/`sma.R` respectively) if those functions aren't already
loaded, so they also work standalone.

## Function signatures (exact names/arguments, matching the provided templates)

| File | Function | Arguments |
|---|---|---|
| `sma.R` | `sma` | `data, period` |
| `ema.R` | `ema` | `data, period` |
| `macd.R` | `macd` | `data, short_period, long_period, signal_period` |
| `stdev.R` | `stdev` | `data` |
| `linreg.R` | `linreg` | `regressionSource, regressionLength, regressionOffset` |
| `rsi.R` | `rsi` | `data, period` |
| `stoch_rsi.R` | `stoch_rsi` | `data, period, k_period, d_period` |
| `crossover.R` | `crossover` | `arr1, arr2` |
| `crossunder.R` | `crossunder` | `arr1, arr2` |

## Two bugs found and fixed during testing

Testing against the assignment's own pseudocode surfaced two genuine logic
issues, which were corrected and independently re-verified (see
`docs/Student_BDA400_A05_Appendix.docx` for full details and the exact
numbers):

1. **`linreg`** — the pseudocode's `start_index`/`end_index` formula does not
   actually produce a window of length `regressionLength`; the window shrinks
   as `regressionOffset` grows. Fixed so the window is always exactly
   `regressionLength` points ending `regressionOffset` points back from the
   end of the data. Verified against R's own `lm()` on an identical window
   (exact match).
2. **`rsi`** — the pseudocode's smoothing loop starts one index too early,
   re-using a gain/loss value that was already folded into the seed average.
   Fixed so the seed average is used directly as the first RSI value, and the
   recursive smoothing only applies to genuinely new data from then on.
   Verified by hand: literal pseudocode gives RSI = 50.53 at the first
   computed point; the corrected version and a manual calculation both give
   60.00.

## AI assistance

See the **AI Assistance Declaration** at the top of every `.R` file and the
full prompt history in `docs/Student_BDA400_A05_Appendix.docx`. All function
logic was independently tested and verified in R before being accepted —
see the `VERIFIED OUTPUT` comments throughout `R/` and the cross-checks in
`tests/run_all_tests.R`.

