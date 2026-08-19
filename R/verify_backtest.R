# Checks the backtest output: origin counts, no missing forecasts, and that the
# forecasts and actuals are lined up against the right months.

source("R/data.R")

d <- load_brent()
y <- d$y
N <- length(d$diff_y)

args   <- commandArgs(trailingOnly = TRUE)
START  <- if (length(args) >= 1) as.integer(args[1]) else 60
SUFFIX <- if (START == 60) "" else paste0("_start", START)
cat(sprintf("Checking backtest with start origin %d\n\n", START))

errors <- read.csv(sprintf("results/backtest_errors%s.csv", SUFFIX),
                   stringsAsFactors = FALSE)

failures <- 0
check <- function(label, got, want, tol = 0) {
  ok <- is.finite(got) && abs(got - want) <= tol
  if (!ok) failures <<- failures + 1
  cat(sprintf("  %-42s got %-12s want %-12s %s\n", label,
              formatC(got, format = "f", digits = 4),
              formatC(want, format = "f", digits = 4),
              if (ok) "OK" else "MISMATCH"))
}

n_models <- length(unique(errors$model))

cat("=== coverage ===\n")
check("models", n_models, 8)
for (hh in c(1, 6, 12)) {
  n <- length(unique(errors$origin[errors$h == hh]))
  check(sprintf("origins at h = %d", hh), n, N - hh - START + 1)
}
check("missing forecasts", sum(is.na(errors$forecast)), 0)
check("missing actuals",   sum(is.na(errors$actual)),   0)

cat("\n=== actuals point at the right month ===\n")
cat("  a forecast made at origin t for step h should be scored against y[t+1+h]\n")
mism <- 0
for (i in seq_len(nrow(errors))) {
  if (abs(errors$actual[i] - y[errors$origin[i] + 1 + errors$h[i]]) > 1e-9) mism <- mism + 1
}
check("rows with misaligned actual", mism, 0)

cat("\n=== naive forecast really is the last known price ===\n")
cat("  after t returns the last known price is y[t+1], so every naive forecast\n")
cat("  at origin t must equal y[t+1] whatever the horizon\n")
nv   <- errors[errors$model == "Naive RW", ]
diff <- max(abs(nv$forecast - y[nv$origin + 1]))
check("largest deviation from y[t+1]", diff, 0, 1e-9)

cat("\n=== spot check, origin 100 computed by hand ===\n")
t <- 100
cat(sprintf("  last known price y[%d] = %.4f\n", t + 1, y[t + 1]))
for (hh in c(1, 6, 12)) {
  row <- nv[nv$origin == t & nv$h == hh, ]
  check(sprintf("h=%d forecast", hh), row$forecast, y[t + 1], 1e-9)
  check(sprintf("h=%d actual  ", hh), row$actual,   y[t + 1 + hh], 1e-9)
  check(sprintf("h=%d error   ", hh), row$error,    y[t + 1 + hh] - y[t + 1], 1e-9)
}

cat("\n=== no forecast uses data past its origin ===\n")
cat("  the last origin must leave at least one real observation to score\n")
check("max origin", max(errors$origin), N - 1)
check("max origin + max horizon <= n", max(errors$origin + 1 + errors$h), length(y))

cat("\n=====================================\n")
if (failures == 0) {
  cat("BACKTEST VERIFIED: all checks passed\n")
} else {
  cat(sprintf("FAILED: %d check(s) did not match\n", failures))
  quit(status = 1)
}
