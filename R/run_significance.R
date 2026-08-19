# Runs every model against the naive random walk benchmark at each horizon and
# writes the test results. Run from the project root after R/backtest.R.

suppressPackageStartupMessages(library(forecast))
source("R/diebold_mariano.R")

BENCH    <- "Naive RW"
HORIZONS <- c(1, 6, 12)

args   <- commandArgs(trailingOnly = TRUE)
START  <- if (length(args) >= 1) as.integer(args[1]) else 60
SUFFIX <- if (START == 60) "" else paste0("_start", START)

errors <- read.csv(sprintf("results/backtest_errors%s.csv", SUFFIX),
                   stringsAsFactors = FALSE)
models <- setdiff(unique(errors$model), BENCH)

out <- list()
for (hh in HORIZONS) {
  sub <- errors[errors$h == hh, ]
  b   <- sub[sub$model == BENCH, ]
  b   <- b[order(b$origin), ]

  for (nm in models) {
    m <- sub[sub$model == nm, ]
    m <- m[order(m$origin), ]
    stopifnot(identical(b$origin, m$origin))

    dm  <- dm_test(b$error, m$error, h = hh)
    ref <- tryCatch(dm.test(b$error, m$error, h = hh, power = 2),
                    error = function(e) list(statistic = NA, p.value = NA))
    cw  <- cw_test(b$actual, b$forecast, m$forecast, h = hh)

    out[[length(out) + 1]] <- data.frame(
      h              = hh,
      model          = nm,
      n_origins      = dm$n,
      rmse_bench     = sqrt(mean(b$error^2)),
      rmse_model     = sqrt(mean(m$error^2)),
      dm_stat        = dm$statistic,
      dm_p           = dm$p.value,
      dm_stat_pkg    = as.numeric(ref$statistic),
      dm_p_pkg       = as.numeric(ref$p.value),
      cw_stat        = cw$statistic,
      cw_p           = cw$p.value,
      stringsAsFactors = FALSE)
  }
}

res <- do.call(rbind, out)
res$dm_matches_pkg <- abs(res$dm_stat - res$dm_stat_pkg) < 1e-8
res$start_origin   <- START

dir.create("results", showWarnings = FALSE)
write.csv(res, sprintf("results/dm_tests%s.csv", SUFFIX), row.names = FALSE)

cat("Benchmark:", BENCH, "\n")
cat("DM sign convention: positive means the benchmark has the LARGER loss,\n")
cat("i.e. the model is more accurate. Two-sided p-value.\n")
cat("Clark-West is one-sided: small p means the model beats the benchmark.\n")
cat("Cross-check vs forecast::dm.test all match:",
    all(res$dm_matches_pkg), "\n")

for (hh in HORIZONS) {
  cat(sprintf("\n===== horizon h = %d =====\n", hh))
  s <- res[res$h == hh, ]
  s <- s[order(s$rmse_model), ]
  tab <- data.frame(
    model   = s$model,
    RMSE    = round(s$rmse_model, 3),
    vs_naive = round(s$rmse_model - s$rmse_bench, 3),
    DM      = round(s$dm_stat, 3),
    DM_p    = round(s$dm_p, 4),
    CW_p    = round(s$cw_p, 4),
    verdict = ifelse(s$dm_p < 0.05,
                     ifelse(s$rmse_model < s$rmse_bench, "model better", "NAIVE better"),
                     "indistinguishable"),
    stringsAsFactors = FALSE)
  print(tab, row.names = FALSE)
}

cat(sprintf("\nnaive RMSE by horizon: %s\n",
            paste(sprintf("h=%d: %.3f", HORIZONS,
                          sapply(HORIZONS, function(hh)
                            sqrt(mean(errors$error[errors$h == hh &
                                                   errors$model == BENCH]^2)))),
                  collapse = "   ")))
cat(sprintf("\nWritten: results/dm_tests%s.csv\n", SUFFIX))
