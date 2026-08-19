# Checks the baselines in baselines.R behave like the other models and score what
# they should on the notebook hold-out set. Run from the project root.

source("R/data.R")
source("R/models.R")
source("R/baselines.R")

d  <- load_brent()
sp <- submission_split(d)
train <- sp$train; test <- sp$test; h <- sp$h
last_log <- sp$last_log_train
actual   <- exp(last_log + cumsum(test))

failures <- 0
check <- function(label, got, want, tol) {
  ok <- is.finite(got) && abs(got - want) <= tol
  if (!ok) failures <<- failures + 1
  cat(sprintf("  %-34s got %-10s want %-10s %s\n", label,
              formatC(got, format = "f", digits = 3),
              formatC(want, format = "f", digits = 3),
              if (ok) "OK" else "MISMATCH"))
}

rmse <- function(a, p) sqrt(mean((a - p)^2))
mae  <- function(a, p) mean(abs(a - p))
mape <- function(a, p) mean(abs((a - p) / a)) * 100

naive <- fit_naive_rw(train)
drift <- fit_rw_drift(train)

cat("=== contract: same fields as the models.R fits ===\n")
required <- c("mu", "sigma2", "residuals", "loglik", "k", "converged")
for (nm in c("naive_rw", "rw_drift")) {
  m       <- if (nm == "naive_rw") naive else drift
  missing <- setdiff(required, names(m))
  ok      <- length(missing) == 0
  if (!ok) failures <- failures + 1
  cat(sprintf("  %-34s %s\n", nm,
              if (ok) "OK" else paste("MISSING:", paste(missing, collapse = ", "))))
}

cat("\n=== drift equals the mean log-return ===\n")
check("rw_drift mu", drift$mu, mean(train), 1e-12)
check("naive_rw mu is zero", naive$mu, 0, 0)

cat("\n=== forecast shape at every horizon ===\n")
for (nm in c("naive_rw", "rw_drift")) {
  f <- if (nm == "naive_rw") naive_rw_forecast else rw_drift_forecast
  m <- if (nm == "naive_rw") naive else drift
  for (hh in c(1, 2, 3, 12)) {
    r  <- tryCatch(f(m, hh), error = function(e) e)
    ok <- !inherits(r, "error") &&
          length(r$mean) == hh && length(r$sigma2_l) == hh &&
          all(is.finite(r$mean)) && all(r$sigma2_l > 0)
    if (!ok) failures <- failures + 1
    cat(sprintf("  %-12s h=%-3d %s\n", nm, hh, if (ok) "OK" else "BAD"))
  }
}

cat("\n=== variance grows linearly with horizon ===\n")
v <- naive_rw_forecast(naive, 3)$sigma2_l
check("sigma2_l[2] / sigma2_l[1]", v[2] / v[1], 2, 1e-12)
check("sigma2_l[3] / sigma2_l[1]", v[3] / v[1], 3, 1e-12)

cat("\n=== accuracy on the notebook 14-month hold-out ===\n")
p_naive <- forecast_to_price(naive_rw_forecast(naive, h), last_log, bias_correct = FALSE)
check("naive no-change RMSE", rmse(actual, p_naive), 11.38, 0.01)
check("naive no-change MAE",  mae(actual, p_naive),  10.78, 0.01)
check("naive no-change MAPE", mape(actual, p_naive), 16.88, 0.01)

p_drift <- forecast_to_price(rw_drift_forecast(drift, h), last_log, bias_correct = FALSE)
check("rw+drift RMSE", rmse(actual, p_drift), 12.93, 0.01)
check("rw+drift MAPE", mape(actual, p_drift), 19.53, 0.01)

cat("\n=== no-change price really is flat at the last price ===\n")
check("all forecasts equal last price", max(abs(p_naive - exp(last_log))), 0, 1e-10)

cat("\n=== both bias-correction conventions ===\n")
p_bc <- forecast_to_price(naive_rw_forecast(naive, h), last_log, bias_correct = TRUE)
cat(sprintf("  no-change      RMSE %6.2f  MAE %6.2f  MAPE %5.2f\n",
            rmse(actual, p_naive), mae(actual, p_naive), mape(actual, p_naive)))
cat(sprintf("  bias-corrected RMSE %6.2f  MAE %6.2f  MAPE %5.2f\n",
            rmse(actual, p_bc), mae(actual, p_bc), mape(actual, p_bc)))

cat("\n=====================================\n")
if (failures == 0) {
  cat("BASELINES VERIFIED: all checks passed\n")
} else {
  cat(sprintf("FAILED: %d check(s) did not match\n", failures))
  quit(status = 1)
}
