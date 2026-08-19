# Rolling-origin backtest: refits every model at each origin and forecasts ahead,
# so the models are judged on many forecasts instead of one 14-month split.

suppressPackageStartupMessages(library(forecast))

source("R/data.R")
source("R/models.R")
source("R/baselines.R")

START_ORIGIN <- 60
HORIZONS     <- c(1, 6, 12)
HMAX         <- max(HORIZONS)

return_model <- function(fit_fn, fc_fn, needs_last_y, bias_correct) {
  function(log_prices, returns, h) {
    m  <- fit_fn(returns)
    f  <- if (needs_last_y) fc_fn(m, h, tail(returns, 1)) else fc_fn(m, h)
    list(price = forecast_to_price(f, tail(log_prices, 1), bias_correct),
         ok    = isTRUE(m$converged))
  }
}

auto_arima_model <- function(log_prices, returns, h) {
  fit <- auto.arima(ts(log_prices, start = c(2015, 1), frequency = 12),
                    max.p = 3, max.q = 3, max.d = 2,
                    ic = "aic", stepwise = FALSE, trace = FALSE)
  list(price = exp(as.numeric(forecast(fit, h = h)$mean)), ok = TRUE)
}

MODELS <- list(
  "Naive RW"          = return_model(fit_naive_rw,    naive_rw_forecast, FALSE, FALSE),
  "Naive RW (bias-c)" = return_model(fit_naive_rw,    naive_rw_forecast, FALSE, TRUE),
  "RW + drift"        = return_model(fit_rw_drift,    rw_drift_forecast, FALSE, FALSE),
  "AR(1)"             = return_model(yule_walker_ar1, ar1_forecast,      TRUE,  TRUE),
  "MA(1)"             = return_model(fit_ma1,         ma1_forecast,      FALSE, TRUE),
  "MA(2)"             = return_model(fit_ma2,         ma2_forecast,      FALSE, TRUE),
  "ARMA(1,1)"         = return_model(fit_arma11,      arma11_forecast,   TRUE,  TRUE),
  "Auto-ARIMA"        = auto_arima_model
)

d       <- load_brent()
returns <- d$diff_y
log_y   <- d$log_y
y       <- d$y
N       <- length(returns)

origins <- START_ORIGIN:(N - 1)
cat(sprintf("Origins %d to %d (%d refits per model)\n", min(origins), max(origins), length(origins)))
cat(sprintf("Models: %d   horizons: %s\n\n", length(MODELS), paste(HORIZONS, collapse = ", ")))

rows     <- list()
failures <- list()
t0       <- Sys.time()

for (t in origins) {
  r_tr   <- returns[1:t]
  lp_tr  <- log_y[1:(t + 1)]
  h_t    <- min(HMAX, N - t)
  actual <- y[(t + 2):(t + 1 + h_t)]

  for (nm in names(MODELS)) {
    out <- tryCatch(MODELS[[nm]](lp_tr, r_tr, h_t),
                    error = function(e) list(price = rep(NA_real_, h_t),
                                             ok = FALSE, msg = conditionMessage(e)))
    if (!isTRUE(out$ok)) {
      failures[[length(failures) + 1]] <- data.frame(
        origin = t, model = nm,
        reason = if (is.null(out$msg)) "did not converge" else out$msg,
        stringsAsFactors = FALSE)
    }
    rows[[length(rows) + 1]] <- data.frame(
      origin = t, model = nm, h = seq_len(h_t),
      forecast = out$price, actual = actual, stringsAsFactors = FALSE)
  }

  if (t %% 10 == 0) cat(sprintf("  origin %d / %d\n", t, max(origins)))
}

elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("\nDone in %.1f seconds\n", elapsed))

errors <- do.call(rbind, rows)
errors$error <- errors$actual - errors$forecast
errors <- errors[errors$h %in% HORIZONS, ]

fail_df <- if (length(failures)) do.call(rbind, failures) else
           data.frame(origin = integer(0), model = character(0), reason = character(0))

cat(sprintf("\nFit failures: %d\n", nrow(fail_df)))
if (nrow(fail_df)) print(table(fail_df$model))

cat(sprintf("Missing forecasts (NA): %d of %d rows\n",
            sum(is.na(errors$forecast)), nrow(errors)))

metrics <- do.call(rbind, lapply(HORIZONS, function(hh) {
  sub <- errors[errors$h == hh, ]
  do.call(rbind, lapply(names(MODELS), function(nm) {
    s <- sub[sub$model == nm & !is.na(sub$forecast), ]
    data.frame(h = hh, model = nm, n_origins = nrow(s),
               RMSE = sqrt(mean(s$error^2)),
               MAE  = mean(abs(s$error)),
               MAPE = mean(abs(s$error / s$actual)) * 100,
               stringsAsFactors = FALSE)
  }))
}))

dir.create("results", showWarnings = FALSE)
write.csv(errors,  "results/backtest_errors.csv", row.names = FALSE)
write.csv(metrics, "results/metrics.csv",         row.names = FALSE)
write.csv(fail_df, "results/fit_failures.csv",    row.names = FALSE)

for (hh in HORIZONS) {
  cat(sprintf("\n===== horizon h = %d months =====\n", hh))
  s <- metrics[metrics$h == hh, ]
  s <- s[order(s$RMSE), c("model", "n_origins", "RMSE", "MAE", "MAPE")]
  print(format(s, digits = 4), row.names = FALSE)
}

cat("\nWritten: results/backtest_errors.csv, results/metrics.csv, results/fit_failures.csv\n")
