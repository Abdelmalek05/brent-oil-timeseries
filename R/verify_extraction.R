# Checks that the models in models.R give the same numbers as the graded notebook.
# Run from the project root; it exits with an error code if anything does not match.

source("R/data.R")
source("R/models.R")

d  <- load_brent()
sp <- submission_split(d)
train <- sp$train

failures <- 0
check <- function(label, got, want, tol) {
  ok <- is.finite(got) && abs(got - want) <= tol
  if (!ok) failures <<- failures + 1
  cat(sprintf("  %-28s got %-14s want %-14s %s\n",
              label, formatC(got, format = "f", digits = 6),
              formatC(want, format = "f", digits = 6),
              if (ok) "OK" else "MISMATCH"))
}

cat("=== data and split ===\n")
check("observations",     d$n,              135,   0)
check("log-returns",      length(d$diff_y), 134,   0)
check("training obs",     sp$split,         120,   0)
check("test obs",         sp$h,             14,    0)
check("last known price", exp(sp$last_log_train), 73.13, 0.005)

cat("\n=== AR(1), Yule-Walker ===\n")
ar1 <- yule_walker_ar1(train)
check("mu",     ar1$mu,     0.003787, 1e-5)
check("phi",    ar1$phi,    0.332852, 1e-5)
check("sigma2", ar1$sigma2, 0.007856, 1e-5)
ic <- aic_bic(ar1$loglik, ar1$k, length(train))
check("AIC", ic[["AIC"]], -237.036, 1e-3)
check("BIC", ic[["BIC"]], -228.674, 1e-3)

cat("\n=== MA(1), conditional sum of squares ===\n")
ma1 <- fit_ma1(train)
check("mu",     ma1$mu,      0.004905, 1e-5)
check("theta1", ma1$theta1, -0.389192, 1e-5)
check("sigma2", ma1$sigma2,  0.007824, 1e-5)
ic <- aic_bic(ma1$loglik, ma1$k, length(train))
check("AIC", ic[["AIC"]], -237.5263, 1e-3)
check("BIC", ic[["BIC"]], -229.1639, 1e-3)

cat("\n=== MA(2) ===\n")
ma2 <- fit_ma2(train)
check("mu",     ma2$mu,      0.005164, 1e-5)
check("theta1", ma2$theta1, -0.420052, 1e-5)
check("theta2", ma2$theta2, -0.055251, 1e-5)
check("sigma2", ma2$sigma2,  0.007877, 1e-5)
ic <- aic_bic(ma2$loglik, ma2$k, length(train))
check("AIC", ic[["AIC"]], -235.7176, 1e-3)
check("BIC", ic[["BIC"]], -224.5676, 1e-3)

cat("\n=== ARMA(1,1) ===\n")
arma11 <- fit_arma11(train)
check("mu",     arma11$mu,      0.005011, 1e-5)
check("phi",    arma11$phi,     0.054897, 1e-5)
check("theta",  arma11$theta,  -0.347024, 1e-5)
check("sigma2", arma11$sigma2,  0.007885, 1e-5)
ic <- aic_bic(arma11$loglik, arma11$k, length(train))
check("AIC", ic[["AIC"]], -235.5882, 1e-3)
check("BIC", ic[["BIC"]], -224.4383, 1e-3)

cat("\n=== model ranking, the notebook found MA(1) best on both ===\n")
ics <- rbind(
  aic_bic(ar1$loglik,    ar1$k,    length(train)),
  aic_bic(ma1$loglik,    ma1$k,    length(train)),
  aic_bic(ma2$loglik,    ma2$k,    length(train)),
  aic_bic(arma11$loglik, arma11$k, length(train))
)
rownames(ics) <- c("AR(1)", "MA(1)", "MA(2)", "ARMA(1,1)")
print(round(ics, 4))
best_aic <- rownames(ics)[which.min(ics[, "AIC"])]
best_bic <- rownames(ics)[which.min(ics[, "BIC"])]
cat(sprintf("  best by AIC: %-10s %s\n", best_aic,
            if (best_aic == "MA(1)") "OK" else "MISMATCH"))
cat(sprintf("  best by BIC: %-10s %s\n", best_bic,
            if (best_bic == "MA(1)") "OK" else "MISMATCH"))
if (best_aic != "MA(1)") failures <- failures + 1
if (best_bic != "MA(1)") failures <- failures + 1

cat("\n=== forecasts at short horizons ===\n")
last_y <- tail(train, 1)
fns <- list(
  "AR(1)"     = function(h) ar1_forecast(ar1, h, last_y),
  "MA(1)"     = function(h) ma1_forecast(ma1, h),
  "MA(2)"     = function(h) ma2_forecast(ma2, h),
  "ARMA(1,1)" = function(h) arma11_forecast(arma11, h, last_y)
)
for (nm in names(fns)) {
  for (h in c(1, 2, 3, 12)) {
    r  <- tryCatch(fns[[nm]](h), error = function(e) e)
    ok <- !inherits(r, "error") &&
          length(r$mean) == h && length(r$sigma2_l) == h &&
          all(is.finite(r$mean)) && all(is.finite(r$sigma2_l)) &&
          all(r$sigma2_l > 0)
    if (!ok) failures <- failures + 1
    cat(sprintf("  %-10s h=%-3d %s\n", nm, h,
                if (ok) "OK" else
                if (inherits(r, "error")) paste("ERROR:", conditionMessage(r))
                else "BAD LENGTH OR NON-FINITE"))
  }
}

cat("\n=====================================\n")
if (failures == 0) {
  cat("EXTRACTION VERIFIED: all checks passed\n")
} else {
  cat(sprintf("FAILED: %d check(s) did not match\n", failures))
  quit(status = 1)
}
