# Property checks for the tests in diebold_mariano.R, including agreement with
# forecast::dm.test on random data. Run from the project root.

suppressPackageStartupMessages(library(forecast))
source("R/diebold_mariano.R")

failures <- 0
check <- function(label, got, want, tol = 0) {
  ok <- is.finite(got) && abs(got - want) <= tol
  if (!ok) failures <<- failures + 1
  cat(sprintf("  %-46s got %-12s want %-12s %s\n", label,
              formatC(got, format = "f", digits = 6),
              formatC(want, format = "f", digits = 6),
              if (ok) "OK" else "MISMATCH"))
}
check_true <- function(label, ok) {
  if (!ok) failures <<- failures + 1
  cat(sprintf("  %-46s %s\n", label, if (ok) "OK" else "FAILED"))
}

set.seed(42)
n  <- 80
e1 <- rnorm(n, 0, 1.4)
e2 <- rnorm(n, 0, 1.0)

cat("=== hac_var at lag 0 is the population variance ===\n")
x <- rnorm(50)
check("hac_var(x, 0)", hac_var(x, 0), mean((x - mean(x))^2), 1e-12)

cat("\n=== agreement with forecast::dm.test ===\n")
for (hh in c(1, 2, 6, 12)) {
  mine <- dm_test(e1, e2, h = hh)
  pkg  <- dm.test(e1, e2, h = hh, power = 2)
  check(sprintf("h=%-2d statistic", hh), mine$statistic, as.numeric(pkg$statistic), 1e-9)
  check(sprintf("h=%-2d p-value",   hh), mine$p.value,   as.numeric(pkg$p.value),   1e-9)
}

cat("\n=== swapping the forecasts flips the sign ===\n")
a <- dm_test(e1, e2, h = 6)
b <- dm_test(e2, e1, h = 6)
check("statistic sign flips", a$statistic, -b$statistic, 1e-12)
check("p-value unchanged",    a$p.value,    b$p.value,    1e-12)

cat("\n=== a clearly worse forecast is detected ===\n")
bad  <- e2 * 4
res  <- dm_test(bad, e2, h = 1)
check_true("statistic positive (first forecast worse)", res$statistic > 0)
check_true("p-value below 0.01",                        res$p.value < 0.01)

cat("\n=== Clark-West rewards a genuinely better nested model ===\n")
set.seed(7)
actual  <- rnorm(n)
f_bench <- rep(0, n)
f_good  <- 0.6 * actual + rnorm(n, 0, 0.2)
cw <- cw_test(actual, f_bench, f_good, h = 1)
check_true("statistic positive", cw$statistic > 0)
check_true("p-value below 0.01", cw$p.value < 0.01)

cat("\n=== Clark-West does not reward pure noise ===\n")
f_noise <- rnorm(n)
cw2 <- cw_test(actual, f_bench, f_noise, h = 1)
check_true("p-value above 0.05", cw2$p.value > 0.05)

cat("\n=== HAC lag widens the standard error at longer horizons ===\n")
d  <- e1^2 - e2^2
v1 <- hac_var(d, 0)
v6 <- hac_var(d, 5)
check_true("lag 5 variance differs from lag 0", abs(v6 - v1) > 1e-10)

cat("\n=====================================\n")
if (failures == 0) {
  cat("SIGNIFICANCE TESTS VERIFIED: all checks passed\n")
} else {
  cat(sprintf("FAILED: %d check(s) did not match\n", failures))
  quit(status = 1)
}
