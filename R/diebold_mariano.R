# Tests whether one set of forecasts is really more accurate than another, or
# whether the gap is just noise. Diebold-Mariano, plus Clark-West for nested models.

hac_var <- function(x, lag) {
  n  <- length(x)
  xc <- x - mean(x)
  v  <- sum(xc^2) / n
  if (lag >= 1) {
    for (k in 1:lag) {
      v <- v + 2 * sum(xc[1:(n - k)] * xc[(k + 1):n]) / n
    }
  }
  v
}

dm_test <- function(e1, e2, h, power = 2) {
  d <- abs(e1)^power - abs(e2)^power
  n <- length(d)
  v <- hac_var(d, h - 1) / n
  stat <- mean(d) / sqrt(v)
  hln  <- sqrt((n + 1 - 2 * h + h * (h - 1) / n) / n)
  stat <- stat * hln
  list(statistic = stat,
       p.value   = 2 * pt(-abs(stat), df = n - 1),
       mean_diff = mean(d),
       n         = n)
}

cw_test <- function(actual, f_bench, f_model, h) {
  e_b <- actual - f_bench
  e_m <- actual - f_model
  f   <- e_b^2 - e_m^2 + (f_bench - f_model)^2
  n    <- length(f)
  stat <- mean(f) / sqrt(hac_var(f, h - 1) / n)
  list(statistic = stat,
       p.value   = 1 - pt(stat, df = n - 1),
       mean_adj  = mean(f),
       n         = n)
}
