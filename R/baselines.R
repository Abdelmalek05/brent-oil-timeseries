# The two benchmarks every forecast has to beat: a random walk (next price equals
# the last price) and a random walk with drift. Same shape as the models.R fits.

fit_naive_rw <- function(x) {
  n      <- length(x)
  res    <- x
  sigma2 <- sum(res^2) / n
  loglik <- -0.5 * n * log(2 * pi * sigma2) - sum(res^2) / (2 * sigma2)
  list(mu = 0, sigma2 = sigma2, residuals = res,
       loglik = loglik, k = 1, converged = TRUE)
}

naive_rw_forecast <- function(model, h, last_y = NULL) {
  list(mean = rep(0, h), sigma2_l = model$sigma2 * seq_len(h))
}

fit_rw_drift <- function(x) {
  n      <- length(x)
  mu     <- mean(x)
  res    <- x - mu
  sigma2 <- sum(res^2) / (n - 1)
  loglik <- -0.5 * n * log(2 * pi * sigma2) - sum(res^2) / (2 * sigma2)
  list(mu = mu, sigma2 = sigma2, residuals = res,
       loglik = loglik, k = 2, converged = TRUE)
}

rw_drift_forecast <- function(model, h, last_y = NULL) {
  list(mean = rep(model$mu, h), sigma2_l = model$sigma2 * seq_len(h))
}
