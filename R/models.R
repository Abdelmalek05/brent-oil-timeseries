# Time series models written from scratch: ACF, PACF, the Ljung-Box test, and
# four estimators (AR(1), MA(1), MA(2), ARMA(1,1)) each with a forecast function.

my_acf <- function(x, max_lag) {
  n   <- length(x)
  xc  <- x - mean(x)
  g0  <- sum(xc^2) / n
  rho <- numeric(max_lag)
  for (k in 1:max_lag) {
    rho[k] <- sum(xc[1:(n - k)] * xc[(k + 1):n]) / (n * g0)
  }
  rho
}

my_pacf <- function(acf_vals) {
  p    <- length(acf_vals)
  pacf <- numeric(p)
  pacf[1] <- acf_vals[1]
  phi_prev <- matrix(0, p, p)
  phi_prev[1, 1] <- acf_vals[1]
  for (k in 2:p) {
    num <- acf_vals[k] - sum(phi_prev[k-1, 1:(k-1)] * acf_vals[(k-1):1])
    den <- 1            - sum(phi_prev[k-1, 1:(k-1)] * acf_vals[1:(k-1)])
    phi_curr_kk <- num / den
    pacf[k] <- phi_curr_kk
    phi_curr <- numeric(k)
    phi_curr[k] <- phi_curr_kk
    for (j in 1:(k-1)) {
      phi_curr[j] <- phi_prev[k-1, j] - phi_curr_kk * phi_prev[k-1, k-j]
    }
    phi_prev[k, 1:k] <- phi_curr
  }
  pacf
}

ljung_box <- function(residuals, lag, df_correction = 1) {
  n  <- length(residuals)
  rk <- my_acf(residuals, lag)
  Q  <- n * (n + 2) * sum(rk^2 / (n - 1:lag))
  df <- lag - df_correction
  list(statistic = Q, df = df, p.value = 1 - pchisq(Q, df = df))
}

yule_walker_ar1 <- function(x) {
  xc     <- x - mean(x)
  n      <- length(xc)
  g0     <- sum(xc^2) / n
  g1     <- sum(xc[1:(n-1)] * xc[2:n]) / n
  phi    <- g1 / g0
  res    <- x[-1] - mean(x) - phi * (x[-n] - mean(x))
  sigma2 <- sum(res^2) / (n - 2)
  loglik <- -0.5 * n * log(2 * pi * sigma2) - sum(res^2) / (2 * sigma2)
  list(phi = phi, mu = mean(x), sigma2 = sigma2,
       residuals = res, loglik = loglik, k = 3, converged = TRUE)
}

ar1_forecast <- function(model, h, last_y) {
  phi <- model$phi; mu <- model$mu; sigma2 <- model$sigma2
  fc    <- numeric(h)
  fc[1] <- mu + phi * (last_y - mu)
  if (h >= 2) for (i in 2:h) fc[i] <- mu + phi * (fc[i-1] - mu)
  list(mean = fc, sigma2_l = sigma2 * cumsum(phi^(2 * (0:(h-1)))))
}

css_ma1 <- function(params, x) {
  mu <- params[1]; theta <- params[2]
  n  <- length(x)
  eps <- numeric(n)
  eps[1] <- x[1] - mu
  for (t in 2:n) eps[t] <- (x[t] - mu) + theta * eps[t-1]
  sum(eps^2)
}

fit_ma1 <- function(x) {
  opt <- optim(c(mean(x), 0.3), css_ma1, x = x, method = "BFGS",
               control = list(reltol = 1e-10))
  mu <- opt$par[1]; theta <- opt$par[2]
  n  <- length(x)
  eps <- numeric(n)
  eps[1] <- x[1] - mu
  for (t in 2:n) eps[t] <- (x[t] - mu) + theta * eps[t-1]
  sigma2 <- sum(eps^2) / (n - 2)
  loglik <- -0.5 * n * log(2 * pi * sigma2) - sum(eps^2) / (2 * sigma2)
  list(mu = mu, theta1 = theta, sigma2 = sigma2, residuals = eps,
       loglik = loglik, k = 3, converged = opt$convergence == 0)
}

ma1_forecast <- function(model, h, last_y = NULL) {
  mu <- model$mu; theta <- model$theta1; sigma2 <- model$sigma2
  fc    <- numeric(h)
  fc[1] <- mu - theta * tail(model$residuals, 1)
  if (h >= 2) fc[2:h] <- mu
  sigma2_l <- c(sigma2, rep(sigma2 * (1 + theta^2), max(0, h - 1)))
  list(mean = fc, sigma2_l = sigma2_l[seq_len(h)])
}

css_ma2 <- function(params, x) {
  mu <- params[1]; th1 <- params[2]; th2 <- params[3]
  n  <- length(x)
  eps <- numeric(n)
  eps[1] <- x[1] - mu
  eps[2] <- (x[2] - mu) + th1 * eps[1]
  for (t in 3:n) eps[t] <- (x[t] - mu) + th1 * eps[t-1] + th2 * eps[t-2]
  sum(eps^2)
}

fit_ma2 <- function(x) {
  opt <- optim(c(mean(x), 0.3, 0.1), css_ma2, x = x, method = "BFGS",
               control = list(reltol = 1e-10))
  mu <- opt$par[1]; th1 <- opt$par[2]; th2 <- opt$par[3]
  n  <- length(x)
  eps <- numeric(n)
  eps[1] <- x[1] - mu
  eps[2] <- (x[2] - mu) + th1 * eps[1]
  for (t in 3:n) eps[t] <- (x[t] - mu) + th1 * eps[t-1] + th2 * eps[t-2]
  sigma2 <- sum(eps^2) / (n - 3)
  loglik <- -0.5 * n * log(2 * pi * sigma2) - sum(eps^2) / (2 * sigma2)
  list(mu = mu, theta1 = th1, theta2 = th2, sigma2 = sigma2, residuals = eps,
       loglik = loglik, k = 4, converged = opt$convergence == 0)
}

ma2_forecast <- function(model, h, last_y = NULL) {
  mu <- model$mu; th1 <- model$theta1; th2 <- model$theta2
  sigma2 <- model$sigma2
  eps <- model$residuals
  eps_T <- tail(eps, 1); eps_Tm1 <- tail(eps, 2)[1]
  fc <- numeric(h)
  fc[1] <- mu - th1 * eps_T - th2 * eps_Tm1
  if (h >= 2) fc[2] <- mu - th2 * eps_T
  if (h >  2) fc[3:h] <- mu
  sigma2_l <- c(sigma2,
                sigma2 * (1 + th1^2),
                rep(sigma2 * (1 + th1^2 + th2^2), max(0, h - 2)))
  list(mean = fc, sigma2_l = sigma2_l[seq_len(h)])
}

css_arma11 <- function(params, x) {
  mu <- params[1]; phi <- params[2]; theta <- params[3]
  n  <- length(x)
  eps <- numeric(n)
  eps[1] <- x[1] - mu
  for (t in 2:n) eps[t] <- (x[t] - mu) - phi * (x[t-1] - mu) + theta * eps[t-1]
  sum(eps^2)
}

fit_arma11 <- function(x) {
  opt <- optim(c(mean(x), 0.2, 0.2), css_arma11, x = x, method = "BFGS",
               control = list(reltol = 1e-10))
  mu <- opt$par[1]; phi <- opt$par[2]; theta <- opt$par[3]
  n  <- length(x)
  eps <- numeric(n)
  eps[1] <- x[1] - mu
  for (t in 2:n) eps[t] <- (x[t] - mu) - phi * (x[t-1] - mu) + theta * eps[t-1]
  sigma2 <- sum(eps^2) / (n - 3)
  loglik <- -0.5 * n * log(2 * pi * sigma2) - sum(eps^2) / (2 * sigma2)
  list(mu = mu, phi = phi, theta = theta, sigma2 = sigma2, residuals = eps,
       loglik = loglik, k = 4, converged = opt$convergence == 0)
}

arma11_forecast <- function(model, h, last_y) {
  mu <- model$mu; phi <- model$phi; theta <- model$theta
  sigma2 <- model$sigma2
  fc    <- numeric(h)
  fc[1] <- mu + phi * (last_y - mu) - theta * tail(model$residuals, 1)
  if (h >= 2) for (i in 2:h) fc[i] <- mu + phi * (fc[i-1] - mu)
  psi_1    <- phi - theta
  psi_vals <- c(1, psi_1, if (h > 2) phi^(1:(h-2)) * psi_1 else numeric(0))
  list(mean = fc, sigma2_l = sigma2 * cumsum(psi_vals^2)[seq_len(h)])
}

aic_bic <- function(loglik, k, n) {
  c(AIC = -2 * loglik + 2 * k, BIC = -2 * loglik + k * log(n))
}

forecast_to_price <- function(fc, last_log, bias_correct = TRUE) {
  adj <- if (bias_correct) 0.5 * fc$sigma2_l else 0
  exp(last_log + cumsum(fc$mean) + adj)
}
