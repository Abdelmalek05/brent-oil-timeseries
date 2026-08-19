# Reads the Brent price CSV and turns it into monthly log-returns.
# That log-return series is what every model is fitted on.

load_brent <- function(path = "data/brent_oil_monthly.csv") {
  raw       <- read.csv(path)
  price_col <- suppressWarnings(as.numeric(raw$Close))
  date_col  <- as.Date(raw$Price, format = "%Y-%m-%d")
  keep      <- !is.na(price_col) & !is.na(date_col)

  y     <- price_col[keep]
  dates <- date_col[keep]
  log_y <- log(y)

  list(
    dates  = dates,
    y      = y,
    log_y  = log_y,
    diff_y = diff(log_y),
    n      = length(y)
  )
}

submission_split <- function(d, frac = 0.9) {
  m     <- length(d$diff_y)
  split <- floor(frac * m)
  list(
    split          = split,
    train          = d$diff_y[1:split],
    test           = d$diff_y[(split + 1):m],
    h              = m - split,
    last_log_train = d$log_y[split]
  )
}
