# Compares the backtest run from three different start origins, to check the
# ranking is a property of the data and not of where the backtest happens to begin.

STARTS   <- c(48, 60, 72)
HORIZONS <- c(1, 6, 12)
BENCH    <- "Naive RW"

read_start <- function(kind, start) {
  suffix <- if (start == 60) "" else paste0("_start", start)
  path   <- sprintf("results/%s%s.csv", kind, suffix)
  if (!file.exists(path)) return(NULL)
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$start_origin <- start
  df
}

metrics <- do.call(rbind, lapply(STARTS, function(s) read_start("metrics", s)))
dm      <- do.call(rbind, lapply(STARTS, function(s) read_start("dm_tests", s)))

if (is.null(metrics)) stop("no metrics files found; run R/backtest.R first")
available <- sort(unique(metrics$start_origin))
cat("Start origins available:", paste(available, collapse = ", "), "\n\n")

metrics$rank <- NA_integer_
for (s in available) {
  for (hh in HORIZONS) {
    i <- metrics$start_origin == s & metrics$h == hh
    metrics$rank[i] <- rank(metrics$RMSE[i], ties.method = "min")
  }
}

write.csv(metrics[order(metrics$h, metrics$start_origin, metrics$rank), ],
          "results/sensitivity.csv", row.names = FALSE)

for (hh in HORIZONS) {
  cat(sprintf("===== horizon h = %d : RMSE (rank) by start origin =====\n", hh))
  sub    <- metrics[metrics$h == hh, ]
  models <- unique(sub$model[order(sub$rank)])
  tab <- data.frame(model = models, stringsAsFactors = FALSE)
  for (s in available) {
    col <- sapply(models, function(m) {
      r <- sub[sub$start_origin == s & sub$model == m, ]
      if (nrow(r) == 0) NA_character_
      else sprintf("%7.2f (%d)", r$RMSE, r$rank)
    })
    tab[[paste0("start", s)]] <- col
  }
  print(tab, row.names = FALSE)
  cat("\n")
}

cat("=== Q1: does the naive random walk still win at h = 6 and h = 12? ===\n")
for (hh in c(6, 12)) {
  for (s in available) {
    sub  <- metrics[metrics$h == hh & metrics$start_origin == s, ]
    best <- sub$model[which.min(sub$RMSE)]
    cat(sprintf("  h=%-2d start=%-3d best: %-18s %s\n", hh, s, best,
                if (best == BENCH) "naive wins" else "NAIVE DOES NOT WIN"))
  }
}

cat("\n=== Q2: is the h = 1 ordering stable across start origins? ===\n")
h1 <- metrics[metrics$h == 1, ]
if (length(available) > 1) {
  for (i in seq_along(available)) {
    for (j in seq_along(available)) {
      if (j <= i) next
      a <- h1[h1$start_origin == available[i], ]
      b <- h1[h1$start_origin == available[j], ]
      common <- intersect(a$model, b$model)
      rho <- cor(a$rank[match(common, a$model)],
                 b$rank[match(common, b$model)], method = "spearman")
      cat(sprintf("  rank correlation start%d vs start%d: %+.3f\n",
                  available[i], available[j], rho))
    }
  }
}

cat("\n=== Q3: do the marginal Clark-West results at h = 1 survive? ===\n")
if (!is.null(dm)) {
  d1 <- dm[dm$h == 1, ]
  models <- unique(d1$model)
  tab <- data.frame(model = models, stringsAsFactors = FALSE)
  for (s in available) {
    tab[[paste0("CW_p_start", s)]] <- sapply(models, function(m) {
      r <- d1[d1$start_origin == s & d1$model == m, ]
      if (nrow(r) == 0) NA_real_ else round(r$cw_p, 4)
    })
  }
  tab$significant_at_all_starts <- apply(
    tab[, grepl("^CW_p_start", names(tab)), drop = FALSE], 1,
    function(v) all(!is.na(v) & v < 0.05))
  print(tab, row.names = FALSE)
} else {
  cat("  no dm_tests files found; run R/run_significance.R for each start\n")
}

cat("\nWritten: results/sensitivity.csv\n")
