# Emits the markdown tables used in the README, straight from results/, so the
# README numbers are never typed by hand. Writes results/readme_tables.md.

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
m60     <- metrics[metrics$start_origin == 60, ]

order_by <- m60[m60$h == 6, ]
model_order <- order_by$model[order(order_by$RMSE)]

out <- file("results/readme_tables.md", "w")
w   <- function(...) cat(..., "\n", sep = "", file = out)

w("### Table 1 - forecast accuracy, 60-origin backtest")
w("")
w("| Model | h=1 RMSE | h=6 RMSE | h=12 RMSE | vs naive @ h=6 | vs naive @ h=12 |")
w("|---|---|---|---|---|---|")
for (nm in model_order) {
  r <- sapply(HORIZONS, function(hh) m60$RMSE[m60$h == hh & m60$model == nm])
  b <- sapply(HORIZONS, function(hh) m60$RMSE[m60$h == hh & m60$model == BENCH])
  lab <- if (nm == BENCH) sprintf("**%s**", nm) else nm
  pct <- function(i) if (nm == BENCH) "-" else sprintf("%+.1f%%", 100 * (r[i] / b[i] - 1))
  w(sprintf("| %s | %.2f | %.2f | %.2f | %s | %s |",
            lab, r[1], r[2], r[3], pct(2), pct(3)))
}
w("")
w(sprintf("Origins: %d at h=1, %d at h=6, %d at h=12.",
          m60$n_origins[m60$h == 1][1],
          m60$n_origins[m60$h == 6][1],
          m60$n_origins[m60$h == 12][1]))
w("")

w("### Table 2 - is any model significantly better than the random walk?")
w("")
w("| Model | h | DM p | Clark-West p | Verdict |")
w("|---|---|---|---|---|")
d60 <- dm[dm$start_origin == 60, ]
for (hh in HORIZONS) {
  s <- d60[d60$h == hh, ]
  s <- s[order(s$rmse_model), ]
  for (i in seq_len(nrow(s))) {
    verdict <- if (s$dm_p[i] < 0.05) "different" else "indistinguishable"
    w(sprintf("| %s | %d | %.3f | %.3f | %s |",
              s$model[i], hh, s$dm_p[i], s$cw_p[i], verdict))
  }
}
w("")

w("### Table 3 - does the ranking depend on where the backtest starts?")
w("")
w("| Model | h=6 rank @48 | @60 | @72 | h=1 CW p @48 | @60 | @72 |")
w("|---|---|---|---|---|---|---|")
rank_at <- function(s, hh, nm) {
  sub <- metrics[metrics$start_origin == s & metrics$h == hh, ]
  rank(sub$RMSE, ties.method = "min")[which(sub$model == nm)]
}
cw_at <- function(s, nm) {
  x <- dm[dm$start_origin == s & dm$h == 1 & dm$model == nm, ]
  if (nrow(x) == 0) NA_real_ else x$cw_p
}
for (nm in model_order) {
  ranks <- sapply(STARTS, rank_at, hh = 6, nm = nm)
  cws   <- sapply(STARTS, cw_at, nm = nm)
  cwtxt <- if (nm == BENCH) c("-", "-", "-") else sprintf("%.3f", cws)
  lab   <- if (nm == BENCH) sprintf("**%s**", nm) else nm
  w(sprintf("| %s | %d | %d | %d | %s | %s | %s |",
            lab, ranks[1], ranks[2], ranks[3], cwtxt[1], cwtxt[2], cwtxt[3]))
}
w("")

naive_wins <- all(sapply(STARTS, function(s)
  all(sapply(c(6, 12), function(hh) rank_at(s, hh, BENCH) == 1))))
h6_identical <- all(sapply(STARTS, function(s)
  identical(order(metrics$RMSE[metrics$start_origin == s & metrics$h == 6]),
            order(metrics$RMSE[metrics$start_origin == 60 & metrics$h == 6]))))
any_sig <- any(sapply(setdiff(model_order, BENCH), function(nm)
  all(sapply(STARTS, cw_at, nm = nm) < 0.05)))

w("### Facts asserted in the README")
w("")
w(sprintf("- naive ranks first at h=6 and h=12 at every start: %s", naive_wins))
w(sprintf("- h=6 ordering identical across all three starts: %s", h6_identical))
w(sprintf("- any model significant (CW p<0.05) at all three starts: %s", any_sig))
w(sprintf("- runs with zero fit failures: %s",
          all(sapply(STARTS, function(s) {
            suffix <- if (s == 60) "" else paste0("_start", s)
            nrow(read.csv(sprintf("results/fit_failures%s.csv", suffix))) == 0
          }))))
close(out)

cat(readLines("results/readme_tables.md"), sep = "\n")
