# Builds the PNG charts used in the README from the files in results/.
# Run from the project root after the backtests and significance tests.

source("R/data.R")

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
dir.create("figures", showWarnings = FALSE)

model_order <- c("Naive RW", "Naive RW (bias-c)", "RW + drift",
                 "AR(1)", "MA(1)", "MA(2)", "ARMA(1,1)", "Auto-ARIMA")
cols <- c("#C62828", "#EF6C00", "#B8860B", "#1F78B4",
          "#6A3D9A", "#8C6D31", "#D81B8C", "#0F9D77")
pchs <- c(19, 17, 15, 18, 4, 3, 8, 6)
ltys <- c(1, 2, 3, 4, 5, 6, 2, 4)
lwds <- c(3.4, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7)
names(cols) <- names(pchs) <- names(ltys) <- names(lwds) <- model_order

m60 <- metrics[metrics$start_origin == 60, ]

png("figures/rmse_by_horizon.png", width = 1500, height = 950, res = 170)
par(mar = c(5.2, 4.5, 3.5, 10.5), xpd = FALSE)
plot(NA, xlim = c(0.7, 12.3), ylim = range(m60$RMSE) * c(0.93, 1.05), xaxt = "n",
     xlab = "", ylab = "RMSE (USD)",
     main = "Forecast error grows with horizon, and the random walk stays ahead")
axis(1, at = HORIZONS, lwd = 0, lwd.ticks = 1)
mtext("Forecast horizon (months)", side = 1, line = 2.4)
mtext("only h = 1, 6 and 12 are measured; lines join those points",
      side = 1, line = 3.6, cex = 0.72, col = "grey35")
abline(v = HORIZONS, col = "grey92")
grid(nx = NA, ny = NULL, col = "grey92")
for (nm in model_order) {
  s <- m60[m60$model == nm, ]
  s <- s[order(s$h), ]
  lines(s$h, s$RMSE, col = cols[nm], lwd = lwds[nm], lty = ltys[nm],
        type = "b", pch = pchs[nm], cex = 1)
}
legend("topleft", inset = c(1.02, 0), xpd = TRUE, bty = "n", cex = 0.8,
       legend = model_order, col = cols[model_order],
       lwd = lwds[model_order], lty = ltys[model_order], pch = pchs[model_order])
dev.off()

png("figures/pct_worse_than_naive.png", width = 1500, height = 950, res = 170)
par(mar = c(4.5, 4.5, 3.5, 9))
others <- setdiff(model_order, BENCH)
mat <- sapply(HORIZONS, function(hh) {
  b <- m60$RMSE[m60$h == hh & m60$model == BENCH]
  sapply(others, function(nm) 100 * (m60$RMSE[m60$h == hh & m60$model == nm] / b - 1))
})
colnames(mat) <- paste0("h = ", HORIZONS)
bp <- barplot(mat, beside = TRUE, col = cols[others], border = NA,
              ylim = range(c(0, mat)) * 1.2,
              ylab = "% worse than the naive random walk",
              main = "Every model is worse than doing nothing at 6 and 12 months")
abline(h = 0, lwd = 2)
legend("topleft", inset = c(1.02, 0), xpd = TRUE, bty = "n", cex = 0.78,
       legend = others, fill = cols[others], border = NA)
dev.off()

png("figures/rank_stability.png", width = 1500, height = 1050, res = 170)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3.5, 1), oma = c(5, 0, 0, 0))
for (hh in c(6, 1)) {
  plot(NA, xlim = c(0.8, 3.2), ylim = c(8.5, 0.5), xaxt = "n", yaxt = "n",
       xlab = "Backtest start origin", ylab = "Rank by RMSE (1 = best)",
       main = sprintf("h = %d month%s", hh, if (hh == 1) "" else "s"))
  axis(2, at = 1:8, las = 1, lwd = 0, lwd.ticks = 1)
  axis(1, at = 1:3, labels = STARTS, lwd = 0, lwd.ticks = 1)
  grid(col = "grey88")
  for (nm in model_order) {
    r <- sapply(STARTS, function(s) {
      x <- metrics[metrics$start_origin == s & metrics$h == hh & metrics$model == nm, ]
      if (nrow(x) == 0) NA else rank(metrics$RMSE[metrics$start_origin == s &
                                                  metrics$h == hh])[
        which(metrics$model[metrics$start_origin == s & metrics$h == hh] == nm)]
    })
    lines(1:3, r, col = cols[nm], lwd = lwds[nm], lty = ltys[nm],
          type = "b", pch = pchs[nm], cex = 1)
  }
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", horiz = FALSE, ncol = 4, bty = "n", cex = 0.72,
       legend = model_order, col = cols[model_order],
       lwd = lwds[model_order], lty = ltys[model_order], pch = pchs[model_order])
dev.off()

png("figures/cw_pvalues.png", width = 1500, height = 950, res = 170)
par(mar = c(4.5, 4.5, 3.5, 9))
d1 <- dm[dm$h == 1, ]
mods <- setdiff(model_order, BENCH)
plot(NA, xlim = c(0.8, 3.2), ylim = c(0, max(d1$cw_p) * 1.1), xaxt = "n",
     xlab = "Backtest start origin", ylab = "Clark-West p-value",
     main = "The only significant results at h = 1 do not survive a different start")
axis(1, at = 1:3, labels = STARTS, lwd = 0, lwd.ticks = 1)
grid(col = "grey88")
abline(h = 0.05, col = "firebrick", lty = 2, lwd = 2)
text(3.15, 0.075, "p = 0.05", col = "firebrick", cex = 0.75, pos = 2)
for (nm in mods) {
  p <- sapply(STARTS, function(s) {
    x <- d1[d1$start_origin == s & d1$model == nm, ]
    if (nrow(x) == 0) NA else x$cw_p
  })
  lines(1:3, p, col = cols[nm], lwd = 1.9, lty = ltys[nm], type = "b",
        pch = pchs[nm], cex = 1)
}
legend("topleft", inset = c(1.02, 0), xpd = TRUE, bty = "n", cex = 0.8,
       legend = mods, col = cols[mods], lwd = 1.9, lty = ltys[mods], pch = pchs[mods])
dev.off()

d <- load_brent()
png("figures/series.png", width = 1500, height = 850, res = 170)
par(mar = c(4.5, 4.5, 3.5, 1))
plot(d$dates, d$y, type = "l", col = "steelblue", lwd = 1.8,
     xlab = "", ylab = "Brent crude (USD)",
     main = "Monthly Brent crude, and the region used as backtest origins")
rect(d$dates[61], par("usr")[3], d$dates[length(d$dates)], par("usr")[4],
     col = adjustcolor("firebrick", 0.07), border = NA)
lines(d$dates, d$y, col = "steelblue", lwd = 1.8)
abline(v = d$dates[61], col = "firebrick", lty = 2, lwd = 1.6)
legend("topleft", bty = "n", cex = 0.8,
       legend = c("Brent price", "first backtest origin (start 60)"),
       col = c("steelblue", "firebrick"), lty = c(1, 2), lwd = c(1.8, 1.6))
dev.off()

cat("Written:\n")
for (f in list.files("figures", full.names = TRUE)) {
  cat(sprintf("  %-42s %6.0f KB\n", f, file.size(f) / 1024))
}
