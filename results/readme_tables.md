### Table 1 - forecast accuracy, 60-origin backtest

| Model | h=1 RMSE | h=6 RMSE | h=12 RMSE | vs naive @ h=6 | vs naive @ h=12 |
|---|---|---|---|---|---|
| **Naive RW** | 6.77 | 15.35 | 22.77 | - | - |
| Naive RW (bias-c) | 6.77 | 15.42 | 23.09 | +0.4% | +1.4% |
| RW + drift | 6.81 | 16.17 | 25.15 | +5.4% | +10.4% |
| MA(1) | 6.71 | 16.37 | 25.58 | +6.6% | +12.3% |
| AR(1) | 6.70 | 16.50 | 25.47 | +7.5% | +11.9% |
| ARMA(1,1) | 6.76 | 16.53 | 25.79 | +7.7% | +13.2% |
| MA(2) | 6.84 | 16.71 | 26.00 | +8.9% | +14.2% |
| Auto-ARIMA | 7.23 | 18.43 | 25.32 | +20.1% | +11.2% |

Origins: 74 at h=1, 69 at h=6, 63 at h=12.

### Table 2 - is any model significantly better than the random walk?

| Model | h | DM p | Clark-West p | Verdict |
|---|---|---|---|---|
| AR(1) | 1 | 0.821 | 0.071 | indistinguishable |
| MA(1) | 1 | 0.850 | 0.034 | indistinguishable |
| ARMA(1,1) | 1 | 0.972 | 0.041 | indistinguishable |
| Naive RW (bias-c) | 1 | 0.902 | 0.357 | indistinguishable |
| RW + drift | 1 | 0.505 | 0.671 | indistinguishable |
| MA(2) | 1 | 0.862 | 0.056 | indistinguishable |
| Auto-ARIMA | 1 | 0.265 | 0.120 | indistinguishable |
| Naive RW (bias-c) | 6 | 0.932 | 0.436 | indistinguishable |
| RW + drift | 6 | 0.395 | 0.765 | indistinguishable |
| MA(1) | 6 | 0.457 | 0.623 | indistinguishable |
| AR(1) | 6 | 0.368 | 0.666 | indistinguishable |
| ARMA(1,1) | 6 | 0.427 | 0.612 | indistinguishable |
| MA(2) | 6 | 0.418 | 0.570 | indistinguishable |
| Auto-ARIMA | 6 | 0.472 | 0.635 | indistinguishable |
| Naive RW (bias-c) | 12 | 0.889 | 0.450 | indistinguishable |
| RW + drift | 12 | 0.132 | 0.950 | indistinguishable |
| Auto-ARIMA | 12 | 0.500 | 0.413 | indistinguishable |
| AR(1) | 12 | 0.265 | 0.822 | indistinguishable |
| MA(1) | 12 | 0.311 | 0.781 | indistinguishable |
| ARMA(1,1) | 12 | 0.321 | 0.754 | indistinguishable |
| MA(2) | 12 | 0.367 | 0.691 | indistinguishable |

### Table 3 - does the ranking depend on where the backtest starts?

| Model | h=6 rank @48 | @60 | @72 | h=1 CW p @48 | @60 | @72 |
|---|---|---|---|---|---|---|
| **Naive RW** | 1 | 1 | 1 | - | - | - |
| Naive RW (bias-c) | 2 | 2 | 2 | 0.349 | 0.357 | 0.341 |
| RW + drift | 3 | 3 | 3 | 0.680 | 0.671 | 0.578 |
| MA(1) | 4 | 4 | 4 | 0.036 | 0.034 | 0.133 |
| AR(1) | 5 | 5 | 5 | 0.072 | 0.071 | 0.187 |
| ARMA(1,1) | 6 | 6 | 6 | 0.043 | 0.041 | 0.152 |
| MA(2) | 7 | 7 | 7 | 0.056 | 0.056 | 0.198 |
| Auto-ARIMA | 8 | 8 | 8 | 0.116 | 0.120 | 0.449 |

### Facts asserted in the README

- naive ranks first at h=6 and h=12 at every start: TRUE
- h=6 ordering identical across all three starts: TRUE
- any model significant (CW p<0.05) at all three starts: FALSE
- runs with zero fit failures: TRUE
