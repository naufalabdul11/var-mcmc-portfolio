# Value at Risk of an agribusiness stock portfolio with Markov chain Monte Carlo

Actuarial Simulation course project, Actuarial Science ITS (June–July 2025). I led the team.

Two years of daily closing prices for three Indonesian agribusiness stocks, TBLA, LSIP and SMAR. The script computes daily returns and their characteristics, fits the return distributions, simulates 10,000 scenarios with Markov chain Monte Carlo to estimate daily Value at Risk, and builds the minimum variance efficient portfolio (MVEP), which puts its largest weight, 53.5%, on SMAR.

## Run it
```r
install.packages(c("MASS", "dplyr", "fitdistrplus", "ggplot2", "logspline", "openxlsx"))
source("var_mcmc.R")
```
Price data is in `data/`; charts from the original run are in `images/`.

Credit: built with my Actuarial Simulation project team at ITS. Portfolio: https://naufalabdul11.github.io
