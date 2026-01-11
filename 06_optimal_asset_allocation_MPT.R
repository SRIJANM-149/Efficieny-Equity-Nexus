############################################################
# FILE: 06_optimal_asset_allocation_MPT.R
# PURPOSE:
#   Optimal asset allocation using Markowitz MPT
#   Universe derived from Monte Carlo conviction robustness
############################################################

rm(list = ls())
gc()

library(data.table)
library(quantmod)
library(ggplot2)

############################################################
# 1. LOAD MONTE CARLO ROBUSTNESS OUTPUT
############################################################

reco <- fread("Data/CLEANED/simulated_scores.csv")

############################################################
# 2. POSITIVE CONVICTION METRIC
############################################################
# Conviction already computed as:
# StrongBuy + 0.5 * Accumulate

stopifnot("Conviction" %in% names(reco))

############################################################
# 3. SELECT INVESTABLE BANKS
############################################################

# Core investable universe
investable <- reco[Conviction >= 0.50, Banks]

# Top 6 by conviction strength
top6 <- reco[Conviction >= 0.50][
  order(-Conviction),
  head(Banks, 6)
]

banks <- unique(top6)
banks <- banks[!is.na(banks)]

cat("Banks used for Asset Allocation:\n")
print(banks)

############################################################
# 4. BANK → TICKER MAP (NSE / Yahoo)
############################################################

ticker_map <- c(
  "HDFC BANK LTD."           = "HDFCBANK.NS",
  "ICICI BANK LIMITED"       = "ICICIBANK.NS",
  "AXIS BANK LIMITED"        = "AXISBANK.NS",
  "STATE BANK OF INDIA"      = "SBIN.NS",
  "KOTAK MAHINDRA BANK LTD." = "KOTAKBANK.NS",
  "INDUSIND BANK LTD"        = "INDUSINDBK.NS",
  "FEDERAL BANK LTD"         = "FEDERALBNK.NS",
  "INDIAN BANK"              = "INDIANB.NS",
  "BANK OF BARODA"           = "BANKBARODA.NS",
  "PUNJAB NATIONAL BANK"     = "PNB.NS",
  "UNION BANK OF INDIA"      = "UNIONBANK.NS",
  "IDBI BANK LIMITED"        = "IDBI.NS",
  "UCO BANK"                 = "UCOBANK.NS",
  "BANK OF INDIA"            = "BANKINDIA.NS"
)

tickers <- ticker_map[banks]

if (any(is.na(tickers))) {
  stop("❌ Missing ticker mapping for one or more selected banks.")
}

############################################################
# 5. DOWNLOAD PRICE DATA
############################################################

prices <- list()

for (b in banks) {

  tk <- ticker_map[b]

  px <- try(
    getSymbols(
      tk,
      src = "yahoo",
      from = "2020-01-01",
      to   = Sys.Date(),
      auto.assign = FALSE
    ),
    silent = TRUE
  )

  if (inherits(px, "try-error")) {
    cat("⚠️ Price data unavailable for:", b, "\n")
    next
  }

  prices[[b]] <- Ad(px)
}

cat("\nAssets with usable price data:\n")
print(names(prices))

prices <- do.call(merge, prices)

############################################################
# 6. COMPUTE DAILY LOG RETURNS
############################################################

returns <- na.omit(diff(log(prices)))

############################################################
# 7. MARKOWITZ MONTE CARLO OPTIMISATION
############################################################

N_SIM <- 5000
rf    <- 0.06

mu  <- colMeans(returns) * 252
cov_mat <- cov(returns) * 252

n_assets <- length(banks)

results <- matrix(NA, nrow = N_SIM, ncol = n_assets + 3)
colnames(results) <- c(
  paste0("w_", banks),
  "Return", "Risk", "Sharpe"
)

for (i in seq_len(N_SIM)) {

  w <- runif(n_assets)
  w <- w / sum(w)

  port_return <- sum(w * mu)
  port_risk   <- sqrt(t(w) %*% cov_mat %*% w)
  sharpe      <- (port_return - rf) / port_risk

  results[i, ] <- c(w, port_return, port_risk, sharpe)
}

results <- as.data.table(results)

############################################################
# 8. SELECT MAX-SHARPE PORTFOLIO
############################################################

optimal <- results[which.max(Sharpe)]

############################################################
# 9. SAVE OPTIMAL ALLOCATION
############################################################

fwrite(
  optimal,
  "Data/CLEANED/optimal_asset_allocation.csv"
)

cat("\n✅ OPTIMAL ASSET ALLOCATION COMPLETED\n")
print(optimal)

############################################################
# 10. COST vs PROFIT VISUALISATION (RAW, EXECUTIVE-GRADE)
############################################################

library(ggplot2)

# ----------------------------------------------------------
# PREP DATA
# ----------------------------------------------------------

plot_data <- copy(results)

# Order by risk (cost axis)
plot_data <- plot_data[order(Risk)]
plot_data[, pct := seq_len(.N) / .N]

# ----------------------------------------------------------
# SMOOTH PROFIT ONLY (VISUAL CLARITY)
# ----------------------------------------------------------

k_profit <- max(5, floor(0.03 * nrow(plot_data)))  # ~3% window

plot_data[, Profit_smooth :=
            frollmean(Return, n = k_profit, align = "center")]

plot_data[, Profit_norm :=
            (Profit_smooth - min(Profit_smooth, na.rm = TRUE)) /
            (max(Profit_smooth, na.rm = TRUE) -
             min(Profit_smooth, na.rm = TRUE))]

# ----------------------------------------------------------
# NORMALISE COST 
# ----------------------------------------------------------

plot_data[, Cost_norm :=
            1 - (Risk - min(Risk)) /
                (max(Risk) - min(Risk))]

# ----------------------------------------------------------
# REDUCE VISUAL DENSITY 
# ----------------------------------------------------------

k_thin <- max(1, floor(nrow(plot_data) / 1200))
plot_data_thin <- plot_data[seq(1, .N, by = k_thin)]

# ----------------------------------------------------------
# MAP OPTIMAL PORTFOLIO
# ----------------------------------------------------------

opt_risk <- optimal$Risk
opt_row <- plot_data[
  which.min(abs(Risk - opt_risk))
]

# ----------------------------------------------------------
# PLOT
# ----------------------------------------------------------

p <- ggplot(plot_data_thin, aes(x = pct)) +

  geom_line(
    aes(y = Cost_norm),
    color = "#B22222",
    linewidth = 1.2
  ) +

  geom_line(
    aes(y = Profit_norm),
    color = "#1B7F3A",
    linewidth = 1.4
  ) +

  geom_point(
    aes(x = opt_row$pct, y = opt_row$Profit_norm),
    size = 3.5,
    color = "black"
  ) +

  annotate(
    "text",
    x = 0.02,
    y = 0.95,
    label = "COST\nAnnualised Volatility\n(from Daily Log Returns)",
    hjust = 0,
    vjust = 1,
    size = 4.5,
    fontface = "bold",
    color = "#B22222"
  ) +

  annotate(
    "text",
    x = 0.02,
    y = 0.05,
    label = "PROFIT\nAnnualised Expected Return\n(from Historical Prices)",
    hjust = 0,
    vjust = 0,
    size = 4.5,
    fontface = "bold",
    color = "#1B7F3A"
  ) +

  annotate(
    "text",
    x = opt_row$pct + 0.03,
    y = opt_row$Profit_norm,
    label = "Max Sharpe Portfolio",
    hjust = 0,
    size = 4
  ) +

  labs(
    title = "Cost–Profit Trade-off from Monte Carlo Portfolio Simulation",
    subtitle =
      "Cost = annualised volatility (ordered frontier) | Profit = smoothed expected return\nProfit smoothed for visual clarity; no smoothing applied to risk",
    x = "Portfolio Risk Rank (Low → High)",
    y = NULL
  ) +

  theme_minimal(base_size = 14) +

  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, color = "grey20"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# ----------------------------------------------------------
# SAVE OUTPUT
# ----------------------------------------------------------

ggsave(
  filename = "Data/CLEANED/profitvscost_exec.png",
  plot = p,
  width = 16,
  height = 9,
  dpi = 300
)

print(p)

