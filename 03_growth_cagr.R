############################################################
# FILE: 03_growth_cagr.R
############################################################

rm(list=ls()); gc()

library(data.table)

panel <- fread("Data/CLEANED/bank_panel.csv")
dea   <- fread("Data/CLEANED/dea_managerial_screening.csv")

setnames(panel, trimws(names(panel)))
setnames(dea, trimws(names(dea)))

panel <- panel[Banks %in% dea[Status=="Included", Banks]]

panel[, Year := as.integer(Year)]
setorder(panel, Banks, Year)

# ----------------------------------------------------------
# ROBUST CAGR
# ----------------------------------------------------------

cagr <- function(x, cap = 0.50) {

  x <- as.numeric(x)
  x <- x[!is.na(x)]

  if (length(x) < 2) return(NA_real_)

  start <- x[1]
  end   <- tail(x, 1)
  yrs   <- length(x) - 1

  if (start <= 0) return(NA_real_)
  if (end   <= 0) return(-cap)

  val <- (end / start)^(1 / yrs) - 1

  if (!is.finite(val)) return(NA_real_)

  if (val > cap)  return(cap)
  if (val < -cap) return(-cap)

  return(as.numeric(val))
}

# ----------------------------------------------------------
# REGIMES
# ----------------------------------------------------------

panel[, Regime :=
       fifelse(Year<=2019,"Pre-COVID",
       fifelse(Year<=2021,"COVID","Post-COVID"))]

# ----------------------------------------------------------
# REGIME GROWTH
# ----------------------------------------------------------

g <- panel[, .(
  Adv_CAGR = cagr(`Total of Advances`),
  Dep_CAGR = cagr(`Total of Deposits`),
  NII_CAGR = cagr(`Net Interest Income`),
  Inc_CAGR = cagr(`Total Income`),
  Prof_CAGR= cagr(`Net Profit`)
), by=.(Banks,Regime)]

g[, Regime_Growth :=
     rowMeans(.SD, na.rm=TRUE),
     .SDcols=patterns("_CAGR")]

###########################################################
# PROFIT STABILITY PENALTY
###########################################################

g[, Profit_Vol :=
     sd(Prof_CAGR, na.rm=TRUE),
   by = Banks]

g[, Quality_Adjust :=
     fifelse(Profit_Vol > 0.30, -0.10, 0)]

# ----------------------------------------------------------
# MARKET STYLE AGGREGATION
# ----------------------------------------------------------

growth <- g[, .(
  Growth_Score =
    0.45*max(Regime_Growth, na.rm=TRUE) +
    0.35*median(Regime_Growth, na.rm=TRUE) +
    0.20*last(Regime_Growth) +
    mean(Quality_Adjust, na.rm=TRUE)
), by = Banks]
###########################################################
# GROWTH QUALITY MULTIPLIER (STABILITY BONUS)
###########################################################

# Profit stability proxy
panel[, Profit_Margin :=
         `Net Profit` / `Total Income`]

profit_stab <- panel[
  ,
  .(Profit_Stability = sd(Profit_Margin, na.rm = TRUE)),
  by = Banks
]

growth <- merge(growth, profit_stab, by = "Banks", all.x = TRUE)

# Reward stable profit generators
growth[, Growth_Score :=
         Growth_Score *
         fifelse(
           Profit_Stability < 0.15,   # stable earnings
           1.10,                      # bonus
           1.00
         )]


# Long-run median ROA proxy using Net Profit / Advances
panel[, ROA_proxy :=
         `Net Profit` / `Total of Advances`]

roa_bank <- panel[
  ,
  .(ROA_Med = median(ROA_proxy, na.rm = TRUE)),
  by = Banks
]

# Attach to growth table
growth <- merge(growth, roa_bank, by = "Banks", all.x = TRUE)

# Penalise structurally weak profitability
growth[, Growth_Score :=
         Growth_Score *
         fifelse(
           ROA_Med < 0.006,   # ~0.6% ROA threshold
           0.85,
           1.00
         )]

fwrite(growth, "Data/CLEANED/growth_scores.csv")

cat("\n✅ FILE 03 COMPLETED\n")
