############################################################
# FILE: 11_idbi_public_vs_private.R
# PURPOSE:
#   Compare IDBI Bank performance before and after
#   change in ownership structure
############################################################

rm(list = ls())
gc()

library(data.table)
library(ggplot2)

############################################################
# 1. LOAD DATA
############################################################

panel  <- fread("Data/CLEANED/bank_panel.csv")
dea    <- fread("Data/CLEANED/dea_managerial_screening.csv")
covid  <- fread("Data/CLEANED/covid_bank_resilience.csv")

############################################################
# 2. FILTER IDBI DATA
############################################################

idbi_panel <- panel[Banks == "IDBI BANK LIMITED"]
stopifnot(nrow(idbi_panel) > 0)

############################################################
# 3. DEFINE OWNERSHIP PERIODS
############################################################

# LIC acquired control in 2019
idbi_panel[, Ownership :=
             fifelse(Year <= 2018,
                     "Government-Owned Period",
                     "Post-Ownership Change")]

############################################################
# 4. PERIOD-WISE AVERAGE ANNUAL GROWTH
############################################################

idbi_regime <- idbi_panel[
  order(Year),
  .(
    Profit_Growth   = mean(diff(log(`Net Profit` + 1)), na.rm = TRUE),
    Advances_Growth = mean(diff(log(`Total of Advances`)), na.rm = TRUE),
    Deposit_Growth  = mean(diff(log(`Total of Deposits`)), na.rm = TRUE),
    NPA_Change      = last(`Net NPA`) - first(`Net NPA`),
    Years           = .N
  ),
  by = Ownership
]

# Simple overall growth indicator
idbi_regime[, Overall_Growth :=
              rowMeans(.SD, na.rm = TRUE),
            .SDcols = c("Profit_Growth",
                        "Advances_Growth",
                        "Deposit_Growth")]

############################################################
# 5. ADD CONTEXT 
############################################################

setnames(dea, "MAES", "Efficiency_Score")

idbi_regime[, Efficiency_Context :=
              dea[Banks == "IDBI BANK LIMITED", Efficiency_Score]]

idbi_regime[, Covid_Context :=
              covid[Banks == "IDBI BANK LIMITED", Covid_Adaptation]]

############################################################
# 6. VISUAL — BEFORE VS AFTER OWNERSHIP CHANGE
############################################################

p <- ggplot(
  idbi_regime,
  aes(x = Ownership, y = Overall_Growth, fill = Ownership)
) +
  geom_col(width = 0.55, alpha = 0.8) +
  geom_text(
    aes(label = round(Overall_Growth, 3)),
    vjust = -0.6,
    size = 4
  ) +
  scale_fill_manual(values = c(
    "Government-Owned Period"   = "#4C72B0",
    "Post-Ownership Change"     = "#55A868"
  )) +
  labs(
    title = "IDBI Bank — Performance Before and After Ownership Change",
    subtitle =
      "Comparison of average annual business growth across the two periods",
    y = "Average Annual Growth",
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "grey40")
  )

ggsave(
  "Data/CLEANED/idbi_public_private_comparison.png",
  plot = p,
  width = 9,
  height = 5,
  dpi = 300
)

############################################################
# 7. OUTPUT TABLE
############################################################

fwrite(
  idbi_regime,
  "Data/CLEANED/idbi_regime_comparison.csv"
)

cat("\n✅ FILE 11 COMPLETED — IDBI OWNERSHIP COMPARISON READY\n")
print(idbi_regime)
