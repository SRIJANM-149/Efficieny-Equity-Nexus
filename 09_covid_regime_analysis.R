############################################################
# FILE: 09_covid_regime_analysis.R
# PURPOSE:
#   Pre-, During-, and Post-COVID performance analysis
#   Focus on structural resilience and recovery
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
growth <- fread("Data/CLEANED/growth_scores.csv")

############################################################
# 2. STANDARDISE & FILTER
############################################################

# Regulatory screening
dea <- dea[Status == "Included"]

panel <- panel[Banks %in% dea$Banks]

############################################################
# 3. DEFINE COVID REGIMES
############################################################

panel[, Covid_Regime :=
        fifelse(Year <= 2019, "Pre-COVID",
        fifelse(Year <= 2021, "COVID",
                "Post-COVID"))]

############################################################
# 4. REGIME-WISE ANNUALISED LOG GROWTH
############################################################

regime_growth <- panel[
  order(Year),
  .(
    Profit_Growth   = mean(diff(log(`Net Profit` + 1)), na.rm = TRUE),
    Advances_Growth = mean(diff(log(`Total of Advances`)), na.rm = TRUE),
    Deposit_Growth  = mean(diff(log(`Total of Deposits`)), na.rm = TRUE),
    Years           = .N
  ),
  by = .(Banks, Covid_Regime)
]

# Composite regime growth
regime_growth[, Regime_Growth :=
                rowMeans(.SD, na.rm = TRUE),
              .SDcols = c("Profit_Growth",
                          "Advances_Growth",
                          "Deposit_Growth")]

############################################################
# 5. BANK-LEVEL COVID RESILIENCE
############################################################

resilience <- dcast(
  regime_growth,
  Banks ~ Covid_Regime,
  value.var = "Regime_Growth"
)

# Resilience = post-crisis growth minus pre-crisis growth
resilience[, Resilience_Score :=
             `Post-COVID` - `Pre-COVID`]

############################################################
# 6. MERGE CONTEXT (DEA + GROWTH)
############################################################

setnames(dea, "MAES", "Efficiency_Score")

final <- merge(
  resilience,
  dea[, .(Banks, Efficiency_Score)],
  by = "Banks",
  all.x = TRUE
)

# Attach overall growth score (stable, always present)
final <- merge(
  final,
  growth[, .(Banks, Growth_Score)],
  by = "Banks",
  all.x = TRUE
)

############################################################
# 7. COVID ADAPTATION CLASSIFICATION
############################################################

final[, Covid_Adaptation :=
        fifelse(
          `Post-COVID` >= 0 & `Pre-COVID` >= 0,
          "Resilient (Shock Absorber)",
          fifelse(
            `Post-COVID` > `Pre-COVID`,
            "Adaptive (Post-Crisis Improver)",
            "Vulnerable / Subdued"
          )
        )
]

############################################################
# 8. REGIME-LEVEL SUMMARY (SECTOR VIEW)
############################################################

regime_summary <- regime_growth[, .(
  Avg_Growth = mean(Regime_Growth, na.rm = TRUE)
), by = Covid_Regime]

############################################################
# VISUAL 1: REGIME AVERAGE GROWTH
############################################################

p_bar <- ggplot(
  regime_summary,
  aes(x = Covid_Regime, y = Avg_Growth, fill = Covid_Regime)
) +
  geom_col(width = 0.55, alpha = 0.85) +
  geom_text(
    aes(label = round(Avg_Growth, 3)),
    vjust = -0.6,
    size = 4
  ) +
  scale_fill_manual(values = c(
    "Pre-COVID"  = "#4C72B0",
    "COVID"      = "#DD8452",
    "Post-COVID" = "#55A868"
  )) +
  labs(
    title = "Average Banking Growth Across COVID Regimes",
    subtitle = "Annualised log growth across investable banks",
    y = "Average Annualised Log Growth",
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggsave(
  "Data/CLEANED/covid_regime_bar.png",
  plot = p_bar,
  width = 10,
  height = 6,
  dpi = 300
)

############################################################
# VISUAL 2: STRUCTURAL TRAJECTORY
############################################################

regime_summary[, Covid_Regime :=
  factor(Covid_Regime,
         levels = c("Pre-COVID", "COVID", "Post-COVID"))
]

p_line <- ggplot(
  regime_summary,
  aes(x = Covid_Regime, y = Avg_Growth, group = 1)
) +
  geom_line(linewidth = 1.4, color = "black") +
  geom_point(size = 4, color = "#2C7FB8") +
  labs(
    title = "Structural Growth Trajectory of Indian Banking",
    subtitle = "System-level shock absorption followed by normalisation",
    y = "Average Annualised Log Growth",
    x = NULL
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "Data/CLEANED/covid_regime_line.png",
  plot = p_line,
  width = 10,
  height = 6,
  dpi = 300
)

############################################################
# 9. OUTPUT TABLES
############################################################

fwrite(
  regime_summary,
  "Data/CLEANED/covid_regime_summary.csv"
)

fwrite(
  final,
  "Data/CLEANED/covid_bank_resilience.csv"
)

cat("\n✅ FILE 09 COMPLETED — CLIENT-GRADE COVID REGIME ANALYSIS\n")
print(final[, .(Banks, Covid_Adaptation, Resilience_Score)])
