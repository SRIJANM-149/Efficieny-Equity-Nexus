############################################################
# FILE: 10_psu_vs_private_analysis.R
# PURPOSE:
#   Simple comparison of Private vs Government-Owned banks
#   using an overall performance score
############################################################

rm(list = ls())
gc()

library(data.table)
library(ggplot2)

############################################################
# 1. LOAD DATA
############################################################

fusion <- fread("Data/CLEANED/fusion_scores.csv")
dea    <- fread("Data/CLEANED/dea_managerial_screening.csv")
growth <- fread("Data/CLEANED/growth_scores.csv")
covid  <- fread("Data/CLEANED/covid_bank_resilience.csv")

############################################################
# 2. CLEAN & FILTER DATA
############################################################

# Keep only banks cleared by screening
dea <- dea[Status == "Included"]

# Rename efficiency column
setnames(dea, "MAES", "Efficiency_Score")

############################################################
# 3. MERGE REQUIRED INFORMATION
############################################################

data <- Reduce(
  function(x, y) merge(x, y, by = "Banks", all = FALSE),
  list(
    fusion[, .(Banks, Sector, Fusion_Score)],
    dea[, .(Banks, Efficiency_Score)],
    growth[, .(Banks, Growth_Score)],
    covid[, .(Banks, Covid_Adaptation)]
  )
)

############################################################
# 4. STANDARDISE BANK TYPE NAMES
############################################################

data[, Sector := toupper(trimws(Sector))]

data[Sector %in% c("PUBLIC", "PSU"), Sector := "Government-Owned"]
data[Sector %in% c("PRIVATE"), Sector := "Private"]

# Keep only valid bank types
data <- data[Sector %in% c("Private", "Government-Owned")]

############################################################
# 5. CHECK DATA BALANCE
############################################################

print(table(data$Sector))

############################################################
# 6. VISUAL — OVERALL COMPARISON 
############################################################

p <- ggplot(
  data,
  aes(x = Sector, y = Fusion_Score, fill = Sector)
) +
  geom_boxplot(
    width = 0.55,
    alpha = 0.75,
    outlier.shape = 21,
    outlier.size = 2
  ) +
  scale_fill_manual(values = c(
    "Private"           = "#55A868",
    "Government-Owned"  = "#4C72B0"
  )) +
  labs(
    title = "Private vs Government-Owned Banks — Overall Comparison",
    subtitle =
      "This chart compares private and government-owned banks based on overall business performance,\ncombining efficiency, growth, and market performance into a single score.",
    y = "Overall Bank Score",
    x = "Type of Bank (Private vs Government-Owned)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "grey40")
  )

############################################################
# 7. SAVE OUTPUT
############################################################

ggsave(
  filename = "Data/CLEANED/psu_private_comparison.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)

############################################################
# 8. SAVE SUMMARY TABLE
############################################################

summary_table <- data[, .(
  Average_Score = mean(Fusion_Score),
  Median_Score  = median(Fusion_Score),
  Bank_Count    = .N
), by = Sector]

fwrite(
  summary_table,
  "Data/CLEANED/psu_vs_private_summary.csv"
)

cat("\n✅ FILE 10 COMPLETED — SIMPLE PRIVATE VS GOVERNMENT BANK COMPARISON\n")
print(summary_table)
