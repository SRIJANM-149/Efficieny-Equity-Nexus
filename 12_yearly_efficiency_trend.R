############################################################
# FILE: 12_yearly_efficiency_trend.R
# PURPOSE:
#   Yearly operational efficiency trend for banks
############################################################

rm(list = ls())
gc()

library(data.table)
library(ggplot2)
library(scales)

############################################################
# 1. LOAD DATA
############################################################

panel <- fread("Data/CLEANED/bank_panel.csv")

# Ensure Year is numeric
panel[, Year := as.integer(Year)]

############################################################
# 2. CONSTRUCT YEARLY EFFICIENCY PROXY
############################################################
# Intuitive proxy:
# Profit relative to assets, operating costs, and workforce

panel[, Efficiency_Raw :=
        `Net Profit` /
        (`Total Assets` +
         `Total Operating Expenses` +
         `Total Employees`)
]

panel[!is.finite(Efficiency_Raw), Efficiency_Raw := NA]

############################################################
# 3. NORMALISE WITHIN EACH YEAR (RELATIVE POSITIONING)
############################################################

panel[, Efficiency_Score :=
        (Efficiency_Raw - min(Efficiency_Raw, na.rm = TRUE)) /
        (max(Efficiency_Raw, na.rm = TRUE) -
         min(Efficiency_Raw, na.rm = TRUE)),
      by = Year]
############################################################
# 4. FILTER BANKS WITH INSUFFICIENT DATA
############################################################

panel <- panel[!is.na(Efficiency_Score) | Year >= 2023]

############################################################
# 5. ORDER BANKS FOR CLEAN LEGEND 
############################################################

last_year <- max(panel$Year, na.rm = TRUE)

bank_order <- panel[
  Year == last_year & !is.na(Efficiency_Score),
  .(Efficiency_Score = mean(Efficiency_Score)),
  by = Banks
][order(-Efficiency_Score), Banks]

panel[, Banks := factor(Banks, levels = bank_order)]

############################################################
# 6. COLORFUL EXECUTIVE-GRADE LINE PLOT
############################################################

p <- ggplot(
  panel,
  aes(
    x = Year,
    y = Efficiency_Score,
    color = Banks,
    group = Banks
  )
) +
  geom_line(
    linewidth = 1.3,
    alpha = 0.95
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_x_continuous(
    breaks = seq(min(panel$Year, na.rm = TRUE), max(panel$Year, na.rm = TRUE), by = 1),
    limits = c(min(panel$Year), max(panel$Year, na.rm = TRUE))
  ) +
  labs(
    title = "How Bank Efficiency Has Changed Over Time",
    subtitle =
      "Each line represents a bank. Higher lines indicate better use of assets, people, and operating costs.\nScores are shown relative to peers within each year.",
    y = "Relative Efficiency Score",
    x = NULL,
    color = "Bank"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

############################################################
# 7. SAVE OUTPUT
############################################################

ggsave(
  filename = "Data/CLEANED/yearly_efficiency_trend_exec.png",
  plot = p,
  width = 16,
  height = 9,
  dpi = 300
)

############################################################
# 8. EXPORT DATA (
############################################################

fwrite(
  panel[, .(Year, Banks, Efficiency_Score)],
  "Data/CLEANED/yearly_efficiency_trend_data.csv"
)

cat("\n✅ FILE 12 COMPLETED — YEARLY EFFICIENCY TREND (2024 INCLUDED)\n")
