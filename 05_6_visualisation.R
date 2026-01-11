############################################################
# FILE: 05_6_visualisation.R
# PURPOSE:
#   Executive-grade visualisation of Monte Carlo
#   recommendation robustness
############################################################

rm(list = ls())
gc()

library(data.table)
library(ggplot2)
library(scales)

############################################################
# 1. LOAD ROBUSTNESS RESULTS
############################################################

robust <- fread("Data/CLEANED/simulated_scores.csv")

############################################################
# 2. RESHAPE TO LONG FORMAT
############################################################

robust_long <- melt(
  robust,
  id.vars = "Banks",
  measure.vars = c(
    "StrongBuy_Freq",
    "Accumulate_Freq",
    "Hold_Freq",
    "Avoid_Freq"
  ),
  variable.name = "Recommendation",
  value.name = "Probability"
)

# Clean recommendation labels
robust_long[, Recommendation :=
  factor(
    Recommendation,
    levels = c(
      "StrongBuy_Freq",
      "Accumulate_Freq",
      "Hold_Freq",
      "Avoid_Freq"
    ),
    labels = c(
      "Strong Buy",
      "Accumulate",
      "Hold",
      "Avoid"
    )
  )
]

############################################################
# 3. ORDER BANKS BY CONVICTION THEN STRONG BUY
############################################################

bank_order <- robust[
  order(-Conviction, -StrongBuy_Freq),
  Banks
]

robust_long[, Banks := factor(Banks, levels = bank_order)]

############################################################
# 4. EXECUTIVE COLOR PALETTE 
############################################################

ib_palette <- c(
  "Strong Buy" = "#0B6623",  # Institutional green
  "Accumulate" = "#C9A227",  # Muted gold
  "Hold"       = "#9E9E9E",  # Neutral grey
  "Avoid"      = "#8B0000"   # Dark red
)

############################################################
# 5. STACKED BAR 
############################################################

p <- ggplot(
  robust_long,
  aes(
    x = Banks,
    y = Probability,
    fill = Recommendation
  )
) +

  geom_bar(
    stat = "identity",
    width = 0.78,
    color = "white",
    linewidth = 0.25
  ) +

  scale_fill_manual(values = ib_palette) +

  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +

  labs(
    title = "Monte Carlo Recommendation Robustness",
    subtitle =
      "Distribution of investment recommendations across 5,000 simulated market regimes\nHigher Strong Buy and Accumulate share indicates stronger conviction stability",
    x = NULL,
    y = "Simulation Probability"
  ) +

  theme_minimal(base_size = 13) +

  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    axis.text.y = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

############################################################
# 6. SAVE OUTPUT
############################################################

ggsave(
  filename = "Data/CLEANED/recommendation_robustness_exec.png",
  plot = p,
  width = 13,
  height = 6.5,
  dpi = 300
)

print(p)
cat("✅ Executive-grade robustness visualisation generated\n")
