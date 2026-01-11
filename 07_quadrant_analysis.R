############################################################
# FILE: 07_quadrant_analysis.R
# PURPOSE:
#   Efficiency–Growth positioning of banks
#   Client-grade visual with full labelling
############################################################

rm(list = ls())
gc()

library(data.table)
library(ggplot2)
library(ggrepel)

############################################################
# 1. LOAD INPUT DATA
############################################################

dea    <- fread("Data/CLEANED/dea_managerial_screening.csv")
growth <- fread("Data/CLEANED/growth_scores.csv")

############################################################
# 2. STANDARDISE & FILTER
############################################################

# Managerial efficiency axis
setnames(dea, "MAES", "Efficiency_Score")

# Use only regulator-acceptable banks
dea <- dea[Status == "Included"]

stopifnot("Growth_Score" %in% names(growth))

############################################################
# 3. MERGE DATA
############################################################

quad <- merge(
  dea[, .(Banks, Efficiency_Score)],
  growth[, .(Banks, Growth_Score)],
  by = "Banks",
  all = FALSE
)

stopifnot(
  !any(is.na(quad$Efficiency_Score)),
  !any(is.na(quad$Growth_Score))
)

############################################################
# 4. DEFINE PEER-RELATIVE CUTS
############################################################

eff_cut  <- median(quad$Efficiency_Score)
grow_cut <- median(quad$Growth_Score)

############################################################
# 5. QUADRANT ASSIGNMENT 
############################################################

quad[, Quadrant := fifelse(
  Efficiency_Score >= eff_cut & Growth_Score >= grow_cut,
  "High Efficiency / High Growth",
  fifelse(
    Efficiency_Score >= eff_cut & Growth_Score < grow_cut,
    "High Efficiency / Low Growth",
    fifelse(
      Efficiency_Score < eff_cut & Growth_Score >= grow_cut,
      "Low Efficiency / High Growth",
      "Low Efficiency / Low Growth"
    )
  )
)]

############################################################
# 6. SAVE CLASSIFICATION 
############################################################

fwrite(
  quad,
  "Data/CLEANED/quadrant_classification.csv"
)

############################################################
# 7.  QUADRANT VISUAL 
############################################################

p <- ggplot(
  quad,
  aes(x = Efficiency_Score, y = Growth_Score)
) +

  # Points (single muted colour)
  geom_point(
    size = 4,
    color = "#1F3A5F",
    alpha = 0.85
  ) +

  # Median reference lines (subtle)
  geom_vline(
    xintercept = eff_cut,
    linewidth = 0.3,
    color = "grey70"
  ) +
  geom_hline(
    yintercept = grow_cut,
    linewidth = 0.3,
    color = "grey70"
  ) +

  # Labels for ALL banks (repelled, clean)
  geom_text_repel(
    aes(label = Banks),
    size = 3.4,
    color = "black",
    box.padding = 0.35,
    point.padding = 0.25,
    segment.color = "grey70",
    segment.size = 0.25,
    max.overlaps = Inf
  ) +

  labs(
    title = "Efficiency–Growth Positioning of Banks",
    subtitle =
      "Each point represents a bank positioned by managerial efficiency (DEA-based MAES) on the x-axis\nand through-cycle financial growth on the y-axis. Median lines create peer-relative quadrants.",
    x = "Managerial Efficiency (MAES)",
    y = "Growth Score"
  ) +

  theme_minimal(base_size = 14) +

  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "grey70", linewidth = 0.3),
    axis.text = element_text(color = "grey25"),
    axis.title = element_text(size = 12)
  )

############################################################
# 8. SAVE & DISPLAY
############################################################

ggsave(
  filename = "Data/CLEANED/efficiency_growth_quadrant_exec.png",
  plot = p,
  width = 14,
  height = 8,
  dpi = 300
)

print(p)
cat("\n✅ FILE 07: CLIENT-GRADE QUADRANT ANALYSIS (ALL BANKS LABELLED)\n")
