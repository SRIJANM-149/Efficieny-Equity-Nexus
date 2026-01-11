############################################################
# FILE: 13_risk_profiled_allocation.R
# PURPOSE:
#   Capital allocation (%) across Top 3 banks
#   for Low / Medium / High risk investors
#   (clear, stable, client-facing format)
############################################################

rm(list = ls())
gc()

library(data.table)

############################################################
# 1. LOAD INPUT DATA
############################################################

fusion <- fread("Data/CLEANED/fusion_scores.csv")

stopifnot("Fusion_Score" %in% names(fusion))

############################################################
# 2. IDENTIFY TOP 3 BANKS (STABLE ORDER)
############################################################
# Ranked strictly by composite Fusion Score

top3 <- fusion[
  order(-Fusion_Score),
  .(Banks, Fusion_Score)
][1:3]

############################################################
# 3. DEFINE RISK-PROFILE ALLOCATION LOGIC
############################################################
# Interpretation:
# • Low Risk → concentrate more in top-ranked bank
# • Medium Risk → balanced allocation
# • High Risk → higher tilt toward lower-ranked upside

allocation <- data.table(
  Banks = top3$Banks,

  `Low Risk (%)` = c(50, 30, 20),
  `Medium Risk (%)` = c(40, 35, 25),
  `High Risk (%)` = c(30, 35, 35)
)

############################################################
# 4. SANITY CHECK (EACH PROFILE SUMS TO 100%)
############################################################

stopifnot(
  sum(allocation$`Low Risk (%)`)    == 100,
  sum(allocation$`Medium Risk (%)`) == 100,
  sum(allocation$`High Risk (%)`)   == 100
)

############################################################
# 5. ATTACH CONTEXT 
############################################################

allocation <- merge(
  allocation,
  top3,
  by = "Banks",
  all.x = TRUE
)

setcolorder(
  allocation,
  c(
    "Banks",
    "Fusion_Score",
    "Low Risk (%)",
    "Medium Risk (%)",
    "High Risk (%)"
  )
)

############################################################
# 6. OUTPUT — WIDE, CLIENT-READY TABLE
############################################################

fwrite(
  allocation,
  "Data/CLEANED/risk_profiled_capital_allocation.csv"
)

cat("\n✅ FILE 13 COMPLETED — CLIENT-READY CAPITAL ALLOCATION GENERATED\n")
print(allocation)
