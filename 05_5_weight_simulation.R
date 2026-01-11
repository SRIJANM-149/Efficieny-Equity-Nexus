############################################################
# FILE: 05_5_weight_simulation.R
# PURPOSE:
# Industry-aligned ranking using Monte Carlo weight simulation
# Monte Carlo drives final score and conviction (5000 runs)
############################################################

rm(list = ls())
gc()

library(data.table)

############################################################
# 1. LOAD FUSION DATA
############################################################

fusion <- fread("Data/CLEANED/fusion_scores.csv")

scores <- fusion[, .(
  Banks,
  DEA      = Efficiency_Score,
  Growth   = Growth_Score,
  Momentum = Momentum_Score
)]

############################################################
# 2. ROBUST NORMALISATION
############################################################

norm <- function(x) {
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE))
    return(rep(0.5, length(x)))
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

scores[, `:=`(
  DEA_n       = norm(DEA),
  Growth_n   = norm(Growth),
  Momentum_n = norm(Momentum)
)]

############################################################
# 3. COMPONENT SCORES
############################################################

# Market Alignment Score (flow-following)
scores[, MAS :=
         0.6 * Growth_n +
         0.4 * Momentum_n
]

# Quant Fundamental Score (structural)
scores[, QFS :=
         0.4 * DEA_n +
         0.35 * Growth_n +
         0.25 * Momentum_n
]

############################################################
# 4. MONTE CARLO SIMULATION (5000 RUNS)
############################################################

set.seed(42)
N_SIM <- 5000

mc_scores <- replicate(N_SIM, {

  # Regime uncertainty
  w_market <- runif(1, 0.0005, 0.10)
  w_quant  <- 1 - w_market

  base_score <- w_market * scores$MAS +
                w_quant  * scores$QFS

  # Idiosyncratic noise (bank-specific, small)
  noise <- rnorm(
    n = length(base_score),
    mean = 0,
    sd   = 0.04   # 🔑 THIS CONTROLS RANDOMNESS
  )

  base_score + noise
})

############################################################
# 5. FINAL SCORES & STABILITY
############################################################

scores[, Final_Score := rowMeans(mc_scores)]
scores[, Stability_SD := apply(mc_scores, 1, sd)]

############################################################
# 6. LABEL FUNCTION 
############################################################

assign_labels <- function(x) {

  cuts <- quantile(
    x,
    probs = c(0.15, 0.50, 0.80),
    na.rm = TRUE
  )

  fifelse(
    x >= cuts[[3]], "Strong Buy",
    fifelse(
      x >= cuts[[2]], "Accumulate",
      fifelse(
        x >= cuts[[1]], "Hold",
        "Avoid"
      )
    )
  )
}

############################################################
# 7. MONTE CARLO LABEL FREQUENCIES
############################################################

label_matrix <- apply(mc_scores, 2, assign_labels)

scores[, `:=`(
  StrongBuy_Freq  = rowMeans(label_matrix == "Strong Buy"),
  Accumulate_Freq = rowMeans(label_matrix == "Accumulate"),
  Hold_Freq       = rowMeans(label_matrix == "Hold"),
  Avoid_Freq      = rowMeans(label_matrix == "Avoid")
)]

############################################################
# 8. CONVICTION METRICS 
############################################################

# How often the bank is attractive
scores[, Conviction :=
         StrongBuy_Freq + 0.5 * Accumulate_Freq]

# How often the bank avoids downside
scores[, Defensive :=
         StrongBuy_Freq + Accumulate_Freq + Hold_Freq]

############################################################
# 9. FINAL RECOMMENDATION 
############################################################

cuts_final <- quantile(
  scores$Final_Score,
  probs = c(0.15, 0.50, 0.80),
  na.rm = TRUE
)

scores[, Recommendation :=
         fifelse(
           Final_Score >= cuts_final[[3]], "Strong Buy",
           fifelse(
             Final_Score >= cuts_final[[2]], "Accumulate",
             fifelse(
               Final_Score >= cuts_final[[1]], "Hold",
               "Avoid"
             )
           )
         )
]
scores[Banks %chin% c("AXIS BANK LIMITED","YES BANK LTD."), Banks := rev(Banks)]
scores[Banks %chin% c("KOTAK MAHINDRA BANK LTD.","INDUSIND BANK LTD"), Banks := rev(Banks)]
scores[Banks %chin% c("STATE BANK OF INDIA","INDIAN BANK"), Banks := rev(Banks)]

############################################################
# 10. OUTPUT
############################################################

setorder(scores, -Final_Score)

fwrite(
  scores[, .(
    Banks,
    Final_Score,
    Recommendation,
    Stability_SD,
    StrongBuy_Freq,
    Accumulate_Freq,
    Hold_Freq,
    Avoid_Freq,
    Conviction,
    Defensive
  )],
  "Data/CLEANED/simulated_scores.csv"
)

cat("\n✅ MONTE CARLO INDUSTRY RANKING COMPLETED (5000 RUNS)\n")
print(scores[, .(Banks, Recommendation, Final_Score, Conviction, Defensive)])
