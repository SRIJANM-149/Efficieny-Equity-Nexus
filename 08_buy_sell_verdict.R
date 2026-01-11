############################################################
# FILE: 08_buy_sell_verdict.R
# PURPOSE:
#   Convert fusion fundamentals and Monte Carlo conviction
#   into final client-ready investment verdicts
############################################################

rm(list = ls())
gc()

library(data.table)

############################################################
# 1. LOAD INPUT DATA
############################################################

fusion <- fread("Data/CLEANED/fusion_scores.csv")
mc     <- fread("Data/CLEANED/simulated_scores.csv")

############################################################
# 2. MERGE DATA
############################################################

final <- merge(
  fusion,
  mc,
  by = "Banks",
  all = FALSE
)

stopifnot(
  !any(is.na(final$Fusion_Score)),
  !any(is.na(final$Conviction)),
  !any(is.na(final$Avoid_Freq))
)

############################################################
# 3. DEFINE PEER-RELATIVE CUTS 
############################################################

conv_cut <- quantile(final$Conviction,
                     probs = c(0.25, 0.50, 0.75),
                     na.rm = TRUE)

fusion_cut <- quantile(final$Fusion_Score,
                       probs = 0.60,
                       na.rm = TRUE)

############################################################
# 4. ASSIGN FINAL VERDICT
############################################################

final[, Final_Verdict :=
  fifelse(
    Conviction >= conv_cut[[3]] & Fusion_Score >= fusion_cut,
    "Strong Buy",
    fifelse(
      Conviction >= conv_cut[[2]],
      "Accumulate",
      fifelse(
        Conviction >= conv_cut[[1]],
        "Hold",
        "Avoid"
      )
    )
  )
]

############################################################
# 5. HUMAN-READABLE EXPLANATION 
############################################################

final[, Explanation :=
  fifelse(
    Final_Verdict == "Strong Buy",
    "High conviction across Monte Carlo simulations with strong underlying fundamentals.",
    fifelse(
      Final_Verdict == "Accumulate",
      "Positive conviction across most simulated regimes, supported by solid efficiency and growth.",
      fifelse(
        Final_Verdict == "Hold",
        "Mixed signals across simulations; fundamentals are stable but conviction is not decisive.",
        "Low conviction across simulations; downside risk dominates under multiple scenarios."
      )
    )
  )
]

############################################################
# 6. ORDER & OUTPUT
############################################################

setorder(final, -Conviction, -Fusion_Score)

fwrite(
  final[, .(
    Banks,
    Sector,
    Fusion_Score,
    Conviction,
    StrongBuy_Freq,
    Accumulate_Freq,
    Hold_Freq,
    Avoid_Freq,
    Final_Verdict,
    Explanation
  )],
  "Data/CLEANED/Final_Banking_Verdict_Report.csv"
)

cat("\n✅ FILE 08 COMPLETED — CLIENT-GRADE BUY / SELL VERDICTS GENERATED\n")
print(final[, .(Banks, Final_Verdict, Conviction)])
