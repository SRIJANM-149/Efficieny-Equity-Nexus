############################################################
# FILE: 05_fusion_ranking.R
############################################################

rm(list=ls()); gc()
library(data.table)

dea <- fread("Data/CLEANED/dea_managerial_screening.csv")
growth <- fread("Data/CLEANED/growth_scores.csv")
mom <- fread("Data/CLEANED/momentum_scores.csv")

dea <- dea[Status=="Included"]
setnames(dea,"MAES","Efficiency_Score")

fusion <- merge(dea[,.(Banks,Sector,Efficiency_Score,Efficiency_Tier)],
                growth,by="Banks")
fusion <- merge(fusion,mom,by="Banks")

###########################################################
# ROBUST NORMALIZATION 
###########################################################

rank_norm <- function(x) {
  frank(x, ties.method = "average") / length(x)
}

fusion[, `:=`(
  Eff_N = rank_norm(Efficiency_Score),
  Gro_N = rank_norm(Growth_Score),
  Mom_N = rank_norm(Momentum_Score)
)]






fusion[, Fusion_Score :=
         0.50*Eff_N + 0.40*Gro_N + 0.10*Mom_N]

###########################################################
# FRANCHISE PRIORITY TILT
###########################################################

fusion[, Fusion_Score :=
         Fusion_Score *
         fifelse(
           Banks %in% c("HDFC BANK LTD.",
                        "ICICI BANK LIMITED",
                        "STATE BANK OF INDIA"),
           1.05,
           1.00
         )]

###########################################################
# REGULATORY OVERHANG CAP 
###########################################################

fusion[Banks == "YES BANK LTD.",
       Fusion_Score := Fusion_Score * 0.70]

fusion[, Rank := frank(-Fusion_Score,ties.method="dense")]

fusion[, Verdict :=
         fifelse(Rank<=3,"Strong Buy",
         fifelse(Rank<=7,"Accumulate",
         fifelse(Rank<=12,"Hold","Avoid")))]

###########################################################
# SCREENING VIEW 
###########################################################

fusion[, Screening_View :=
  fifelse(
    Banks %in% c("HDFC BANK LTD.", "ICICI BANK LIMITED", "KOTAK MAHINDRA BANK LTD."),
    "Core Franchise Compounder",
  fifelse(
    Banks %in% c("FEDERAL BANK LTD", "INDUSIND BANK LTD", "AXIS BANK LIMITED"),
    "Growth-at-Reasonable-Quality",
  fifelse(
    Banks %in% c("STATE BANK OF INDIA"),
    "Systemic / Policy Bank",
  fifelse(
    Banks %in% c("YES BANK LTD.", "INDIAN BANK", "UNION BANK OF INDIA"),
    "Turnaround / Re-rating Candidate",
    "Low Conviction / Avoid"
  ))))
]

###########################################################
# DISPERSION GATE 
###########################################################

fusion[, Dispersion :=
         apply(.SD,1,sd),
         .SDcols=c("Eff_N","Gro_N","Mom_N")]

fusion[Dispersion > 0.35,
       Fusion_Score := Fusion_Score * 0.85]

fusion[Banks=="STATE BANK OF INDIA",
       Verdict:=ifelse(Rank<=7,"Accumulate","Hold")]
fwrite(
  fusion[order(Rank)],
  "Data/CLEANED/fusion_scores.csv")
fwrite(fusion[order(Rank)],
       "Data/CLEANED/fusion_ranking.csv")

cat("\n✅ FILE 05 COMPLETED\n")
