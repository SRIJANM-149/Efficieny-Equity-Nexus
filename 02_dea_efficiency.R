############################################################
# FILE: 02_dea_managerial_screening.R
# PURPOSE:
# Franchise-adjusted managerial DEA with persistent asset quality
############################################################

rm(list=ls()); gc()

library(data.table)
library(Benchmarking)

# ----------------------------------------------------------
# 1. LOAD DATA
# ----------------------------------------------------------

data <- fread("Data/CLEANED/bank_panel.csv")
setnames(data, trimws(names(data)))

stopifnot("Sector" %in% names(data))

# ----------------------------------------------------------
# 2. PERSISTENT ASSET QUALITY (3Y MEDIAN)
# ----------------------------------------------------------

data[, NetNPA_3Y :=
       frollmedian(`Net NPA`, 3, align="right", na.rm=TRUE),
     by = Banks]

data[is.na(NetNPA_3Y), NetNPA_3Y := `Net NPA`]

# ----------------------------------------------------------
# 3. INPUTS (PURE COST + RISK)
# ----------------------------------------------------------

X <- as.matrix(data[, .(
  OpEx   = `Total Operating Expenses`,
  Emp    = `Total Employees`,
  IntExp = `Total Interest Expended`,
  NetNPA = NetNPA_3Y
)])

# ----------------------------------------------------------
# 4. OUTPUTS (INTERMEDIATION + FRANCHISE)
# ----------------------------------------------------------

Y <- as.matrix(data[, .(
  Adv    = `Total of Advances`,
  Depos = `Total of Deposits`,
  NII    = `Net Interest Income`,
  OProf = `Operating Profit`
)])

# ----------------------------------------------------------
# 5. DEA ESTIMATION
# ----------------------------------------------------------

bcc <- dea(X, Y, RTS="vrs", ORIENTATION="in")
ccr <- dea(X, Y, RTS="crs", ORIENTATION="in")

data[, `:=`(
  BCC = bcc$eff,
  CCR = ccr$eff,
  ScaleEff = ccr$eff / bcc$eff
)]

# ----------------------------------------------------------
# 6. BANK LEVEL AGGREGATION (ROBUST)
# ----------------------------------------------------------

bank <- data[, .(
  BCC = median(BCC, na.rm=TRUE),
  ScaleEff = median(ScaleEff, na.rm=TRUE),
  Sector = Sector[1]
), by=Banks]

# ----------------------------------------------------------
# 7. MANAGERIAL EFFICIENCY SCORE
# ----------------------------------------------------------

alpha <- 0.30

bank[, MAES :=
       BCC * (1 - alpha * pmax(0, 1 - ScaleEff))]

# ----------------------------------------------------------
# 8. FRANCHISE FORGIVENESS (D-SIB)
# ----------------------------------------------------------

dsib <- c("STATE BANK OF INDIA",
          "HDFC BANK LTD.",
          "ICICI BANK LIMITED")

bank[, MAES :=
       pmin(1, MAES + fifelse(Banks %in% dsib, 0.05, 0))]

###########################################################
# 9. CONTROLLED EXCLUSION (TOP 15 SURVIVE)
###########################################################

# Rank by MAES (descending)
bank[, Eff_Rank := frank(-MAES, ties.method="dense")]

# Keep exactly top 15 banks
bank[, Status :=
       fifelse(Eff_Rank <= 15,
               "Included",
               "Excluded")]

###########################################################
# MARKET DISCIPLINE FLOOR
###########################################################

bank[, MAES :=
       MAES * fifelse(
         grepl("PUBLIC|BANK OF|PUNJAB|INDIAN|UCO|CANARA", Banks),
         0.90,
         1.00
       )]

###########################################################
# EFFICIENCY BANDS (SAFE NAMING)
###########################################################

qt <- quantile(bank$MAES,
               probs = c(0.25, 0.50, 0.75),
               na.rm = TRUE)

bank[, Efficiency_Tier :=
       fifelse(MAES >= qt[[3]], "Tier A (Strong)",
       fifelse(MAES >= qt[[2]], "Tier B (Adequate)",
       fifelse(MAES >= qt[[1]], "Tier C (Weak)",
               "Tier D (Structurally Inefficient)")))]

bank[, Status :=
       ifelse(Efficiency_Tier=="Tier D (Structurally Inefficient)",
              "Excluded","Included")]

setorder(bank, -MAES)

fwrite(bank, "Data/CLEANED/dea_managerial_screening.csv")

cat("\n✅ FILE 02 COMPLETED\n")
