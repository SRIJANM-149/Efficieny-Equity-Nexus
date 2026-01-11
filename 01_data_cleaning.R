############################################################
# FILE: 01_data_cleaning.R
# PURPOSE: Convert DBIE-style Excel into bank-year panel
############################################################

rm(list = ls())
gc()

library(readxl)
library(data.table)
library(zoo)

# ----------------------------------------------------------
# 1. READ EXCEL RAW
# ----------------------------------------------------------
raw <- as.data.table(
  read_excel(
    "Data/DATASET/MASTER.xlsx",
    col_names = FALSE
  )
)

# ----------------------------------------------------------
# 2. HARD-CODE HEADER ROW (KNOWN FROM YOUR FILE)
# ----------------------------------------------------------
HEADER_ROW <- 8

col_names <- as.character(unlist(raw[HEADER_ROW]))
data <- raw[(HEADER_ROW + 1):.N]

# Remove completely empty rows
data <- data[rowSums(is.na(data)) < ncol(data)]

setnames(data, col_names)

# ----------------------------------------------------------
# 3. STANDARDIZE COLUMNS BY POSITION
# ----------------------------------------------------------
setnames(
  data,
  old = names(data),
  new = c(
    "Year",
    "Banks",
    "Total Interest Income",
    "Non Interest Income",
    "Total Income",
    "Total Interest Expended",
    "Total Operating Expenses",
    "Net Interest Income",
    "Operating Profit",
    "Total of Deposits",
    "Total of Advances",
    "Fixed Assets",
    "Total Assets",
    "Net NPA",
    "Net Profit",
    "Total Employees",
    "Sector"
  )
)

# ----------------------------------------------------------
# 4. 🔑 CRITICAL FIXES (BOTH REQUIRED)
# ----------------------------------------------------------

# Fill Year blocks downward
data[, Year := zoo::na.locf(Year, na.rm = FALSE)]

# Fill Bank names downward
data[, Banks := zoo::na.locf(Banks, na.rm = FALSE)]

# ----------------------------------------------------------
# 5. DROP ONLY TRULY INVALID ROWS
# ----------------------------------------------------------
data <- data[
  !is.na(Year) &
  !is.na(Banks)
]

# ----------------------------------------------------------
# 6. BASIC CLEANING (NO NORMALIZATION)
# ----------------------------------------------------------
data[, Banks := trimws(as.character(Banks))]
data[, Sector := trimws(as.character(Sector))]
data[, Year := as.integer(Year)]

# ----------------------------------------------------------
# 7. NUMERIC CONVERSION
# ----------------------------------------------------------
num_cols <- setdiff(names(data), c("Year","Banks","Sector"))
data[, (num_cols) := lapply(.SD, function(x) as.numeric(as.character(x))),
     .SDcols = num_cols]

# ----------------------------------------------------------
# 8. COVID REGIME
# ----------------------------------------------------------
data[, Regime :=
       fifelse(Year <= 2019, "Pre_COVID",
       fifelse(Year <= 2021, "COVID", "Post_COVID"))]

# ----------------------------------------------------------
# 9. PANEL ORDERING
# ----------------------------------------------------------
setorder(data, Banks, Year)
setkey(data, Banks, Year)

# ----------------------------------------------------------
# 10. SAVE
# ----------------------------------------------------------
dir.create("Data/CLEANED", showWarnings = FALSE)
fwrite(data, "Data/CLEANED/bank_panel.csv")

cat("\n✅ File 01 completed successfully\n")
cat("Rows:", nrow(data),
    "| Banks:", length(unique(data$Banks)), "\n")
