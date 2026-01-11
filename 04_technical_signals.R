############################################################
# FILE: 04_momentum_technical.R
############################################################

rm(list=ls()); gc()

library(data.table)
library(quantmod)
library(TTR)

dea <- fread("Data/CLEANED/dea_managerial_screening.csv")
banks <- dea[Status=="Included", Banks]

clean <- function(x) gsub("[^A-Z ]","",toupper(x))
banks_c <- clean(banks)

ticker_map <- c(
  "HDFC BANK LTD."             = "HDFCBANK.NS",
  "ICICI BANK LIMITED"         = "ICICIBANK.NS",
  "YES BANK LTD."              = "YESBANK.NS",
  "IDBI BANK LIMITED"          = "IDBI.NS",
  "STATE BANK OF INDIA"        = "SBIN.NS",
  "AXIS BANK LIMITED"          = "AXISBANK.NS",
  "FEDERAL BANK LTD"           = "FEDERALBNK.NS",
  "INDIAN BANK"                = "INDIANB.NS",
  "INDUSIND BANK LTD"          = "INDUSINDBK.NS",
  "BANK OF BARODA"             = "BANKBARODA.NS",
  "UCO BANK"                   = "UCOBANK.NS",
  "PUNJAB NATIONAL BANK"       = "PNB.NS",
  "BANK OF INDIA"              = "BANKINDIA.NS",
  "KOTAK MAHINDRA BANK LTD."   = "KOTAKBANK.NS",
  "UNION BANK OF INDIA"        = "UNIONBANK.NS"
)


names(ticker_map) <- clean(names(ticker_map))
tickers <- ticker_map[banks_c]

bn <- getSymbols("^NSEBANK",src="yahoo",
                 from="2014-01-01",auto.assign=FALSE)
bn_ret <- dailyReturn(Ad(bn))

compute <- function(b, t) {

  # -------------------------------
  # PRICE DOWNLOAD
  # -------------------------------
  px <- try(
    getSymbols(t, src = "yahoo",
               from = "2014-01-01",
               auto.assign = FALSE),
    silent = TRUE
  )

  if (inherits(px, "try-error")) {
    return(data.table(Banks = b, Momentum_Score = 0.5))
  }

  p <- na.omit(Ad(px))

  if (nrow(p) < 300) {
    return(data.table(Banks = b, Momentum_Score = 0.5))
  }

  # -------------------------------
  # CORE TECHNICAL SIGNALS
  # -------------------------------
  dma <- as.numeric(last(p) > last(SMA(p, 200)))

  rsi <- RSI(p)
  rsi_tail <- tail(rsi, 120)
  rslope <- coef(lm(rsi_tail ~ seq_along(rsi_tail)))[2]
  rsi_s <- ifelse(rslope > 0.02, 1,
           ifelse(rslope < -0.02, 0, 0.5))

  macd <- MACD(p)
  macd_flag <- macd$macd > macd$signal
  macd_s <- mean(tail(macd_flag, 120), na.rm = TRUE)
  macd_s <- ifelse(macd_s > 0.6, 1,
             ifelse(macd_s < 0.4, 0, 0.5))

  ret <- dailyReturn(p)
  rs <- mean(tail(ret, 120), na.rm = TRUE) >
        mean(tail(bn_ret, 120), na.rm = TRUE)

  # -------------------------------
  # BASE MOMENTUM 
  # -------------------------------
  mom <- mean(c(dma, rsi_s, macd_s, rs), na.rm = TRUE)

  # -------------------------------
  # LONG-TERM TREND FILTER
  # -------------------------------
  lt_trend <- 1   # default neutral

  px_3y <- try(tail(p, 750), silent = TRUE)

  if (!inherits(px_3y, "try-error") &&
      !is.null(px_3y) &&
      nrow(px_3y) >= 100) {

    first_px <- first(px_3y)
    last_px  <- last(px_3y)

    if (is.finite(first_px) && is.finite(last_px)) {
      lt_trend <- as.numeric(last_px > first_px)
    }
  }

if (identical(lt_trend, 0)) {
  mom <- mom - 0.20
}


  # -------------------------------
  # DOWNSIDE PENALTY
  # -------------------------------
  if (mean(tail(ret, 120), na.rm = TRUE) < 0) {
    mom <- mom - 0.25
  }

  # -------------------------------
  # FINAL SAFETY CLAMP
  # -------------------------------
  if (!is.finite(mom)) mom <- 0.5
  mom <- pmax(pmin(mom, 1), 0)

  return(data.table(Banks = b, Momentum_Score = mom))
}

mom <- rbindlist(Map(compute,banks,tickers))
fwrite(mom,"Data/CLEANED/momentum_scores.csv")

cat("\n✅ FILE 04 COMPLETED\n")
