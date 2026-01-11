# Efficiency–Equity Nexus  
### A Data-Driven Framework for Ranking, Risk Profiling, and Capital Allocation in Indian Banking

## Overview

This repository contains the complete codebase and analytical pipeline for the research project **“Efficiency–Equity Nexus”**, which develops an integrated framework for evaluating Indian banks by jointly modeling:

- **Operational efficiency** (DEA-based managerial efficiency),
- **Financial growth** (robust, regime-aware CAGR),
- **Market validation** (price-based momentum indicators),
- **Robustness under uncertainty** (Monte Carlo simulation),
- **Portfolio allocation** (risk-profiled and mean–variance optimization).

The framework is designed as a **decision-support system**, not merely a ranking exercise, and bridges accounting-based analysis with market-driven validation.

---

## Data Sources

All data used in this project are obtained from **publicly available sources**:

- **Reserve Bank of India (RBI)**  
  Annual bank-level financial statements, balance sheets, income data, asset quality, and operational variables.

- **Yahoo Finance**  
  Daily adjusted equity prices for listed Indian banks and the NSE Bank Index.

No proprietary or confidential data are used.


---

## Methodological Pipeline

The analysis proceeds in the following sequence:

1. **Data Cleaning & Panel Construction**  
   Conversion of RBI-style Excel data into a consistent bank–year panel.

2. **Operational Efficiency (DEA)**  
   Input-oriented DEA with persistent asset quality, managerial efficiency adjustment, and controlled exclusion.

3. **Financial Growth Measurement**  
   Robust CAGR computed across multiple variables and macro regimes, with stability penalties and quality multipliers.

4. **Market Momentum Indicators**  
   Long-term trend, RSI slope, MACD persistence, and relative strength versus the banking index.

5. **Score Fusion & Ranking**  
   Normalized integration of efficiency, growth, and momentum into a composite score.

6. **Monte Carlo Weight Simulation**  
   5,000 simulations to assess ranking robustness, conviction, and downside protection.

7. **Regime & Structural Analysis**  
   COVID stress testing, PSU vs private comparison, ownership transition case study (IDBI).

8. **Portfolio Construction**  
   Risk-profiled allocation and Markowitz mean–variance optimization.

---

## Key Outputs

- Stable bank rankings with **conviction metrics**
- Efficiency–growth quadrant diagnostics
- COVID resilience classification
- Monte Carlo recommendation robustness
- Risk-profiled capital allocation tables
- Optimal portfolio weights (Max Sharpe)

All figures are stored in the `RESULTS/` directory.

---

## Reproducibility

To reproduce the full analysis:

1. Place raw RBI Excel files in `Data/DATASET/`
2. Ensure an active internet connection for Yahoo Finance price downloads
3. Run scripts sequentially from `01_` to `13_`
4. Outputs will be generated automatically in `Data/CLEANED/` and `RESULTS/`


---

## Intended Use

This repository is intended for:
- Academic research
- Thesis or dissertation work
- Institutional-style equity research
- Policy and banking sector analysis

It is **not** intended as real-time trading or investment advice.

---

## Author

**Srijan Mishra**
SVNIT, Surat

---

## License

This project is shared for academic and research purposes.  
Please cite appropriately if used in derivative work.


