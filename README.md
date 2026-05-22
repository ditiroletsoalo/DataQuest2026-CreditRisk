# DataQuest 2026: Championship Credit Risk App & Interpretable Models

[![Shiny App](https://img.shields.io/badge/ShinyApp-Live-emerald?style=flat&logo=r)](http://ditiroletsoalo.shinyapps.io/DataQuest26-CreditRisk/)
[![R-Version](https://img.shields.io/badge/R-%E2%89%A5%204.1.0-blue)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-gray.svg)](LICENSE)

An end-to-end regulatory-compliant Credit Risk Analytics platform built for **DataQuest 2026**. This project implements a fully reproducible data science pipeline that builds, calibrates, explains, and deploys an interpretable credit scoring model under real-world banking constraints.

**🔗 Live Application:** [ditiroletsoalo.shinyapps.io/DataQuest26-CreditRisk/](http://ditiroletsoalo.shinyapps.io/DataQuest26-CreditRisk/)

---

## 📌 Project Overview

 Lenders require high predictive performance but face strict regulatory mandates (e.g., Basel IV, Equal Credit Opportunity Act) that forbid uninterpretable "black-box" models (like Random Forests or Neural Networks). 

This project solves this trade-off by taking a raw historical loan book, performing rigorous Exploratory Data Analysis (EDA), and building an advanced **Champion Logistic Regression Model**. Through domain-informed feature engineering and independent threshold optimization via **Youden’s J-statistic**, the final Champion model achieves a **0.7977 AUC** (up from the 0.68 baseline), matching the performance of non-linear machine learning architectures while retaining 100% mathematical interpretability.

### Key Pipeline Milestones
1. **Interactive Exploratory Data Analysis (EDA):** Dynamic tracking of class imbalance (15.4% overall default rate) and deep feature-risk profiling.
2. **Advanced Feature Engineering:** Natural cubic splines for non-linear age effects, log transformations, credit utilization flags, and interaction terms.
3. **Multi-Model Calibration:** Rigorous evaluation of Baseline, Champion (Unweighted), and Cost-Sensitive (Weighted) models using independent Youden thresholds.
4. **Credit Scorecard Generation:** Conversion of log-odds coefficients into a transparent 600-point retail scorecard with a Points-to-Double-Odds (PDO) of 20.
5. **Adverse Action & Explainability:** Automated generation of regulatory Top-3 Denial Reasons notices backed by an interactive stateful **AI Credit Analyst Chatbot**.

---

## 🛠️ Repository Structure & Core Modules

The framework is strictly decoupled and deterministically reproducible across three key core files:

```text
├── App.R                # Core Interactive Shiny Application Engine
├── extract_plots.R      # Automated pipeline script to batch-generate figures
├── main (3).tex         # Comprehensive LaTeX research report source
├── loan_book.csv        # Historical credit data (split into train/test sets)
└── README.md            # Repository and reproducibility guide
