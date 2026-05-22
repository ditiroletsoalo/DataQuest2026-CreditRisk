# DataQuest 2026: Institutional Credit Risk Engine & Calibrated Scorecard

[![Shiny App](https://img.shields.io/badge/ShinyApp-Live-emerald?style=flat&logo=r)](http://ditiroletsoalo.shinyapps.io/DataQuest26-CreditRisk/)
[![R-Version](https://img.shields.io/badge/R-%E2%89%A5%204.1.0-blue)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-gray.svg)](LICENSE)

An end-to-end regulatory-compliant Credit Risk Analytics platform built for the **DataQuest 2026 Hackathon hosted by First National Bank (FNB)**. This repository houses a fully reproducible data science pipeline that ingests the provided historical credit dataset, addresses severe class imbalances, optimizes retail underwriting boundaries, and maps predictive parameters directly into an operational credit scorecard under simulated banking governance constraints.

**🔗 Live Production App:** [ditiroletsoalo.shinyapps.io/DataQuest26-CreditRisk/](http://ditiroletsoalo.shinyapps.io/DataQuest26-CreditRisk/)

---

## 📌 Context & Framework Design

Deploying risk capital onto a bank's balance sheet requires balancing credit asset book growth with strict risk appetite boundaries. This platform simulates an automated credit underwriting engine calibrated to evaluate retail term loan facilities while aligning with international regulatory guidelines (Basel IV) and local credit legislation (such as Section 62 of the South African National Credit Act).

### The Modeling Constraint
While high-dimensional non-linear machine learning architectures (such as Gradient Boosting or Neural Networks) offer strong predictive capabilities, they face severe deployment restrictions in production-level lending due to strict transparency mandates regarding credit denials. 

To solve this friction point, this framework enhances the baseline logistic regression engine by:
1. **Isolating Non-Linear Risks:** Employing natural cubic splines to capture non-linear age effects and macroeconomic risk behaviors.
2. **Dynamic Risk-Appetite Calibration:** Utilizing **Youden's J-Statistic** to establish an optimal credit underwriting cut-off, matching the default-capture performance of cost-sensitive architectures while retaining 100% parameter transparency.

---

## 🛠️ Repository Structure & Core Modules

The framework is strictly decoupled and deterministically reproducible across three core analytical components:

```text
├── App.R                # Core Production Shiny Dashboard Engine
├── extract_plots.R      # Headless pipeline script for batch figure generation
├── loan_book.csv        # Provided historical credit data (train/test sets)
├── LICENSE              # Repository MIT License
└── README.md            # Repository documentation and deployment guide
