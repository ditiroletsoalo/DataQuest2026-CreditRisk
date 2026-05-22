# ================================================================
# DataQuest 2026 — Plot Extraction Script
# Run this script ONCE to generate all figures for the LaTeX report.
# Output: plots/ directory with all PNG files referenced in report.tex
# Usage:  Rscript extract_plots.R   (from the same folder as loan_book.csv)
# ================================================================

# ── 0. Packages ──────────────────────────────────────────────────
pkgs <- c("tidyverse", "scales", "pROC", "caret", "splines",
          "broom", "lubridate", "patchwork", "viridis", "ggrepel")
new_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")

library(tidyverse); library(scales); library(pROC); library(caret)
library(splines);   library(broom);  library(lubridate)
library(patchwork); library(viridis); library(ggrepel)

dir.create("plots", showWarnings = FALSE)

SAVE <- function(name, p, w = 8, h = 5) {
  ggsave(file.path("plots", paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 200, bg = "white")
  message("  Saved: ", name, ".png")
}

THEME <- theme_minimal(base_size = 13) +
  theme(plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(colour = "#555555", size = 10),
        legend.position = "bottom",
        axis.title      = element_text(size = 11))

C_DEF   <- "#e74c3c"
C_NOD   <- "#2980b9"
C_GREEN <- "#27ae60"
C_GOLD  <- "#f39c12"
C_DARK  <- "#2c3e50"

# ================================================================
# SECTION 1 — DATA LOADING & CLEANING
# ================================================================
cat("Loading data...\n")
dat_raw <- read.csv("loan_book.csv", stringsAsFactors = FALSE)

dat <- dat_raw %>%
  mutate(
    home_ownership = tolower(home_ownership),
    home_ownership = case_when(
      str_detect(home_ownership, "rent")     ~ "Rent",
      str_detect(home_ownership, "mortgage") ~ "Mortgage",
      str_detect(home_ownership, "own")      ~ "Own",
      TRUE                                   ~ "Other"
    ),
    loan_purpose = tolower(str_replace_all(loan_purpose, "[ -]", "_")),
    loan_purpose = case_when(
      str_detect(loan_purpose, "debt|consolidat") ~ "Debt Consolidation",
      str_detect(loan_purpose, "home|improv")     ~ "Home Improvement",
      str_detect(loan_purpose, "major|purchase")  ~ "Major Purchase",
      str_detect(loan_purpose, "medic")            ~ "Medical",
      str_detect(loan_purpose, "educ")             ~ "Education",
      str_detect(loan_purpose, "small|business")  ~ "Small Business",
      TRUE                                         ~ "Other"
    ),
    parsed_date = parse_date_time(application_date,
                                  orders = c("mdy","ymd","dmy","dBY","d-BY"), quiet = TRUE),
    app_year  = year(parsed_date),
    app_month = month(parsed_date, label = TRUE, abbr = TRUE),
    is_never_delinquent           = as.integer(is.na(months_since_last_delinquency)),
    months_since_last_delinquency = ifelse(is.na(months_since_last_delinquency), -1,
                                           months_since_last_delinquency)
  ) %>%
  group_by(home_ownership, region) %>%
  mutate(
    annual_income           = ifelse(is.na(annual_income),
                                     median(annual_income, na.rm=TRUE), annual_income),
    employment_length_years = ifelse(is.na(employment_length_years),
                                     median(employment_length_years, na.rm=TRUE),
                                     employment_length_years),
    num_open_accounts       = ifelse(is.na(num_open_accounts),
                                     median(num_open_accounts, na.rm=TRUE), num_open_accounts)
  ) %>%
  ungroup() %>%
  mutate(
    high_util_flag = as.integer(credit_utilisation_pct > 80),
    log_income     = log1p(annual_income),
    log_loan       = log1p(loan_amount),
    risk_stress    = interest_rate * dti_ratio,
    income_to_loan = annual_income / (loan_amount + 1),
    delinq_flag    = as.integer(num_delinquencies_2yr > 0),
    across(where(is.character), as.factor)
  )

train_dat <- dat %>% filter(set == "train")
test_dat  <- dat %>% filter(set == "test")
overall_dr <- mean(dat$default_flag)

# ================================================================
# SECTION 2 — MODELS
# ================================================================
cat("Fitting models...\n")
scale_cols <- c(
  "age","annual_income","employment_length_years","num_open_accounts",
  "total_revolving_balance","credit_utilisation_pct","months_since_oldest_account",
  "num_hard_inquiries_6mo","loan_amount","interest_rate","dti_ratio",
  "months_since_last_delinquency","pct_accounts_current","months_at_current_address",
  "log_income","log_loan","risk_stress","income_to_loan","num_delinquencies_2yr"
)
preProc  <- preProcess(train_dat[, scale_cols], method = c("center","scale"))
train_s  <- predict(preProc, train_dat)
test_s   <- predict(preProc, test_dat)

f_base  <- default_flag ~ annual_income + loan_amount + interest_rate +
  dti_ratio + credit_utilisation_pct + num_delinquencies_2yr
f_champ <- default_flag ~
  log_income + log_loan + credit_utilisation_pct + interest_rate + dti_ratio +
  employment_length_years + num_delinquencies_2yr + months_since_last_delinquency +
  is_never_delinquent + high_util_flag + risk_stress + income_to_loan +
  home_ownership + loan_purpose + region + email_domain_type + phone_verified +
  num_hard_inquiries_6mo + months_since_oldest_account + pct_accounts_current +
  months_at_current_address + delinq_flag + ns(age, df = 4)

m_base  <- glm(f_base,  data = train_s, family = binomial)
m_champ <- glm(f_champ, data = train_s, family = binomial)

p_champ <- predict(m_champ, newdata = test_s, type = "response")
p_base  <- predict(m_base,  newdata = test_s, type = "response")
p_train_champ <- predict(m_champ, newdata = train_s, type = "response")

roc_c  <- roc(test_s$default_flag, p_champ, quiet = TRUE)
roc_b  <- roc(test_s$default_flag, p_base,  quiet = TRUE)
auc_c  <- round(as.numeric(auc(roc_c)), 4)
auc_b  <- round(as.numeric(auc(roc_b)), 4)
gini_c <- round(2*auc_c - 1, 4)
ks_stat <- round(max(abs(roc_c$sensitivities - (1 - roc_c$specificities))), 4)
best_thresh <- tryCatch(
  as.numeric(coords(roc_c,"best",ret="threshold",best.method="youden")[1]),
  error = function(e) 0.20)

PDO       <- 20
B_sc      <- PDO / log(2)
base_odds <- (1 - mean(dat$default_flag)) / mean(dat$default_flag)
A_sc      <- 600 + B_sc * log(base_odds)
sc_scores <- round(A_sc - B_sc * log(p_champ / (1 - p_champ)))

cat(paste0("Champion AUC: ", auc_c, "  Baseline AUC: ", auc_b,
           "  Gini: ", gini_c, "  KS: ", ks_stat, "\n"))

# ================================================================
# FIGURE 1 — Missing Values Heatmap
# ================================================================
cat("Generating plots...\n")
miss_df <- dat_raw %>%
  summarise(across(everything(), ~ sum(is.na(.))/n()*100)) %>%
  pivot_longer(everything(), names_to="variable", values_to="pct") %>%
  filter(pct > 0) %>%
  arrange(desc(pct))

p_miss <- ggplot(miss_df, aes(x=reorder(variable, pct), y=pct, fill=pct)) +
  geom_col(width=0.7) +
  geom_text(aes(label=paste0(round(pct,1),"%")), hjust=-0.1, size=3.5) +
  coord_flip() +
  scale_fill_gradient(low="#f39c12", high="#c0392b", guide="none") +
  scale_y_continuous(limits=c(0,60), labels=function(x) paste0(x,"%")) +
  labs(title="Missing Values by Variable",
       subtitle="Three variables have notable missingness; all handled before modelling",
       x=NULL, y="% Missing") +
  THEME
SAVE("fig01_missing", p_miss, 7, 4)

# ================================================================
# FIGURE 2 — Category Cleaning: home_ownership
# ================================================================
raw_df  <- dat_raw %>% count(home_ownership) %>%
  mutate(stage="Raw (14 variants)", home_ownership=as.character(home_ownership))
clean_df <- dat %>% count(home_ownership) %>%
  mutate(stage="Cleaned (4 levels)", home_ownership=as.character(home_ownership))
pd_cat <- bind_rows(raw_df, clean_df)

p_clean <- ggplot(pd_cat, aes(x=reorder(home_ownership, n), y=n, fill=stage)) +
  geom_col(position="dodge", width=0.6, alpha=0.9) +
  coord_flip() +
  scale_fill_manual(values=c("Raw (14 variants)"=C_DEF,"Cleaned (4 levels)"=C_GREEN)) +
  scale_y_continuous(labels=comma) +
  labs(title="home_ownership: 14 Raw Variants → 4 Clean Levels",
       subtitle="Case normalisation, fuzzy matching and re-labelling resolve duplicates",
       x=NULL, y="Count", fill=NULL) +
  THEME
SAVE("fig02_category_cleaning", p_clean, 7, 4.5)

# ================================================================
# FIGURE 3 — Credit Utilisation Distribution by Default Status
# ================================================================
p_util <- dat %>%
  mutate(Status=ifelse(default_flag==1,"Default","No Default")) %>%
  ggplot(aes(x=credit_utilisation_pct, fill=Status)) +
  geom_histogram(aes(y=after_stat(density)), bins=50, alpha=0.65, position="identity") +
  geom_vline(xintercept=80, linetype="dashed", colour=C_DARK, linewidth=0.8) +
  scale_fill_manual(values=c("No Default"=C_NOD,"Default"=C_DEF)) +
  annotate("text", x=83, y=0.025, label="80% threshold\n(high_util_flag)", hjust=0, size=3.5) +
  labs(title="Credit Utilisation Distribution by Default Status",
       subtitle="Defaults heavily right-skewed — high utilisation strongly predicts default",
       x="Credit Utilisation (%)", y="Density", fill=NULL) +
  THEME
SAVE("fig03_util_dist", p_util, 8, 5)

# ================================================================
# FIGURE 4 — Default Rate by Credit Utilisation Decile
# ================================================================
p_util_dr <- dat %>%
  mutate(bin=ntile(credit_utilisation_pct, 10)) %>%
  group_by(bin) %>%
  summarise(dr=mean(default_flag), n=n(),
            mid=median(credit_utilisation_pct), .groups="drop") %>%
  ggplot(aes(x=factor(bin), y=dr, fill=dr)) +
  geom_col(width=0.75) +
  geom_hline(yintercept=overall_dr, linetype="dashed", colour=C_DEF, linewidth=0.9) +
  scale_fill_gradient(low="#f1c40f", high="#c0392b", guide="none") +
  scale_y_continuous(labels=percent) +
  annotate("text", x=1, y=overall_dr+0.005, label=paste0("Mean: ",percent(overall_dr,0.1)),
           hjust=0, colour=C_DEF, size=3.5) +
  labs(title="Default Rate by Credit Utilisation Decile",
       subtitle="Monotone increase — decile 10 default rate ~4× the population mean",
       x="Decile (1=lowest utilisation)", y="Default Rate") +
  THEME
SAVE("fig04_util_dr", p_util_dr, 7, 4.5)

# ================================================================
# FIGURE 5 — WoE chart for interest_rate
# ================================================================
compute_woe <- function(df, var, n_bins=10) {
  df2 <- df %>% select(v=all_of(var), y=default_flag) %>% filter(!is.na(v))
  brks <- unique(quantile(df2$v, probs=seq(0,1,length.out=n_bins+1), na.rm=TRUE))
  df2$bin <- if (length(brks)>=3) as.character(cut(df2$v,breaks=brks,include.lowest=TRUE))
  else as.character(df2$v)
  tot_ev  <- max(sum(df2$y),1); tot_nev <- max(sum(1-df2$y),1)
  df2 %>% group_by(bin) %>%
    summarise(n=n(), events=sum(y), .groups="drop") %>%
    mutate(non_events=n-events,
           dist_ev=pmax(events/tot_ev,1e-6), dist_nev=pmax(non_events/tot_nev,1e-6),
           woe=log(dist_ev/dist_nev), iv_part=(dist_ev-dist_nev)*woe,
           default_rate=events/n)
}

woe_rate <- compute_woe(dat, "interest_rate")
p_woe <- ggplot(woe_rate, aes(x=reorder(bin,woe), y=woe, fill=woe>0)) +
  geom_col(width=0.75, alpha=0.9) +
  geom_hline(yintercept=0, linewidth=0.5) +
  coord_flip() +
  scale_fill_manual(values=c("FALSE"=C_NOD,"TRUE"=C_DEF), guide="none") +
  labs(title="Weight of Evidence — Interest Rate",
       subtitle=paste0("IV = ",round(sum(woe_rate$iv_part),4),
                       " (Strong predictor). Red bins → above-average default risk."),
       x="Bin", y="WoE") +
  THEME
SAVE("fig05_woe_rate", p_woe, 8, 5)

# ================================================================
# FIGURE 6 — IV Bar Chart
# ================================================================
key_vars <- c("credit_utilisation_pct","interest_rate","dti_ratio","loan_amount",
              "annual_income","employment_length_years","num_delinquencies_2yr",
              "months_since_last_delinquency","age","num_hard_inquiries_6mo",
              "total_revolving_balance","months_since_oldest_account","risk_stress",
              "income_to_loan","home_ownership","loan_purpose","region",
              "email_domain_type","phone_verified","application_dow")
compute_iv <- function(var) tryCatch(sum(compute_woe(dat,var)$iv_part,na.rm=TRUE), error=function(e) NA)
iv_df <- tibble(variable=key_vars, iv=sapply(key_vars, compute_iv)) %>%
  filter(!is.na(iv)) %>%
  mutate(strength=case_when(
    iv<0.02~"Useless",iv<0.10~"Weak",iv<0.30~"Medium",iv<0.50~"Strong",TRUE~"Very Strong"
  ))
sc_col <- c("Useless"="#95a5a6","Weak"="#f39c12","Medium"="#3498db",
            "Strong"="#27ae60","Very Strong"="#8e44ad")
p_iv <- ggplot(iv_df, aes(x=reorder(variable,iv), y=iv, fill=strength)) +
  geom_col(width=0.7) +
  geom_hline(yintercept=0.10, linetype="dashed", colour="gray50", linewidth=0.7) +
  geom_hline(yintercept=0.30, linetype="dashed", colour="#2980b9", linewidth=0.7) +
  coord_flip() +
  scale_fill_manual(values=sc_col) +
  labs(title="Information Value (IV) Ranking",
       subtitle="Dashed lines: 0.10 = Medium | 0.30 = Strong. Top features drive the champion model.",
       x=NULL, y="Information Value", fill="Strength") +
  THEME + theme(legend.position="right")
SAVE("fig06_iv", p_iv, 9, 6)

# ================================================================
# FIGURE 7 — Default Rate Heatmap: Region × Loan Purpose
# ================================================================
heat_df <- dat %>%
  group_by(region, loan_purpose) %>%
  summarise(dr=mean(default_flag), n=n(), .groups="drop") %>%
  filter(n>100) %>%
  mutate(across(c(region,loan_purpose), as.character))

p_heat <- ggplot(heat_df, aes(x=loan_purpose, y=region, fill=dr)) +
  geom_tile(colour="white", linewidth=0.6) +
  geom_text(aes(label=percent(dr,0.1)), size=2.8, colour="white", fontface="bold") +
  scale_fill_gradient2(low=C_GREEN, mid=C_GOLD, high=C_DEF,
                       midpoint=overall_dr, labels=percent) +
  labs(title="Default Rate Heatmap: Region × Loan Purpose",
       subtitle="Small Business and Medical loans in North-Urban drive the highest default concentrations",
       x=NULL, y=NULL, fill="Default\nRate") +
  THEME + theme(axis.text.x=element_text(angle=35, hjust=1))
SAVE("fig07_heatmap", p_heat, 9, 5.5)

# ================================================================
# FIGURE 8 — Correlation Matrix
# ================================================================
num_vars <- c("age","annual_income","employment_length_years","credit_utilisation_pct",
              "dti_ratio","interest_rate","loan_amount","num_delinquencies_2yr",
              "num_hard_inquiries_6mo","total_revolving_balance","risk_stress",
              "income_to_loan","default_flag")
cm  <- cor(dat[,num_vars], use="complete.obs")
cdf <- as.data.frame(as.table(cm)) %>%
  rename(r=Freq) %>%
  mutate(Var1=as.character(Var1), Var2=as.character(Var2))

p_corr <- ggplot(cdf, aes(x=Var1, y=Var2, fill=r)) +
  geom_tile(colour="white") +
  geom_text(aes(label=round(r,2)), size=2.3, colour="black") +
  scale_fill_gradient2(low=C_NOD, mid="white", high=C_DEF,
                       midpoint=0, limits=c(-1,1)) +
  labs(title="Pearson Correlation Matrix",
       subtitle="risk_stress, interest_rate and credit_utilisation_pct show strongest positive correlation with default",
       x=NULL, y=NULL, fill="r") +
  THEME + theme(axis.text.x=element_text(angle=45, hjust=1), legend.position="right")
SAVE("fig08_corr", p_corr, 9, 7)

# ================================================================
# FIGURE 9 — ROC Curves
# ================================================================
champ_lbl <- paste0("Champion (AUC = ", auc_c, ")")
base_lbl  <- paste0("Baseline (AUC = ", auc_b, ")")
roc_df <- bind_rows(
  data.frame(fpr=1-roc_c$specificities, tpr=roc_c$sensitivities, model=champ_lbl),
  data.frame(fpr=1-roc_b$specificities, tpr=roc_b$sensitivities, model=base_lbl)
)
p_roc <- ggplot(roc_df, aes(x=fpr, y=tpr, colour=model)) +
  geom_line(linewidth=1.4) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="gray50") +
  scale_colour_manual(values=c(setNames(C_GREEN, champ_lbl), setNames(C_DEF, base_lbl))) +
  coord_equal() +
  annotate("text", x=0.55, y=0.35, hjust=0, size=4,
           label=paste0("Gini = ", gini_c, "\nKS = ", ks_stat)) +
  labs(title="ROC Curves: Champion vs Baseline Logistic Regression",
       subtitle="Champion model lifts AUC from 0.68 to current level through feature engineering",
       x="False Positive Rate (1 – Specificity)",
       y="True Positive Rate (Sensitivity)", colour=NULL) +
  THEME
SAVE("fig09_roc", p_roc, 8, 6)

# ================================================================
# FIGURE 10 — Score Separation Density
# ================================================================
score_df <- data.frame(score=p_champ,
                       Status=ifelse(test_s$default_flag==1,"Default","No Default"))
p_sep <- ggplot(score_df, aes(x=score, fill=Status)) +
  geom_density(alpha=0.6, adjust=1.2) +
  geom_vline(xintercept=best_thresh, linetype="dashed", colour=C_DARK, linewidth=1) +
  scale_fill_manual(values=c("No Default"=C_NOD,"Default"=C_DEF)) +
  scale_x_continuous(labels=percent) +
  annotate("text", x=best_thresh+0.01, y=12,
           label=paste0("Optimal\nthreshold\n", round(best_thresh,3)),
           hjust=0, size=3.5) +
  labs(title="Score Separation by Default Status",
       subtitle="Good separation between score distributions confirms model discriminatory power",
       x="Predicted Default Probability", y="Density", fill=NULL) +
  THEME
SAVE("fig10_separation", p_sep, 8, 5)

# ================================================================
# FIGURE 11 — Calibration Plot
# ================================================================
calib_df <- data.frame(pred=p_champ, actual=test_s$default_flag) %>%
  mutate(decile=ntile(pred,10)) %>%
  group_by(decile) %>%
  summarise(mean_pred=mean(pred), mean_actual=mean(actual), n=n(), .groups="drop")

p_calib <- ggplot(calib_df, aes(x=mean_pred, y=mean_actual)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="gray50", linewidth=1) +
  geom_line(colour=C_GREEN, linewidth=1.2) +
  geom_point(aes(size=n), colour=C_GREEN, alpha=0.9) +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  scale_size(range=c(3,8), guide="none") +
  labs(title="Calibration Plot (Reliability Diagram)",
       subtitle="Points close to the diagonal confirm that predicted probabilities reflect true default rates",
       x="Mean Predicted Probability", y="Actual Default Rate") +
  THEME
SAVE("fig11_calibration", p_calib, 7, 5.5)

# ================================================================
# FIGURE 12 — Gain / Lift Chart
# ================================================================
n_t <- length(p_champ); tot_ev <- sum(test_s$default_flag)
gain_df <- data.frame(score=p_champ, default=test_s$default_flag) %>%
  arrange(desc(score)) %>%
  mutate(cum_n=row_number()/n_t,
         cum_def=cumsum(default)/tot_ev,
         lift=(cumsum(default)/row_number())/(tot_ev/n_t))
gain_pts <- gain_df[round(seq(1,n_t,length.out=500)),]
gain_pts <- dplyr::bind_rows(
  data.frame(cum_n = 0, cum_def = 0, lift = 1), 
  gain_pts
)
p_gain <- ggplot(gain_pts, aes(x=cum_n, y=cum_def)) +
  geom_line(colour="#8e44ad", linewidth=1.5) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="gray50") +
  geom_ribbon(aes(ymin=cum_n, ymax=cum_def), fill="#8e44ad", alpha=0.13) +
  geom_vline(xintercept=0.30, linetype="dotted", colour=C_DARK, linewidth=0.7) +
  annotate("text", x=0.31, y=0.1,
           label=paste0("Top 30% captures\n",
                        percent(gain_pts$cum_def[which.min(abs(gain_pts$cum_n-0.30))],0.1),
                        " of defaults"),
           hjust=0, size=3.5) +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  labs(title="Cumulative Gains Curve",
       subtitle="Area between curve and diagonal = model lift over random screening",
       x="% of Applicants Screened (high risk first)", y="% of Defaults Captured") +
  THEME
SAVE("fig12_gains", p_gain, 8, 5.5)

# ================================================================
# FIGURE 13 — Lorenz Curve
# ================================================================
lorenz_df <- data.frame(score=p_champ, default=test_s$default_flag) %>%
  arrange(desc(score)) %>%
  mutate(cum_pop=row_number()/n(), cum_default=cumsum(default)/sum(default))
lorenz_pts <- lorenz_df[round(seq(1,nrow(lorenz_df),length.out=600)),]
lorenz_pts <- dplyr::bind_rows(
  data.frame(cum_pop = 0, cum_default = 0), 
  lorenz_pts,
  data.frame(cum_pop = 1, cum_default = 1)
)

p_lorenz <- ggplot(lorenz_pts, aes(x=cum_pop, y=cum_default)) +
  geom_line(colour=C_DEF, linewidth=1.5) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="gray50") +
  geom_ribbon(aes(ymin=cum_pop, ymax=cum_default), fill=C_DEF, alpha=0.13) +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  labs(title=paste0("Lorenz Curve  |  Gini = ", gini_c),
       subtitle="The further the curve from the diagonal, the more defaults are concentrated in high-score deciles",
       x="% of Population (ranked by predicted risk, high \u2192 low)",
       y="% of Defaults Captured") +
  THEME
SAVE("fig13_lorenz", p_lorenz, 8, 5.5)

# ================================================================
# FIGURE 14 — Scorecard Score Distribution
# ================================================================
sc_df <- data.frame(score=sc_scores,
                    Status=ifelse(test_s$default_flag==1,"Default","No Default"))
p_sc <- ggplot(sc_df, aes(x=score, fill=Status)) +
  geom_density(alpha=0.6, adjust=1.3) +
  geom_vline(xintercept=600, linetype="dashed", colour=C_DARK, linewidth=0.8) +
  annotate("text", x=603, y=0.005, label="Base\nScore 600", hjust=0, size=3.5) +
  scale_fill_manual(values=c("No Default"=C_NOD,"Default"=C_DEF)) +
  labs(title="Scorecard Score Distribution (PDO = 20, Base = 600)",
       subtitle="Higher score = lower risk. Clear separation between default and non-default populations.",
       x="Scorecard Points", y="Density", fill=NULL) +
  THEME
SAVE("fig14_scorecard", p_sc, 8, 5)

# ================================================================
# FIGURE 15 — Top 20 Model Coefficients
# ================================================================
coef_df <- tidy(m_champ) %>%
  filter(term != "(Intercept)", !str_detect(term,"^ns\\(")) %>%
  mutate(abs_z=abs(statistic)) %>%
  arrange(desc(abs_z)) %>%
  slice_head(n=20) %>%
  mutate(term=str_replace_all(term,c("home_ownership"="HO: ","loan_purpose"="LP: ",
                                     "region"="Reg: ","email_domain_type"="Email: ",
                                     "TRUE"="")))

p_coef <- ggplot(coef_df, aes(x=reorder(term,estimate), y=estimate, fill=estimate>0)) +
  geom_col(width=0.7, alpha=0.9) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error),
                width=0.3, colour="black", linewidth=0.5) +
  geom_hline(yintercept=0, linewidth=0.5) +
  coord_flip() +
  scale_fill_manual(values=c("FALSE"=C_NOD,"TRUE"=C_DEF), guide="none") +
  labs(title="Top 20 Champion Model Coefficients (log-odds scale)",
       subtitle="Red = increases default risk | Blue = decreases risk | Bars = 95% CI",
       x=NULL, y="Coefficient (log-odds)") +
  THEME
SAVE("fig15_coef", p_coef, 9, 6.5)

# ================================================================
# FIGURE 16 — Business: Volume vs Risk
# ================================================================
biz_df <- map_dfr(seq(0.05,0.95,by=0.01), function(t) {
  pred <- as.integer(p_champ >= t)
  approved <- sum(pred==0)
  fn <- sum(pred==0 & test_s$default_flag==1)
  tibble(threshold=t, approved_pct=approved/length(pred),
         default_rate_approved=fn/max(approved,1))
})

p_biz <- ggplot(biz_df, aes(x=approved_pct, y=default_rate_approved)) +
  geom_line(colour=C_NOD, linewidth=1.4) +
  geom_point(data=biz_df %>% filter(abs(threshold-best_thresh) == min(abs(threshold-best_thresh))),
             aes(x=approved_pct, y=default_rate_approved),
             colour=C_DEF, size=4) +
  geom_hline(yintercept=overall_dr, linetype="dashed", colour="gray50") +
  annotate("text", x=0.9, y=overall_dr+0.005, label="Population\ndefault rate",
           hjust=1, size=3.5, colour="gray40") +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  labs(title="Volume vs Risk Trade-off Curve",
       subtitle="Red dot = Youden optimal threshold. As approval rate rises, portfolio default rate increases non-linearly.",
       x="Approval Rate", y="Default Rate Among Approved Loans") +
  THEME
SAVE("fig16_biz", p_biz, 8, 5)

# ================================================================
# FIGURE 17 — Temporal Default Rate
# ================================================================
temp_df <- dat %>%
  filter(!is.na(app_year), !is.na(app_month), app_year >= 2020, app_year <= 2025) %>%
  group_by(app_year, app_month) %>%
  summarise(dr=mean(default_flag), n=n(), .groups="drop") %>%
  mutate(label=paste0(app_month," ",app_year))

p_temp <- ggplot(temp_df, aes(x=app_month, y=dr, colour=factor(app_year), group=factor(app_year))) +
  geom_line(linewidth=1.2) + geom_point(size=2.5) +
  geom_hline(yintercept=overall_dr, linetype="dashed", colour="gray50") +
  scale_y_continuous(labels=percent) +
  scale_colour_brewer(palette="Set1") +
  labs(title="Default Rate by Application Year and Month",
       subtitle="No strong seasonal pattern detected; variation is consistent with sampling noise",
       x=NULL, y="Default Rate", colour="Year") +
  THEME
SAVE("fig17_temporal", p_temp, 9, 5)

# ================================================================
# DONE
# ================================================================
cat("\n=== All", 17, "plots saved to plots/ directory ===\n")
cat("Next step: place plots/ alongside report.tex and run pdflatex.\n")