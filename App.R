# ================================================================
# DataQuest 2026: CHAMPIONSHIP CREDIT RISK APP — FINAL EDITION
# Author: Ditiro Letsoalo
# ================================================================
# V3 ADDITIONS:
#  • ADVERSE ACTION TAB — Coefficient attribution waterfall,
#    regulatory Top-3 Denial Reasons notice, AI letter generator
#  • AI CREDIT ANALYST TAB — Stateful chat powered by Mistral AI
#    (via ellmer), pre-built prompts, full markdown rendering
# ================================================================

if (file.exists("secrets.R")) source("secrets.R")

# ── 0. PACKAGES ──────────────────────────────────────────────────
pkgs <- c("shiny", "shinydashboard", "tidyverse", "scales", "plotly",
          "pROC", "caret", "splines", "DT", "broom", "lubridate",
          "shinycssloaders", "shinyjs", "ellmer", "commonmark", "promises", "future")
new_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs) > 0) install.packages(new_pkgs, repos = "https://cloud.r-project.org")

library(shiny);          library(shinydashboard)
library(tidyverse);      library(scales);         library(plotly)
library(pROC);           library(caret);          library(splines)
library(DT);             library(broom);          library(lubridate)
library(shinycssloaders); library(shinyjs)
library(ellmer);         library(commonmark)

# ================================================================
# SECTION 1: DATA LOADING & PREPROCESSING
# ================================================================
message("=== DataQuest 2026 Final: Loading & Processing Data ===")
dat_raw <- read.csv("loan_book.csv", stringsAsFactors = FALSE)

continuous_vars <- c(
  "Age"                         = "age",
  "Annual Income"               = "annual_income",
  "Employment Length (Years)"   = "employment_length_years",
  "Number of Open Accounts"     = "num_open_accounts",
  "Total Revolving Balance"     = "total_revolving_balance",
  "Credit Utilisation (%)"      = "credit_utilisation_pct",
  "Months Since Oldest Account" = "months_since_oldest_account",
  "Loan Amount"                 = "loan_amount",
  "Interest Rate"               = "interest_rate",
  "DTI Ratio"                   = "dti_ratio",
  "Months Since Last Delinq."   = "months_since_last_delinquency",
  "Pct Accounts Current"        = "pct_accounts_current",
  "Months at Current Address"   = "months_at_current_address",
  "Num Delinquencies (2yr)"     = "num_delinquencies_2yr",
  "Num Hard Inquiries (6mo)"    = "num_hard_inquiries_6mo"
)
continuous_vars_eng <- c(
  continuous_vars,
  "Risk Stress Index"    = "risk_stress",
  "Income to Loan Ratio" = "income_to_loan",
  "Log Income"           = "log_income",
  "Log Loan"             = "log_loan"
)
key_vars <- c(
  "credit_utilisation_pct","interest_rate","dti_ratio","loan_amount",
  "annual_income","employment_length_years","num_delinquencies_2yr",
  "months_since_last_delinquency","age","num_hard_inquiries_6mo",
  "total_revolving_balance","months_since_oldest_account","risk_stress",
  "income_to_loan","home_ownership","loan_purpose","region",
  "email_domain_type","phone_verified","application_dow"
)

miss_summary <- dat_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to="variable", values_to="missing") %>%
  mutate(total=nrow(dat_raw), pct_miss=missing/total*100) %>%
  filter(missing>0) %>% arrange(desc(pct_miss))

dat <- dat_raw %>%
  mutate(
    home_ownership_raw = home_ownership,
    home_ownership     = tolower(home_ownership),
    home_ownership     = case_when(
      str_detect(home_ownership,"rent")     ~ "Rent",
      str_detect(home_ownership,"mortgage") ~ "Mortgage",
      str_detect(home_ownership,"own")      ~ "Own",
      TRUE                                  ~ "Other"),
    loan_purpose_raw = loan_purpose,
    loan_purpose     = tolower(str_replace_all(loan_purpose,"[ -]","_")),
    loan_purpose     = case_when(
      str_detect(loan_purpose,"debt|consolidat") ~ "debt_consolidation",
      str_detect(loan_purpose,"home|improv")     ~ "home_improvement",
      str_detect(loan_purpose,"major|purchase")  ~ "major_purchase",
      str_detect(loan_purpose,"medic")            ~ "medical",
      str_detect(loan_purpose,"educ")             ~ "education",
      str_detect(loan_purpose,"small|business")  ~ "small_business",
      TRUE                                        ~ "other"),
    parsed_date = parse_date_time(application_date,
                                  orders=c("mdy","ymd","dmy","dBY","d-BY"),quiet=TRUE),
    app_year  = year(parsed_date),
    app_month = month(parsed_date, label=TRUE, abbr=TRUE),
    is_never_delinquent           = as.integer(is.na(months_since_last_delinquency)),
    months_since_last_delinquency = ifelse(is.na(months_since_last_delinquency),-1,
                                           months_since_last_delinquency)
  ) %>%
  group_by(home_ownership, region) %>%
  mutate(
    annual_income           = ifelse(is.na(annual_income),
                                     median(annual_income,na.rm=TRUE),annual_income),
    employment_length_years = ifelse(is.na(employment_length_years),
                                     median(employment_length_years,na.rm=TRUE),
                                     employment_length_years),
    num_open_accounts       = ifelse(is.na(num_open_accounts),
                                     median(num_open_accounts,na.rm=TRUE),num_open_accounts)
  ) %>%
  ungroup() %>%
  mutate(
    high_util_flag = as.integer(credit_utilisation_pct>80),
    log_income     = log1p(annual_income),
    log_loan       = log1p(loan_amount),
    risk_stress    = interest_rate*dti_ratio,
    income_to_loan = annual_income/(loan_amount+1),
    delinq_flag    = as.integer(num_delinquencies_2yr>0),
    across(where(is.character), as.factor)
  )

message("  Cleaning complete: ", nrow(dat), " rows.")

# ================================================================
# SECTION 2: WoE / IV
# ================================================================
compute_woe_iv <- function(df, var, target="default_flag", n_bins=10) {
  df_sub <- df %>% select(v=all_of(var), y=all_of(target)) %>% filter(!is.na(v))
  if (is.numeric(df_sub$v) && n_distinct(df_sub$v)>10) {
    brks <- unique(quantile(df_sub$v, probs=seq(0,1,length.out=n_bins+1), na.rm=TRUE))
    df_sub$bin_label <- if(length(brks)>=3)
      as.character(cut(df_sub$v,breaks=brks,include.lowest=TRUE)) else as.character(df_sub$v)
  } else { df_sub$bin_label <- as.character(df_sub$v) }
  tot_ev <- max(sum(df_sub$y),1); tot_nev <- max(sum(1-df_sub$y),1)
  woe_df <- df_sub %>%
    group_by(bin_label) %>%
    summarise(n=n(), events=sum(y), non_events=n()-sum(y), .groups="drop") %>%
    mutate(dist_ev=pmax(events/tot_ev,1e-6), dist_nev=pmax(non_events/tot_nev,1e-6),
           woe=log(dist_ev/dist_nev), iv_part=(dist_ev-dist_nev)*woe,
           default_rate=events/n)
  list(woe_df=woe_df, iv=round(sum(woe_df$iv_part,na.rm=TRUE),4))
}

message("  Computing IV scores...")
iv_summary <- tibble(variable=key_vars,
                     iv=map_dbl(key_vars,~tryCatch(compute_woe_iv(dat,.x)$iv,error=function(e)NA_real_))) %>%
  mutate(strength=case_when(
    iv<0.02~"Useless (< 0.02)", iv<0.10~"Weak (0.02\u20130.10)",
    iv<0.30~"Medium (0.10\u20130.30)", iv<0.50~"Strong (0.30\u20130.50)",
    TRUE~"Very Strong (> 0.50)")) %>%
  arrange(desc(iv))

# ================================================================
# SECTION 3: MODEL FITTING
# ================================================================
message("  Fitting models...")
train_dat <- dat %>% filter(set=="train")
test_dat  <- dat %>% filter(set=="test")

scale_cols <- c(
  "age","annual_income","employment_length_years","num_open_accounts",
  "total_revolving_balance","credit_utilisation_pct","months_since_oldest_account",
  "num_hard_inquiries_6mo","loan_amount","interest_rate","dti_ratio",
  "months_since_last_delinquency","pct_accounts_current","months_at_current_address",
  "log_income","log_loan","risk_stress","income_to_loan","num_delinquencies_2yr"
)
preProc <- preProcess(train_dat[,scale_cols], method=c("center","scale"))
train_s <- predict(preProc, train_dat)
test_s  <- predict(preProc, test_dat)

f_base  <- default_flag ~ annual_income+loan_amount+interest_rate+
  dti_ratio+credit_utilisation_pct+num_delinquencies_2yr
f_champ <- default_flag ~
  log_income+log_loan+credit_utilisation_pct+interest_rate+dti_ratio+
  employment_length_years+num_delinquencies_2yr+months_since_last_delinquency+
  is_never_delinquent+high_util_flag+risk_stress+income_to_loan+
  home_ownership+loan_purpose+region+email_domain_type+phone_verified+
  num_hard_inquiries_6mo+months_since_oldest_account+pct_accounts_current+
  months_at_current_address+delinq_flag+ns(age,df=4)

m_base  <- glm(f_base,  data=train_s, family=binomial)
m_champ <- glm(f_champ, data=train_s, family=binomial)
# Cost-sensitive weighted model (pre-fitted, used by weight slider)
default_rate_train <- mean(train_s$default_flag)
inv_freq_weight    <- (1 - default_rate_train) / default_rate_train
weights_full       <- ifelse(train_s$default_flag == 1, inv_freq_weight, 1)
m_weighted         <- glm(f_champ, data=train_s, family=binomial, weights=weights_full)
p_weighted_test    <- predict(m_weighted, newdata=test_s, type="response")
roc_w              <- roc(test_s$default_flag, p_weighted_test, quiet=TRUE)
auc_w              <- round(as.numeric(auc(roc_w)), 4)
thresh_weighted    <- tryCatch(
  as.numeric(coords(roc_w,"best",ret="threshold",best.method="youden")[1]),
  error=function(e) 0.51)

p_champ <- predict(m_champ, newdata=test_s,  type="response")
p_base  <- predict(m_base,  newdata=test_s,  type="response")
p_train_champ <- predict(m_champ, newdata=train_s, type="response")

roc_c   <- roc(test_s$default_flag, p_champ, quiet=TRUE)
roc_b   <- roc(test_s$default_flag, p_base,  quiet=TRUE)
auc_c   <- round(as.numeric(auc(roc_c)),4)
auc_b   <- round(as.numeric(auc(roc_b)),4)
gini_c  <- round(2*auc_c-1,4)
gini_b  <- round(2*auc_b-1,4)
ks_stat <- round(max(abs(roc_c$sensitivities-(1-roc_c$specificities))),4)
best_thresh <- tryCatch(
  as.numeric(coords(roc_c,"best",ret="threshold",best.method="youden")[1]),
  error=function(e)0.20)

champ_label <- paste0("Champion (AUC = ",auc_c,")")
base_label  <- paste0("Baseline (AUC = ",auc_b,")")
roc_df <- bind_rows(
  data.frame(fpr=1-roc_c$specificities, tpr=roc_c$sensitivities, model=champ_label),
  data.frame(fpr=1-roc_b$specificities, tpr=roc_b$sensitivities, model=base_label))
model_colors <- c("#27ae60","#e74c3c")
names(model_colors) <- c(champ_label, base_label)

score_dist_df <- data.frame(score=p_champ,
                            Status=ifelse(test_s$default_flag==1,"Default","No Default"))
coef_df <- tidy(m_champ) %>%
  filter(term!="(Intercept)", !str_detect(term,"^ns\\(")) %>%
  mutate(abs_z=abs(statistic)) %>% arrange(desc(abs_z)) %>% slice_head(n=20)

lorenz_raw <- data.frame(score=p_champ, default=test_s$default_flag) %>%
  arrange(desc(score)) %>%
  mutate(cum_pop=row_number()/n(), cum_default=cumsum(default)/sum(default))
lorenz_pts <- lorenz_raw[round(seq(1,nrow(lorenz_raw),length.out=600)),]
lorenz_pts <- bind_rows(data.frame(cum_pop=0,cum_default=0), lorenz_pts,
                        data.frame(cum_pop=1,cum_default=1))

biz_df <- map_dfr(seq(0.05,0.95,by=0.01), function(t) {
  pred <- as.integer(p_champ>=t)
  tp<-sum(pred==1&test_s$default_flag==1); fp<-sum(pred==1&test_s$default_flag==0)
  fn<-sum(pred==0&test_s$default_flag==1); tn<-sum(pred==0&test_s$default_flag==0)
  approved <- sum(pred==0)
  tibble(threshold=t, tp=tp,fp=fp,fn=fn,tn=tn,
         approved=approved, approved_pct=approved/length(pred),
         default_rate_approved=fn/max(approved,1),
         precision=tp/max(tp+fp,1), recall=tp/max(tp+fn,1),
         f1=2*tp/max(2*tp+fp+fn,1), accuracy=(tp+tn)/length(pred))
})

# ================================================================
# SECTION 4: EXTRA ANALYTICS
# ================================================================
message("  Calibration, gain, decile, scorecard, PSI...")
calib_df <- data.frame(pred=p_champ,actual=test_s$default_flag) %>%
  mutate(decile=ntile(pred,10)) %>%
  group_by(decile) %>%
  summarise(mean_pred=mean(pred),mean_actual=mean(actual),n=n(),.groups="drop")

n_test <- length(p_champ); total_events <- sum(test_s$default_flag)
gain_df <- data.frame(score=p_champ, default=test_s$default_flag) %>%
  arrange(desc(score)) %>%
  mutate(cum_n=row_number()/n_test, cum_defaults=cumsum(default)/total_events,
         lift=(cumsum(default)/row_number())/(total_events/n_test))
gain_pts <- gain_df[round(seq(1,nrow(gain_df),length.out=500)),]
gain_pts <- bind_rows(data.frame(cum_n=0,cum_defaults=0,lift=1), gain_pts)

decile_df <- data.frame(score=p_champ, default=test_s$default_flag) %>%
  arrange(desc(score)) %>%
  mutate(decile=ceiling(row_number()/(n()/10)), decile=pmin(decile,10)) %>%
  group_by(decile) %>%
  summarise(n=n(),defaults=sum(default),non_defaults=n()-sum(default),
            default_rate=mean(default),avg_score=mean(score),.groups="drop") %>%
  mutate(cum_defaults=cumsum(defaults), cum_non_defaults=cumsum(non_defaults),
         cum_pct_def=cum_defaults/sum(defaults), cum_pct_nondef=cum_non_defaults/sum(non_defaults),
         ks=round(abs(cum_pct_def-cum_pct_nondef),4),
         lift=round(default_rate/mean(test_s$default_flag),2),
         cum_capture=round(cum_pct_def*100,1))

PDO       <- 20; B_sc <- PDO/log(2)
base_odds <- (1-mean(dat$default_flag))/mean(dat$default_flag)
A_sc      <- 600+B_sc*log(base_odds)
scorecard_scores       <- round(A_sc-B_sc*log(p_champ/(1-p_champ)))
scorecard_scores_train <- round(A_sc-B_sc*log(p_train_champ/(1-p_train_champ)))

score_dist_sc <- data.frame(score=scorecard_scores,
                            Status=ifelse(test_s$default_flag==1,"Default","No Default"))
score_lookup <- data.frame(score=seq(400,850,by=25)) %>%
  mutate(log_odds=(A_sc-score)/B_sc, prob=round(1/(1+exp(-log_odds))*100,1))

# scorecard_coef_df <- tidy(m_champ) %>%
#   filter(term!="(Intercept)",!str_detect(term,"^ns\\(")) %>%
#   mutate(points_per_sd=round(-B_sc*estimate,1),
#          direction=ifelse(estimate>0,"Increases Risk \u25b2","Decreases Risk \u25bc")) %>%
#   arrange(desc(abs(points_per_sd))) %>% slice_head(n=15) %>%
#   select(Feature=term,`\u03b2 (log-odds)`=estimate,`Points per SD`=points_per_sd,Direction=direction)

scorecard_coef_df <- tidy(m_champ) %>%
  filter(term != "(Intercept)", !str_detect(term, "^ns\\(")) %>%
  mutate(
    points_per_sd = round(-B_sc * estimate, 1),
    direction = ifelse(estimate > 0, "Increases Risk \u25b2", "Decreases Risk \u25bc")
  ) %>%
  arrange(desc(abs(points_per_sd))) %>% 
  slice_head(n = 15) %>%
  # Use column = value syntax to rename without the backtick/Unicode issue
  select(
    Feature = term,
    `β (log-odds)` = estimate,  # You can paste the actual symbol directly here!
    `Points per SD` = points_per_sd,
    Direction = direction
  )

psi_breaks <- quantile(scorecard_scores_train, probs=seq(0,1,by=0.1), na.rm=TRUE)
psi_breaks[1] <- -Inf; psi_breaks[11] <- Inf
train_bins <- table(cut(scorecard_scores_train,breaks=psi_breaks))
test_bins  <- table(cut(scorecard_scores,       breaks=psi_breaks))
train_pct  <- prop.table(train_bins); test_pct <- prop.table(test_bins)
psi_val    <- round(sum((test_pct-train_pct)*log((test_pct+1e-6)/(train_pct+1e-6))),4)
psi_status <- case_when(psi_val<0.10~"Stable",psi_val<0.25~"Moderate Shift",TRUE~"Major Shift")
psi_df <- data.frame(Bin=names(train_bins),
                     Train_Pct=as.numeric(train_pct)*100, Test_Pct=as.numeric(test_pct)*100,
                     PSI_Part=round((as.numeric(test_pct)-as.numeric(train_pct))*
                                      log((as.numeric(test_pct)+1e-6)/(as.numeric(train_pct)+1e-6)),5))

med_profile <- test_s %>%
  summarise(across(all_of(scale_cols), median, na.rm=TRUE)) %>%
  mutate(
    home_ownership    = names(sort(table(test_s$home_ownership),   decreasing=TRUE))[1],
    loan_purpose      = names(sort(table(test_s$loan_purpose),     decreasing=TRUE))[1],
    region            = names(sort(table(test_s$region),           decreasing=TRUE))[1],
    email_domain_type = names(sort(table(test_s$email_domain_type),decreasing=TRUE))[1],
    phone_verified    = names(sort(table(test_s$phone_verified),   decreasing=TRUE))[1],
    high_util_flag=0L, is_never_delinquent=0L, delinq_flag=0L,
    default_flag=0L, set="test")
for (col in names(med_profile)) {
  if (col %in% names(test_s) && is.factor(test_s[[col]]))
    med_profile[[col]] <- factor(med_profile[[col]], levels=levels(test_s[[col]]))
}

# ================================================================
# SECTION 5: ADVERSE ACTION SETUP
# ================================================================
message("  Setting up Adverse Action engine...")

FEATURE_LABELS <- c(
  "credit_utilisation_pct"        = "Credit Utilisation Ratio",
  "interest_rate"                 = "Loan Interest Rate",
  "dti_ratio"                     = "Debt-to-Income Ratio",
  "num_delinquencies_2yr"         = "Delinquencies (Past 2 Years)",
  "risk_stress"                   = "Interest \u00d7 Debt Stress Index",
  "log_loan"                      = "Loan Amount",
  "months_since_last_delinquency" = "Recency of Last Delinquency",
  "num_hard_inquiries_6mo"        = "Hard Credit Inquiries (6 months)",
  "high_util_flag"                = "Credit Utilisation Exceeds 80%",
  "delinq_flag"                   = "Recent Delinquency Flag",
  "income_to_loan"                = "Income-to-Loan Ratio",
  "log_income"                    = "Income Level",
  "employment_length_years"       = "Employment Tenure",
  "months_since_oldest_account"   = "Age of Oldest Credit Account",
  "pct_accounts_current"          = "Accounts in Good Standing (%)",
  "months_at_current_address"     = "Residential Stability",
  "is_never_delinquent"           = "Clean Delinquency History",
  "num_open_accounts"             = "Number of Open Accounts",
  "total_revolving_balance"       = "Total Revolving Balance",
  "home_ownership"                = "Housing Type",
  "loan_purpose"                  = "Loan Purpose Category",
  "region"                        = "Geographic Region",
  "email_domain_type"             = "Email Domain Type",
  "phone_verified"                = "Phone Verification Status",
  "age"                           = "Applicant Age Profile"
)

REASON_DETAIL <- c(
  "Credit Utilisation Ratio"        = "Credit card balances are too high relative to credit limits.",
  "Loan Interest Rate"              = "The interest rate indicates this loan carries elevated repayment risk.",
  "Debt-to-Income Ratio"            = "Monthly debt obligations are too high relative to declared income.",
  "Delinquencies (Past 2 Years)"    = "One or more late/missed payments appear in the past 24 months.",
  "Interest \u00d7 Debt Stress Index" = "The combination of high interest rate and debt load creates excessive repayment stress.",
  "Loan Amount"                     = "The requested loan amount is too large relative to the applicant's financial profile.",
  "Recency of Last Delinquency"     = "A delinquency occurred too recently to demonstrate restored creditworthiness.",
  "Hard Credit Inquiries (6 months)" = "An unusual number of recent credit applications signals elevated risk.",
  "Credit Utilisation Exceeds 80%"  = "Utilisation above 80% is a high-risk threshold; immediate balance reduction is advised.",
  "Recent Delinquency Flag"         = "The credit file contains evidence of recent delinquent payment behaviour.",
  "Income-to-Loan Ratio"            = "Income level is insufficient to service the requested loan amount.",
  "Income Level"                    = "Declared income does not meet minimum serviceability requirements.",
  "Employment Tenure"               = "Insufficient employment history to confirm stable income and repayment capacity.",
  "Age of Oldest Credit Account"    = "Limited credit history depth; the oldest account is relatively recent.",
  "Accounts in Good Standing (%)"   = "A significant proportion of credit accounts are not currently in good standing.",
  "Residential Stability"           = "Short tenure at current address reduces confidence in applicant stability.",
  "Clean Delinquency History"       = "The delinquency record does not satisfy clean credit history requirements.",
  "Number of Open Accounts"         = "The number of open accounts falls outside acceptable parameters.",
  "Total Revolving Balance"         = "Total revolving credit balance exceeds acceptable portfolio risk limits.",
  "Housing Type"                    = "Housing status is associated with elevated default risk in this market segment.",
  "Loan Purpose Category"           = "The stated loan purpose is statistically associated with higher default rates.",
  "Geographic Region"               = "Applications from this region carry above-average portfolio default rates.",
  "Email Domain Type"               = "The provided email domain type is associated with elevated application risk.",
  "Phone Verification Status"       = "Phone verification status did not meet minimum identity-confirmation standards.",
  "Applicant Age Profile"           = "The applicant's age profile is outside the standard low-risk band."
)

# ── Coefficient Attribution Engine ──────────────────────────────
get_contributions <- function(app_row, model) {
  tryCatch({
    mf    <- model.frame(formula(model), data=app_row, na.action=na.pass)
    mm    <- model.matrix(formula(model), data=mf)
    coefs <- coef(model)
    common_terms <- intersect(names(coefs), colnames(mm))
    
    df <- data.frame(
      term         = common_terms,
      contribution = coefs[common_terms] * as.numeric(mm[1, common_terms]),
      stringsAsFactors = FALSE
    ) %>% filter(term != "(Intercept)")
    
    # Group natural spline age basis functions → single "age" row
    spline_rows  <- df %>% filter(str_detect(term,"^ns\\("))
    other_rows   <- df %>% filter(!str_detect(term,"^ns\\("))
    if (nrow(spline_rows) > 0) {
      age_row <- data.frame(term="age",
                            contribution=sum(spline_rows$contribution),
                            stringsAsFactors=FALSE)
      df <- bind_rows(other_rows, age_row)
    } else { df <- other_rows }
    
    # Group factor dummy columns → single row per base variable
    df <- df %>%
      mutate(group=case_when(
        str_detect(term,"^home_ownership")    ~ "home_ownership",
        str_detect(term,"^loan_purpose")      ~ "loan_purpose",
        str_detect(term,"^region")            ~ "region",
        str_detect(term,"^email_domain")      ~ "email_domain_type",
        str_detect(term,"phone_verified")     ~ "phone_verified",
        TRUE                                  ~ term)) %>%
      group_by(group) %>%
      summarise(contribution=sum(contribution), .groups="drop") %>%
      mutate(
        label = ifelse(group %in% names(FEATURE_LABELS),
                       FEATURE_LABELS[group], group),
        reason = ifelse(label %in% names(REASON_DETAIL),
                        REASON_DETAIL[label],
                        "This factor contributed to the model's risk assessment."),
        direction = ifelse(contribution > 0, "risk", "protective")
      ) %>%
      arrange(desc(contribution))
    return(df)
  }, error = function(e) NULL)
}

format_raw_value <- function(group, raw_row) {
  tryCatch({
    val <- switch(as.character(group),
                  "credit_utilisation_pct"        = paste0(round(raw_row$credit_utilisation_pct, 1), "%"),
                  "interest_rate"                 = paste0(round(raw_row$interest_rate, 2), "%"),
                  "dti_ratio"                     = paste0(round(raw_row$dti_ratio, 2)),
                  "num_delinquencies_2yr"         = as.character(as.integer(raw_row$num_delinquencies_2yr)),
                  "risk_stress"                   = paste0(round(raw_row$interest_rate * raw_row$dti_ratio, 2)),
                  "log_loan"                      = paste0("R ", comma(round(raw_row$loan_amount))),
                  "months_since_last_delinquency" = ifelse(raw_row$months_since_last_delinquency == -1,
                                                           "Never delinquent",
                                                           paste0(raw_row$months_since_last_delinquency, " months ago")),
                  "num_hard_inquiries_6mo"        = as.character(as.integer(raw_row$num_hard_inquiries_6mo)),
                  "high_util_flag"                = ifelse(raw_row$credit_utilisation_pct > 80,
                                                           paste0("Yes — ", round(raw_row$credit_utilisation_pct,1), "%"),
                                                           paste0("No — ", round(raw_row$credit_utilisation_pct,1), "%")),
                  "delinq_flag"                   = ifelse(raw_row$num_delinquencies_2yr > 0, "Yes", "No"),
                  "income_to_loan"                = paste0(round(raw_row$annual_income / (raw_row$loan_amount + 1), 2), "x"),
                  "log_income"                    = paste0("R ", comma(round(raw_row$annual_income))),
                  "employment_length_years"       = paste0(round(raw_row$employment_length_years, 1), " yrs"),
                  "months_since_oldest_account"   = paste0(raw_row$months_since_oldest_account, " months"),
                  "pct_accounts_current"          = paste0(round(raw_row$pct_accounts_current, 1), "%"),
                  "months_at_current_address"     = paste0(raw_row$months_at_current_address, " months"),
                  "is_never_delinquent"           = ifelse(raw_row$is_never_delinquent == 1, "Clean history", "Has delinquency"),
                  "num_open_accounts"             = as.character(as.integer(raw_row$num_open_accounts)),
                  "total_revolving_balance"       = paste0("R ", comma(round(raw_row$total_revolving_balance))),
                  "home_ownership"                = as.character(raw_row$home_ownership),
                  "loan_purpose"                  = as.character(raw_row$loan_purpose),
                  "region"                        = as.character(raw_row$region),
                  "email_domain_type"             = as.character(raw_row$email_domain_type),
                  "phone_verified"                = as.character(raw_row$phone_verified),
                  "age"                           = paste0(raw_row$age, " yrs"),
                  "\u2014"
    )
    as.character(val[1])
  }, error = function(e) "\u2014")
}

# ── Applicant Selector ────────────────────────────────────────────
test_display_df <- data.frame(
  idx      = seq_along(p_champ),
  prob     = p_champ,
  score    = scorecard_scores,
  actual   = test_s$default_flag,
  decision = ifelse(p_champ >= best_thresh, "DECLINED", "APPROVED"),
  util     = round(test_dat$credit_utilisation_pct, 0),
  stringsAsFactors = FALSE
)
declined_idx <- which(p_champ >= best_thresh)
approved_idx <- which(p_champ <  best_thresh)
sel_idx      <- sort(c(head(declined_idx,300), head(approved_idx,200)))[1:min(500,nrow(test_dat))]

aa_choices <- setNames(
  sel_idx,
  paste0(ifelse(test_display_df$decision[sel_idx]=="DECLINED","\u274c","\u2705"),
         " App #", sel_idx,
         " | Score:", test_display_df$score[sel_idx],
         " | ", round(test_display_df$prob[sel_idx]*100,1), "%",
         " | Util:", test_display_df$util[sel_idx], "%")
)
first_declined <- if (length(declined_idx)>0) declined_idx[1] else 1

# ── AI System Prompt (baked with live model stats) ───────────────
AI_SYSTEM_PROMPT <- paste0(
  "You are an expert Credit Risk AI Analyst embedded in a retail bank's loan adjudication system.\n",
  "You have deep knowledge of this specific model suite and dataset.\n\n",
  
  "## Model Suite\n",
  "Three logistic regression models were fitted on the same champion formula:\n",
  "1. **Champion model** (unweighted): AUC = ", auc_c, ", Gini = ", gini_c,
  ", KS = ", ks_stat, ", Youden threshold = ", round(best_thresh,3), "\n",
  "2. **Baseline model** (6 raw features, no engineering): AUC = ", auc_b,
  ", Gini = ", gini_b, "\n",
  "3. **Cost-sensitive weighted model**: AUC = ", auc_w,
  ", Youden threshold = ", round(thresh_weighted,3), "\n",
  "   The weighted model upweights minority-class (default) observations by a factor of ",
  round(inv_freq_weight,2), "x (inverse frequency weighting) to correct for class imbalance. ",
  "It catches more defaults but may over-decline good customers. ",
  "It is selected via the model toggle in the Model Performance tab.\n\n",
  
  "## Scorecard\n",
  "PDO=20, Base=600. Formula: Score = ", round(A_sc,1), " - ", round(B_sc,1), " x log-odds\n",
  "Higher score = lower risk. Scores typically range 400\u2013850.\n\n",
  
  "## Dataset\n",
  "- ", comma(nrow(dat)), " loan applications | ",
  round(mean(dat$default_flag)*100,1), "% population default rate\n",
  "- Train: ", comma(nrow(train_dat)), " | Test: ", comma(nrow(test_dat)), "\n\n",
  
  "## Feature Engineering\n",
  "- Log transforms: log_income=log1p(income), log_loan=log1p(loan_amount)\n",
  "- Interaction: risk_stress = interest_rate x dti_ratio\n",
  "- Flags: high_util_flag (util>80%), delinq_flag, is_never_delinquent\n",
  "- Ratio: income_to_loan = income / (loan_amount+1)\n",
  "- Non-linear age via natural cubic spline (df=4)\n",
  "- Categorical: home_ownership(4 levels), loan_purpose(7), region, email_domain_type, phone_verified\n",
  "- ALL continuous features are standardised (z-score) before model fitting.\n\n",
  
  "## Coefficient Attribution (Adverse Action)\n",
  "Contribution = beta_i x standardised_value_i (log-odds scale, internal use only).\n",
  "When explaining to end users or writing adverse action letters, ALWAYS translate to plain English ",
  "using the actual raw feature values (e.g. 'your credit utilisation is 87%', NOT '-0.61 log-odds').\n",
  "The top 3 positive contributions are the primary denial reasons per ECOA/FCRA.\n\n",
  
  "## Top Predictors (Information Value)\n",
  "credit_utilisation_pct (Strong), interest_rate (Strong), dti_ratio (Medium),\n",
  "risk_stress (engineered, Strong), num_delinquencies_2yr (Medium)\n\n",
  
  "Respond using ## Markdown headers. Be specific, data-driven, and concise.\n",
  "Topics: model methodology, weighted vs unweighted tradeoffs, risk segments, threshold policy,\n",
  "feature importance, adverse action letters (ECOA/FCRA compliant), regulatory concerns,\n",
  "portfolio strategy, what-if analyses, interpreting specific applicant results."
)

message("=== Setup complete. AUC=",auc_c," | PSI=",psi_val," (",psi_status,") ===")

# ================================================================
# SECTION 6: UI
# ================================================================
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$span(HTML(
      '<span style="font-weight:700;color:#f1c40f;">DataQuest</span>
       <span style="font-weight:300;color:#ecf0f1;"> 2026</span>')),
    titleWidth = 280),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(id="main_tab",
                menuItem("\U0001f4ca Data Quality",       tabName="quality",   icon=icon("magnifying-glass")),
                menuItem("\u2696\ufe0f  Dataset Integrity",tabName="integrity", icon=icon("scale-balanced")),
                menuItem("\U0001f50d Univariate EDA",      tabName="univariate",icon=icon("chart-bar")),
                menuItem("\U0001f517 Bivariate EDA",       tabName="bivariate", icon=icon("circle-nodes")),
                menuItem("\U0001f4a1 Feature Importance",  tabName="iv_tab",    icon=icon("star")),
                menuItem("\U0001f3af Model Performance",   tabName="model_tab", icon=icon("bullseye")),
                menuItem("\U0001f0cf Scorecard",           tabName="scorecard", icon=icon("id-card")),
                menuItem("\U0001f6a8 Adverse Action",      tabName="adverse",   icon=icon("file-circle-xmark")),
                menuItem("\U0001f4bc Business Dashboard",  tabName="business",  icon=icon("briefcase")),
                menuItem("\U0001f916 AI Credit Analyst",   tabName="ai_chat",   icon=icon("robot"))
    ),
    hr(),
    conditionalPanel("input.main_tab=='univariate'",
                     selectInput("uni_var","Variable:",choices=sort(key_vars),selected="credit_utilisation_pct"),
                     sliderInput("uni_bins","Bins / Groups:",min=4,max=20,value=10)),
    conditionalPanel("input.main_tab=='bivariate'",
                     selectInput("biv_x","X Variable (continuous):",choices=continuous_vars_eng,
                                 selected="credit_utilisation_pct"),
                     selectInput("biv_y","Y Variable (continuous):",choices=continuous_vars_eng,
                                 selected="interest_rate"),
                     selectInput("biv_color","Colour By:",
                                 choices=c("Default Status"="default_flag","Home Ownership"="home_ownership",
                                           "Region"="region","Loan Purpose"="loan_purpose"),
                                 selected="default_flag")),
    conditionalPanel("input.main_tab=='integrity'",
                     selectInput("reg_select","Region:",choices=sort(unique(as.character(dat$region)))),
                     selectInput("var_select","Variable:",
                                 choices=c("Annual Income"="annual_income",
                                           "Employment Length"="employment_length_years",
                                           "Credit Utilisation %"="credit_utilisation_pct"))),
    conditionalPanel("input.main_tab=='model_tab'",
                     hr(),
                     h5(HTML('<span style="color:#f1c40f;font-weight:700;">\u2696\ufe0f Model Selector</span>'),
                        style="padding-left:12px;margin-bottom:4px;"),
                     sliderInput("weight_slider",
                                 label    = NULL,
                                 min      = 1, max = 10, value = 1, step = 0.5),
                     uiOutput("weight_slider_label"),
                     uiOutput("weight_insight"),       # ← add this line
                     p("1 = Unweighted Champion (recommended)",
                       style="color:#95a5a6;font-size:10px;padding:2px 12px;"),
                     p(paste0("Auto-weight \u2248 ", round(inv_freq_weight, 1), "x (class ratio)"),
                       style="color:#95a5a6;font-size:10px;padding:2px 12px;"),
                     h5(HTML('<span style="color:#f1c40f;font-weight:700;">\u2699\ufe0f Threshold Control</span>'),
                        style="padding-left:12px;margin-bottom:4px;"),
                     sliderInput("model_thresh","Decision Threshold:",min=0.05,max=0.95,
                                 value=round(best_thresh,2),step=0.01),
                     uiOutput("optimal_badge_sidebar"),
                     tags$p(paste0("Youden Optimal: ",round(best_thresh,3)),
                            style="color:#95a5a6;font-size:11px;padding:2px 12px;"), hr()),
    conditionalPanel("input.main_tab=='scorecard'",
                     hr(),
                     h5(HTML('<span style="color:#f1c40f;font-weight:700;">\U0001f522 Score Calculator</span>'),
                        style="padding-left:12px;margin-bottom:4px;"),
                     sliderInput("sc_util","Credit Utilisation %",0,100,50),
                     sliderInput("sc_dti","DTI Ratio",0.0,1.0,0.3,step=0.01),
                     sliderInput("sc_int","Interest Rate (%)",5,35,15),
                     sliderInput("sc_age","Age",18,80,35),
                     sliderInput("sc_income","Annual Income",20000,300000,75000,step=5000),
                     sliderInput("sc_delinq","Delinquencies (2yr)",0,10,0)),
    # ── ADVERSE ACTION SIDEBAR ──
    conditionalPanel("input.main_tab=='adverse'",
                     hr(),
                     h5(HTML('<span style="color:#f1c40f;font-weight:700;">\U0001f6a8 Applicant Selector</span>'),
                        style="padding-left:12px;margin-bottom:4px;"),
                     selectInput("aa_applicant","Select Test Applicant:",
                                 choices=aa_choices, selected=first_declined),
                     sliderInput("aa_thresh","Approval Threshold:",min=0.05,max=0.95,
                                 value=round(best_thresh,2),step=0.01),
                     hr(),
                     p("\u274c = Model DECLINES at current threshold",
                       style="color:#e74c3c;font-size:11px;padding:0 10px;"),
                     p("\u2705 = Model APPROVES at current threshold",
                       style="color:#27ae60;font-size:11px;padding:0 10px;")),
    conditionalPanel("input.main_tab=='business'",
                     hr(),
                     sliderInput("biz_thresh","Decision Threshold:",min=0.05,max=0.95,
                                 value=round(best_thresh,2),step=0.01),
                     p("\u25b2 Stricter \u2192 less volume, less risk",
                       style="padding:5px 10px;color:#bdc3c7;font-size:11px;"),
                     p("\u25bc Looser \u2192 more volume, more risk",
                       style="padding:5px 10px;color:#bdc3c7;font-size:11px;"))
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$script(src="https://cdn.jsdelivr.net/npm/canvas-confetti@1.9.2/dist/confetti.browser.min.js"),
      tags$style(HTML("
  body,.content-wrapper{background-color:#f0f2f5!important;}
  .main-header .logo{background-color:#1a1a2e!important;}
  .main-header .navbar{background-color:#16213e!important;}
  .main-sidebar{background-color:#0d1b2a!important;}
  .sidebar-menu>li>a{color:#bdc3c7!important;}
  .sidebar-menu>li.active>a{background-color:#f1c40f!important;color:#1a1a2e!important;}
  .box{border-radius:8px;box-shadow:0 2px 12px rgba(0,0,0,.12);border-top-width:3px!important;}
  .value-box,.info-box{border-radius:8px;}
  .box-header .box-title{font-weight:700;font-size:14px;}
  h3{color:#2c3e50;font-weight:700;margin-bottom:20px;}
 
  /* ── Adverse Action styles (unchanged) ── */
  .aa-decision-declined{background:linear-gradient(135deg,#1a1a2e,#2c0a0a);
    border:2px solid #e74c3c;border-radius:12px;padding:20px 24px;margin-bottom:8px;}
  .aa-decision-approved{background:linear-gradient(135deg,#0a1a0a,#0a2c1a);
    border:2px solid #27ae60;border-radius:12px;padding:20px 24px;margin-bottom:8px;}
  .aa-header{font-size:22px;font-weight:900;letter-spacing:2px;margin-bottom:8px;}
  .aa-subtext{font-size:13px;color:#bdc3c7;margin-bottom:16px;}
  .reason-card{background:rgba(231,76,60,0.12);border-left:4px solid #e74c3c;
    border-radius:0 8px 8px 0;padding:12px 16px;margin:8px 0;}
  .reason-number{font-size:28px;font-weight:900;color:#e74c3c;
    float:left;margin-right:14px;line-height:1.1;}
  .reason-label{font-size:13px;font-weight:700;color:#ecf0f1;display:block;}
  .reason-detail{font-size:12px;color:#bdc3c7;margin-top:3px;display:block;}
  .reason-clearfix{overflow:hidden;}
  .protective-card{background:rgba(39,174,96,0.10);border-left:4px solid #27ae60;
    border-radius:0 8px 8px 0;padding:10px 14px;margin:6px 0;
    font-size:12px;color:#bdc3c7;}
  .path-tip{background:rgba(241,196,15,0.12);border-left:4px solid #f1c40f;
    border-radius:0 8px 8px 0;padding:10px 14px;margin:6px 0;
    font-size:12px;color:#ecf0f1;}
 
  /* ================================================================
     AI CHAT — FIXED LAYOUT (replaces the broken float-based rules)
     ================================================================ */
 
  /* Outer scroll container */
  .ai-chat-wrap {
    background: #0d1b2a;
    border-radius: 12px;
    padding: 12px 0;
    min-height: 420px;
    max-height: 520px;
    overflow-y: auto;
    margin-bottom: 12px;
    /* FLEX COLUMN so children stack vertically and container
       always expands to fit them — fixes the collapsed-height bug */
    display: flex;
    flex-direction: column;
  }
 
  /* User message — right-aligned */
  .chat-msg-user {
    background: #1e3a5f;
    border-radius: 12px 12px 4px 12px;
    padding: 10px 16px;
    margin: 6px 12px 6px auto;   /* auto left margin pushes right */
    max-width: 80%;
    font-size: 13px;
    color: #ecf0f1;
    /* NO float — flexbox handles alignment */
    align-self: flex-end;
    word-break: break-word;
  }
 
  /* AI message — left-aligned */
  .chat-msg-ai {
    background: #16213e;
    border-radius: 12px 12px 12px 4px;
    padding: 10px 16px;
    margin: 6px 12px 6px 12px;
    max-width: 88%;
    font-size: 13px;
    color: #ecf0f1;
    /* NO float — flexbox handles alignment */
    align-self: flex-start;
    word-break: break-word;
  }
 
  /* Markdown content inside AI bubbles */
  .chat-msg-ai h2,.chat-msg-ai h3{color:#f1c40f;font-size:14px;margin:8px 0 4px;}
  .chat-msg-ai p{margin:4px 0;}
  .chat-msg-ai ul,.chat-msg-ai ol{margin:4px 0;padding-left:18px;}
  .chat-msg-ai strong{color:#f1c40f;}
  .chat-msg-ai code{background:#2c3e50;padding:1px 5px;border-radius:4px;font-size:12px;}
 
  /* Clearfix — kept for safety, not strictly needed with flexbox */
  .chat-clearfix { clear: both; display: block; }
 
  /* Typing indicator */
  .ai-typing {
    padding: 10px 16px;
    color: #7f8c8d;
    font-style: italic;
    font-size: 12px;
    align-self: flex-start;
    margin: 6px 12px;
  }
 
  /* Quick-prompt buttons */
  .quick-btn {
    background: #16213e;
    color: #bdc3c7;
    border: 1px solid #2c3e50;
    border-radius: 20px;
    padding: 5px 14px;
    font-size: 11px;
    cursor: pointer;
    margin: 3px;
    transition: all 0.2s;
    display: inline-block;
  }
  .quick-btn:hover { background:#f1c40f; color:#1a1a2e; border-color:#f1c40f; }
 
  /* ── Threshold optimal celebration (unchanged) ── */
  @keyframes pulse-gold{0%{box-shadow:0 0 0 0 rgba(241,196,15,.7);}
    70%{box-shadow:0 0 0 14px rgba(241,196,15,0);}100%{box-shadow:0 0 0 0 rgba(241,196,15,0);}}
  @keyframes shimmer{0%{background-position:-200% center;}100%{background-position:200% center;}}
  .optimal-badge{background:linear-gradient(135deg,#f1c40f 0%,#f39c12 50%,#f1c40f 100%);
    background-size:200% auto;animation:shimmer 2s linear infinite,pulse-gold 1.5s ease-in-out infinite;
    border-radius:8px;padding:8px 12px;margin:6px 10px;text-align:center;
    color:#1a1a2e;font-weight:700;}
  .metric-box-live{animation:fadeInUp 0.4s ease-out;}
  @keyframes fadeInUp{from{opacity:0;transform:translateY(20px);}to{opacity:1;transform:translateY(0);}}
"))
    ),
    
    tabItems(
      
      # ============================================================
      # TAB 1: DATA QUALITY
      # ============================================================
      tabItem(tabName="quality",
              h3("\U0001f4ca Data Quality Report"),
              fluidRow(
                valueBoxOutput("vb_rows",width=3), valueBoxOutput("vb_cols",width=3),
                valueBoxOutput("vb_miss_rate",width=3), valueBoxOutput("vb_default_rate",width=3)),
              fluidRow(
                box(title="Missing Values by Variable",width=6,status="danger",solidHeader=TRUE,
                    withSpinner(plotlyOutput("miss_plot",height="320px"),color="#e74c3c")),
                box(title="Dirty Categories: Before vs After Cleaning",width=6,status="warning",solidHeader=TRUE,
                    withSpinner(plotlyOutput("dirty_plot",height="320px"),color="#f39c12"),
                    footer="home_ownership: 14 raw variants \u2192 4 clean. loan_purpose: 20 \u2192 7.")),
              fluidRow(
                box(title="Date Format Inconsistency \u2014 3 Formats Detected",width=5,status="info",solidHeader=TRUE,
                    withSpinner(plotlyOutput("date_fmt_plot",height="260px"),color="#3498db"),
                    footer="YYYY-MM-DD, MM/DD/YYYY, DD-Mon-YYYY. All parsed with lubridate."),
                box(title="Variable Summary",width=7,status="primary",solidHeader=TRUE,
                    withSpinner(DTOutput("var_tbl"),color="#2980b9")))),
      
      # ============================================================
      # TAB 2: INTEGRITY
      # ============================================================
      tabItem(tabName="integrity",
              h3("\u2696\ufe0f Train / Test Split Integrity"),
              fluidRow(
                valueBoxOutput("vb_psi",width=3), valueBoxOutput("vb_psi_status",width=3),
                valueBoxOutput("vb_train_n",width=3), valueBoxOutput("vb_test_n",width=3)),
              fluidRow(
                box(title="Class Balance: Train vs Test",width=5,status="primary",solidHeader=TRUE,
                    withSpinner(plotlyOutput("split_plot",height="320px"),color="#2980b9"),
                    footer="Default rate ~15.4% in BOTH splits \u2014 no distribution shift."),
                box(title="PSI \u2014 Population Stability Index (Train \u2192 Test)",width=7,status="warning",solidHeader=TRUE,
                    withSpinner(plotlyOutput("psi_plot",height="320px"),color="#f39c12"),
                    footer=paste0("PSI = ",psi_val," \u2192 ",psi_status,". < 0.10: Stable | 0.10\u20130.25: Moderate | > 0.25: Major"))),
              fluidRow(
                box(title="Why Grouped Imputation?",width=5,status="success",
                    h4("The Problem with Global Median Imputation"),
                    p("A global income median treats a mortgaged borrower in North-Urban identically to a renting borrower in South-Suburban \u2014 structurally different populations."),
                    h4("Our Solution: Region \u00d7 Home Ownership Groups"),
                    p("By computing the imputation median within each ",strong("Region + Home Ownership")," cell, we capture the local socio-economic reality."),
                    p("The boxplot proves this matters: group medians often deviate from the national median (",span("red dashed line",style="color:red;"),") by large margins."),
                    hr(), strong("Sidebar: "),"Select any region and variable to explore."),
                box(title="Regional Housing Interaction (Imputation Justification)",width=7,status="info",solidHeader=TRUE,
                    withSpinner(plotlyOutput("housing_box_plot",height="340px"),color="#3498db"),
                    footer="Red dashed line = national median. Boxes = per-group distribution."))),
      
      # ============================================================
      # TAB 3: UNIVARIATE EDA
      # ============================================================
      tabItem(tabName="univariate",
              h3("\U0001f50d Univariate Explorer"),
              fluidRow(
                valueBoxOutput("uni_iv",width=3), valueBoxOutput("uni_strength",width=3),
                valueBoxOutput("uni_miss",width=3), valueBoxOutput("uni_dr",width=3)),
              fluidRow(
                box(title="Feature Distribution (by Default Status)",width=6,status="primary",solidHeader=TRUE,
                    withSpinner(plotlyOutput("uni_dist",height="320px"),color="#2980b9")),
                box(title="Default Rate by Bin",width=6,status="danger",solidHeader=TRUE,
                    withSpinner(plotlyOutput("uni_dr_plot",height="320px"),color="#e74c3c"),
                    footer="Red dashed line = overall default rate (15.4%)")),
              fluidRow(
                box(title="Weight of Evidence (WoE) by Bin",width=6,status="info",solidHeader=TRUE,
                    withSpinner(plotlyOutput("woe_plot",height="320px"),color="#3498db"),
                    footer="WoE = ln(% Events / % Non-Events). Red = higher default risk."),
                box(title="WoE & IV Detail Table",width=6,status="warning",solidHeader=TRUE,
                    withSpinner(DTOutput("woe_tbl"),color="#f39c12")))),
      
      # ============================================================
      # TAB 4: BIVARIATE EDA
      # ============================================================
      tabItem(tabName="bivariate",
              h3("\U0001f517 Bivariate Explorer"),
              fluidRow(
                box(title="Scatter Plot (5 000-row sample)",width=7,status="primary",solidHeader=TRUE,
                    withSpinner(plotlyOutput("biv_scatter",height="440px"),color="#2980b9"),
                    footer="Continuous variables only. Colour by Default Status, Home Ownership, Region, or Loan Purpose."),
                box(title="Default Rate Heatmap: Region \u00d7 Loan Purpose",width=5,status="warning",solidHeader=TRUE,
                    withSpinner(plotlyOutput("dr_heatmap",height="440px"),color="#f39c12"))),
              fluidRow(
                box(title="Numeric Correlation Matrix",width=12,status="info",solidHeader=TRUE,
                    withSpinner(plotlyOutput("corr_heatmap",height="480px"),color="#3498db"),
                    footer="Strong positive correlation with default_flag highlights most predictive features."))),
      
      # ============================================================
      # TAB 5: FEATURE IMPORTANCE
      # ============================================================
      tabItem(tabName="iv_tab",
              h3("\U0001f4a1 Feature Importance \u2014 Information Value (IV)"),
              fluidRow(
                box(title="IV by Variable (ranked)",width=8,status="primary",solidHeader=TRUE,
                    withSpinner(plotlyOutput("iv_plot",height="450px"),color="#2980b9"),
                    footer="Dashed lines: 0.10 = Medium | 0.30 = Strong."),
                box(title="IV Strength Reference",width=4,status="info",
                    tableOutput("iv_guide"), hr(),
                    p(strong("IV Formula: "),"IV = \u03a3 (% Events \u2212 % Non-Events) \u00d7 WoE"),
                    p("Variables with IV > 0.10 were prioritised in the champion model."))),
              fluidRow(
                box(title="Default Rate by Region",width=4,status="danger",solidHeader=TRUE,
                    withSpinner(plotlyOutput("region_dr",height="320px"),color="#e74c3c")),
                box(title="Default Rate by Loan Purpose",width=4,status="warning",solidHeader=TRUE,
                    withSpinner(plotlyOutput("purpose_dr",height="320px"),color="#f39c12")),
                box(title="Default Rate by Year & Month",width=4,status="success",solidHeader=TRUE,
                    withSpinner(plotlyOutput("temporal_dr",height="320px"),color="#27ae60"),
                    footer="Temporal patterns can reveal economic cycles or data artefacts."))),
      
      # ============================================================
      # TAB 6: MODEL PERFORMANCE
      # ============================================================
      tabItem(tabName="model_tab",
              h3("\U0001f3af Model Performance"),
              fluidRow(
                valueBoxOutput("vb_auc_c",width=3), valueBoxOutput("vb_auc_b",width=3),
                valueBoxOutput("vb_gini",width=3),  valueBoxOutput("vb_ks",width=3)),
              fluidRow(
                valueBoxOutput("vb_active_model", width=8),
                valueBoxOutput("vb_weight_note",  width=4)
              ),
              fluidRow(
                div(class="metric-box-live",
                    valueBoxOutput("vb_live_acc",width=3), valueBoxOutput("vb_live_prec",width=3),
                    valueBoxOutput("vb_live_rec",width=3),  valueBoxOutput("vb_live_f1",width=3))),
              uiOutput("optimal_banner"),
              fluidRow(
                box(title="ROC Curves: Champion vs Baseline",width=6,status="primary",solidHeader=TRUE,
                    withSpinner(plotlyOutput("roc_plot",height="360px"),color="#2980b9")),
                box(title="Score Separation (Default vs No Default)",width=6,status="info",solidHeader=TRUE,
                    withSpinner(plotlyOutput("score_dist_plot",height="360px"),color="#3498db"),
                    footer="Vertical line = current threshold.")),
              fluidRow(
                box(title=uiOutput("conf_title"),width=5,status="warning",solidHeader=TRUE,
                    withSpinner(plotlyOutput("conf_matrix_plot",height="340px"),color="#f39c12")),
                box(title="Top 20 Model Coefficients (log-odds)",width=7,status="danger",solidHeader=TRUE,
                    withSpinner(plotlyOutput("coef_plot",height="340px"),color="#e74c3c"),
                    footer="Error bars = 95% CI. Red = increases risk. Blue = decreases risk.")),
              fluidRow(
                box(title="Calibration Plot (Reliability Diagram)",width=6,status="success",solidHeader=TRUE,
                    withSpinner(plotlyOutput("calib_plot",height="340px"),color="#27ae60"),
                    footer="Points on the diagonal = perfect calibration."),
                box(title="Cumulative Gains & Lift Chart",width=6,status="primary",solidHeader=TRUE,
                    withSpinner(plotlyOutput("gain_plot",height="340px"),color="#8e44ad"),
                    footer="% of defaults captured vs % of population screened.")),
              fluidRow(
                box(title="KS Decile Analysis Table",width=12,status="primary",solidHeader=TRUE,
                    withSpinner(DTOutput("decile_tbl"),color="#2980b9"),
                    footer="Decile 1 = highest risk. Max KS decile highlighted."))),
      
      # ============================================================
      # TAB 7: SCORECARD
      # ============================================================
      tabItem(tabName="scorecard",
              h3("\U0001f0cf Scorecard \u2014 PDO 20 | Base Score 600"),
              fluidRow(
                valueBoxOutput("vb_sc_pdo",width=3), valueBoxOutput("vb_sc_base",width=3),
                valueBoxOutput("vb_sc_median",width=3), valueBoxOutput("vb_sc_calc",width=3)),
              fluidRow(
                box(title="Score Distribution by Default Status",width=7,status="primary",solidHeader=TRUE,
                    withSpinner(plotlyOutput("sc_dist_plot",height="360px"),color="#2980b9"),
                    footer="Higher score = lower risk. Separation indicates model quality."),
                box(title="Live Score Calculator",width=5,status="success",solidHeader=TRUE,
                    uiOutput("sc_calc_display"), hr(),
                    p("Adjust sliders in the sidebar to recalculate in real time.",
                      style="color:#7f8c8d;font-size:12px;"))),
              fluidRow(
                box(title="Score \u2192 Default Probability Lookup", width=6, status="info", solidHeader=TRUE,
                    withSpinner(plotlyOutput("sc_prob_plot", height="300px"), color="#3498db"),
                    footer=paste0("Formula: Score = ",round(A_sc,1)," \u2212 ",round(B_sc,1)," \u00d7 log-odds")),
                box(title="Scorecard Coefficient Points Table (Top 15)", width=6, status="warning", solidHeader=TRUE,
                    withSpinner(DTOutput("sc_coef_tbl"), color="#f39c12"),
                    footer="Points per 1 SD change in the standardised feature.")),
              fluidRow(
                box(title="Score Decile Table with Lift & Cumulative Capture",width=12,status="danger",solidHeader=TRUE,
                    withSpinner(DTOutput("sc_decile_tbl"),color="#e74c3c"),
                    footer="Decile 1 = highest risk. Lift > 1 means this decile over-represents defaults."))),
      
      # ============================================================
      # TAB 8: ADVERSE ACTION  ← NEW
      # ============================================================
      tabItem(tabName="adverse",
              h3("\U0001f6a8 Adverse Action \u2014 Denial Explainability"),
              fluidRow(
                valueBoxOutput("aa_score",width=3), valueBoxOutput("aa_prob",width=3),
                valueBoxOutput("aa_decision",width=3), valueBoxOutput("aa_top_risk",width=3)),
              fluidRow(
                box(title="Feature Contribution Waterfall (Coefficient Attribution)",
                    width=7, status="primary", solidHeader=TRUE,
                    withSpinner(plotlyOutput("aa_waterfall",height="480px"),color="#2980b9"),
                    footer=paste0("Contribution = \u03b2 \u00d7 standardised value. ",
                                  "Red bars push toward default; blue bars reduce risk. ",
                                  "Grey intercept = population baseline log-odds.")),
                # Replace lines 980-982
                box(title=uiOutput("aa_notice_title"), width=5, status="danger", solidHeader=TRUE,
                    div(style="height:480px; overflow-y:auto; padding-right:4px;",
                        uiOutput("aa_notice")))),
              fluidRow(
                box(title="Applicant Profile vs Population Averages",width=6,status="warning",solidHeader=TRUE,
                    withSpinner(DTOutput("aa_profile_tbl"),color="#f39c12"),
                    footer="Values shown are the applicant's raw (unstandardised) feature values vs dataset means."),
                box(title="\U0001f916 AI Adverse Action Letter Generator",width=6,status="info",solidHeader=TRUE,
                    p("Click to generate a regulatory-compliant adverse action letter explaining this decision to the applicant.",
                      style="color:#7f8c8d;font-size:12px;margin-bottom:10px;"),
                    actionButton("gen_aa_letter","Generate AI Adverse Action Letter",
                                 icon=icon("file-alt"),
                                 style="background:#e74c3c;color:white;border:none;border-radius:8px;
                                  padding:10px 20px;font-weight:700;width:100%;margin-bottom:12px;"),
                    uiOutput("aa_letter_output")))),
      
      # ============================================================
      # TAB 9: BUSINESS DASHBOARD
      # ============================================================
      tabItem(tabName="business",
              h3("\U0001f4bc Business Decision Dashboard"),
              fluidRow(
                valueBoxOutput("biz_approved",width=3), valueBoxOutput("biz_dr",width=3),
                valueBoxOutput("biz_precision",width=3), valueBoxOutput("biz_recall",width=3)),
              fluidRow(
                box(title="Volume vs Risk Trade-off",width=8,status="primary",solidHeader=TRUE,
                    withSpinner(plotlyOutput("vol_risk_plot",height="360px"),color="#2980b9"),
                    footer="\U0001f534 Red dot = your current threshold."),
                box(title="Business Metric Definitions",width=4,status="info",
                    h4("Understanding the Metrics"),
                    tags$ul(
                      tags$li(tags$b("Approval Rate: "),"% of applicants passed by the model."),
                      tags$li(tags$b("Default Rate (Approved): "),"% of approved loans that default."),
                      tags$li(tags$b("Precision: "),"Of flagged defaulters, how many truly defaulted?"),
                      tags$li(tags$b("Recall: "),"Of all true defaulters, how many did we catch?")),
                    hr(),
                    p(icon("arrow-down"),strong(" Lower threshold:")," Approve more \u2192 revenue \u2191, risk \u2191."),
                    p(icon("arrow-up"),  strong(" Higher threshold:")," Tighter screen \u2192 risk \u2193, volume \u2193."))),
              fluidRow(
                box(title="Precision\u2013Recall Trade-off",width=6,status="warning",solidHeader=TRUE,
                    withSpinner(plotlyOutput("prec_recall_plot",height="340px"),color="#f39c12")),
                box(title="Lorenz Curve \u2014 Concentration of Risk",width=6,status="danger",solidHeader=TRUE,
                    withSpinner(plotlyOutput("lorenz_plot",height="340px"),color="#e74c3c"),
                    footer=paste0("Gini = ",gini_c,". Curve away from diagonal = risk is concentrated in top deciles."))),
              
              # ── Profit Simulator ─────────────────────────────────────────
              fluidRow(
                box(title="\U0001f4b0 Expected Profit Simulator", width=12, status="success", solidHeader=TRUE,
                    fluidRow(
                      column(3,
                             sliderInput("biz_revenue","Avg Revenue per Approved Loan (R):",
                                         min=500,max=10000,value=2000,step=100,pre="R "),
                             sliderInput("biz_loss","Avg Loss per Default (R):",
                                         min=1000,max=50000,value=15000,step=500,pre="R "),
                             actionButton("biz_ai_advice","\U0001f916 Analyse This Scenario",
                                          class="btn-success btn-sm",style="width:100%;margin-top:8px;")
                      ),
                      column(9,
                             withSpinner(plotlyOutput("profit_plot",height="280px"),color="#27ae60")
                      )
                    ),
                    fluidRow(
                      valueBoxOutput("biz_net_profit",width=4),
                      valueBoxOutput("biz_gross_revenue",width=4),
                      valueBoxOutput("biz_total_loss",width=4)
                    ),
                    uiOutput("biz_ai_output"),
                    footer="Blue dashed = current threshold. Gold dotted = profit-maximising threshold."))),      
      
      # ============================================================
      # TAB 10: AI CREDIT ANALYST  ← NEW
      # ============================================================
      tabItem(tabName="ai_chat",
              h3("\U0001f916 AI Credit Analyst"),
              fluidRow(
                box(
                  title = "\U0001f916 Chat",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Quick-prompt buttons
                  div(style = "padding:8px 0 10px;",
                      tags$b(style = "color:#000000;font-size:13px;",
                             "\U0001f4a1 Quick Prompts: "),
                      tags$span(class="quick-btn",
                                onclick="Shiny.setInputValue(\'quick_prompt\',\'explain_model\',{priority:\'event\'})",
                                "\U0001f9e0 Explain the model"),
                      tags$span(class="quick-btn",
                                onclick="Shiny.setInputValue(\'quick_prompt\',\'worst_segment\',{priority:\'event\'})",
                                "\U0001f6a8 Worst risk segment"),
                      tags$span(class="quick-btn",
                                onclick="Shiny.setInputValue(\'quick_prompt\',\'optimal_threshold\',{priority:\'event\'})",
                                "\u2696\ufe0f Optimal threshold advice"),
                      tags$span(class="quick-btn",
                                onclick="Shiny.setInputValue(\'quick_prompt\',\'feature_engineering\',{priority:\'event\'})",
                                "\u2699\ufe0f Feature engineering choices"),
                      tags$span(class="quick-btn",
                                onclick="Shiny.setInputValue(\'quick_prompt\',\'regulatory\',{priority:\'event\'})",
                                "\u2696\ufe0f Regulatory concerns"),
                      tags$span(class="quick-btn",
                                onclick="Shiny.setInputValue(\'quick_prompt\',\'champion_vs_baseline\',{priority:\'event\'})",
                                "\U0001f3c6 Champion vs Baseline"),
                      tags$span(class="quick-btn",
                                onclick="Shiny.setInputValue(\'quick_prompt\',\'portfolio_strategy\',{priority:\'event\'})",
                                "\U0001f4ca Portfolio strategy"),
                      tags$span(class="quick-btn",
                                onclick="Shiny.setInputValue(\'quick_prompt\',\'adverse_action_letter\',{priority:\'event\'})",
                                "\U0001f4dc Adverse action letter")
                  ),
                  
                  # Chat scroll area — uiOutput directly inside the flex container div
                  div(id = "chat_scroll", class = "ai-chat-wrap",
                      uiOutput("chat_history_ui")
                  ),
                  
                  # Input row
                  fluidRow(
                    column(10,
                           textAreaInput("ai_input",
                                         label    = NULL,
                                         placeholder = "Ask anything about the model, risk strategy, regulations, or data...",
                                         rows     = 2,
                                         width    = "100%")
                    ),
                    column(2,
                           br(),
                           actionButton("send_ai", "Send",
                                        icon  = icon("paper-plane"),
                                        style = "background:#f1c40f;color:#1a1a2e;border:none;
                           font-weight:700;border-radius:8px;width:100%;padding:10px;"),
                           br(), br(),
                           actionButton("clear_chat", "Clear",
                                        icon  = icon("trash"),
                                        style = "background:#2c3e50;color:#bdc3c7;border:none;
                           border-radius:8px;width:100%;padding:6px;")
                    )
                  )
                )
              )
      )
      
      
    )
    
  ) # end tabItems
) # end dashboardBody
#) # end dashboardPage

# ================================================================
# SECTION 7: SERVER
# ================================================================
server <- function(input, output, session) {
  
  # ── Live threshold reactives (model tab) ────────────────────────
  live_row <- reactive({
    req(input$model_thresh)
    ap   <- active_preds()
    thresh_use <- input$model_thresh
    pred <- as.integer(ap$probs >= thresh_use)
    tp <- sum(pred==1 & test_s$default_flag==1)
    fp <- sum(pred==1 & test_s$default_flag==0)
    fn <- sum(pred==0 & test_s$default_flag==1)
    tn <- sum(pred==0 & test_s$default_flag==0)
    approved <- sum(pred==0)
    tibble(threshold=thresh_use, tp=tp, fp=fp, fn=fn, tn=tn,
           approved=approved, approved_pct=approved/length(pred),
           default_rate_approved=fn/max(approved,1),
           precision=tp/max(tp+fp,1), recall=tp/max(tp+fn,1),
           f1=2*tp/max(2*tp+fp+fn,1), accuracy=(tp+tn)/length(pred))
  })
  is_optimal <- reactive({ req(input$model_thresh); abs(input$model_thresh-best_thresh)<=0.015 })
  # Active model predictions — switch based on weight slider
  active_preds <- reactive({
    req(input$weight_slider)
    w <- input$weight_slider
    if (w <= 1.2) {
      list(probs=p_champ, label="Champion (Unweighted)", thresh=best_thresh,
           auc=auc_c, note="Standard logistic regression — recommended for regulatory clarity.")
    } else {
      # Interpolate between unweighted and fully-weighted predictions
      alpha  <- min((w - 1) / (inv_freq_weight - 1), 1)
      blended <- (1 - alpha) * p_champ + alpha * p_weighted_test
      roc_bl  <- tryCatch(roc(test_s$default_flag, blended, quiet=TRUE), error=function(e) roc_c)
      thresh_bl <- tryCatch(
        as.numeric(coords(roc_bl,"best",ret="threshold",best.method="youden")[1]),
        error=function(e) best_thresh)
      list(probs=blended, label=paste0("Weighted (w=",w,")"), thresh=thresh_bl,
           auc=round(as.numeric(auc(roc_bl)),4),
           note=paste0("Minority class upweighted ",w,"x. Threshold self-adjusts via Youden's J."))
    }
  })
  
  output$weight_insight <- renderUI({
    ap <- active_preds()
    w  <- input$weight_slider
    
    if (w <= 1.2) {
      div(style = "margin:6px 10px;padding:8px 10px;border-radius:6px;
                 background:rgba(39,174,96,0.12);border-left:3px solid #27ae60;
                 font-size:11px;color:#bdc3c7;",
          tags$b(style="color:#27ae60;", "\u2705 Recommended setting"), br(),
          "Standard logistic regression. Threshold 0.16 is Youden-optimal.",
          br(), "Easiest to defend to regulators.")
      
    } else if (w < inv_freq_weight) {
      div(style = "margin:6px 10px;padding:8px 10px;border-radius:6px;
                 background:rgba(241,196,15,0.10);border-left:3px solid #f1c40f;
                 font-size:11px;color:#bdc3c7;",
          tags$b(style="color:#f1c40f;", "\u26a0\ufe0f Blended model"), br(),
          paste0("Threshold auto-adjusts to ", round(ap$thresh, 2),
                 " — the Youden optimum for this blend."), br(),
          paste0("AUC = ", ap$auc,
                 " (vs 0.798 unweighted) — discrimination is nearly identical."),
          br(), "Higher weight \u2260 better model, just rescaled probabilities.")
      
    } else {
      div(style = "margin:6px 10px;padding:8px 10px;border-radius:6px;
                 background:rgba(231,76,60,0.10);border-left:3px solid #e74c3c;
                 font-size:11px;color:#bdc3c7;",
          tags$b(style="color:#e74c3c;", "\u23ed\ufe0f Plateau reached"), br(),
          paste0("Weight \u2265 ", round(inv_freq_weight, 1),
                 " = fully weighted model. Slider has no further effect."), br(),
          paste0("Threshold = ", round(ap$thresh, 2),
                 " | AUC = ", ap$auc, "."), br(),
          "Catches fewer defaults than unweighted (3985 vs 4001).")
    }
  })
  
  observeEvent(input$model_thresh, {
    if (is_optimal()) {
      runjs("if(window._lc&&Date.now()-window._lc<3500)return;window._lc=Date.now();
        if(typeof confetti!=='undefined'){
          confetti({particleCount:160,spread:80,origin:{y:.55},
            colors:['#f1c40f','#27ae60','#2980b9','#e74c3c','#ffffff']});
          setTimeout(()=>confetti({particleCount:80,spread:110,origin:{y:.4},angle:60,
            colors:['#f1c40f','#27ae60']}),450);
          setTimeout(()=>confetti({particleCount:80,spread:110,origin:{y:.4},angle:120,
            colors:['#2980b9','#e74c3c']}),850);}")
    }
  })
  
  output$optimal_badge_sidebar <- renderUI({
    if (is_optimal()) div(class="optimal-badge",
                          HTML("\U0001f3c6 OPTIMAL THRESHOLD!<br><small>Youden's J Maximum</small>"))
  })
  output$optimal_banner <- renderUI({
    if (is_optimal()) fluidRow(column(12,
                                      div(style="background:linear-gradient(90deg,#f1c40f,#f39c12,#f1c40f);
                 background-size:200% auto;animation:shimmer 2s linear infinite;
                 border-radius:8px;padding:12px 20px;margin:0 15px 15px;
                 text-align:center;color:#1a1a2e;font-size:16px;font-weight:700;",
                                          HTML("\U0001f3c6 Youden Optimal Threshold found! Maximises Sensitivity + Specificity simultaneously."))))
  })
  
  output$vb_live_acc  <- renderValueBox(valueBox(percent(live_row()$accuracy[1],.1),
                                                 paste0("Accuracy @ ",input$model_thresh),icon=icon("check"),color="blue"))
  output$vb_live_prec <- renderValueBox(valueBox(percent(live_row()$precision[1],.1),
                                                 paste0("Precision @ ",input$model_thresh),icon=icon("crosshairs"),color="purple"))
  output$vb_live_rec  <- renderValueBox(valueBox(percent(live_row()$recall[1],.1),
                                                 paste0("Recall @ ",input$model_thresh),icon=icon("bell"),color="orange"))
  output$vb_live_f1   <- renderValueBox(valueBox(round(live_row()$f1[1],3),
                                                 paste0("F1 Score @ ",input$model_thresh),icon=icon("star-half-stroke"),
                                                 color=if(is_optimal())"yellow"else"red"))
  output$vb_active_model <- renderValueBox({
    ap  <- active_preds()
    col <- if (input$weight_slider <= 1.2) "green" else "orange"
    valueBox(ap$label, "Active Model", icon = icon("scale-balanced"), color = col)
  })
  output$vb_weight_note <- renderValueBox({
    ap <- active_preds()
    valueBox(ap$auc, "Active Model AUC", icon = icon("trophy"), color = "blue")
  })
  
  output$conf_title <- renderUI({
    if (is_optimal()) HTML(paste0("\U0001f3c6 Confusion Matrix @ Optimal (",round(best_thresh,3),")"))
    else HTML(paste0("Confusion Matrix @ Threshold = ",input$model_thresh))
  })
  
  # ── TAB 1: DATA QUALITY ─────────────────────────────────────────
  output$vb_rows  <- renderValueBox(valueBox(comma(nrow(dat)),"Total Records",icon=icon("database"),color="blue"))
  output$vb_cols  <- renderValueBox(valueBox(ncol(dat_raw),"Variables",icon=icon("table-columns"),color="purple"))
  output$vb_miss_rate <- renderValueBox({
    pct <- round(sum(is.na(dat_raw))/prod(dim(dat_raw))*100,1)
    valueBox(paste0(pct,"%"),"Overall Missingness",icon=icon("circle-question"),color="red")})
  output$vb_default_rate <- renderValueBox(valueBox(
    paste0(round(mean(dat$default_flag)*100,1),"%"),"Default Rate",
    icon=icon("triangle-exclamation"),color="orange"))
  
  output$miss_plot <- renderPlotly({
    p <- ggplot(miss_summary,aes(x=reorder(variable,pct_miss),y=pct_miss,fill=pct_miss,
                                 text=paste0(variable,"\n",comma(missing)," (",round(pct_miss,1),"%)"))) +
      geom_col() + coord_flip() +
      scale_fill_gradient(low="#f39c12",high="#e74c3c") +
      scale_y_continuous(labels=function(x)paste0(x,"%")) +
      theme_minimal(base_size=12) + theme(legend.position="none") + labs(x=NULL,y="% Missing")
    ggplotly(p,tooltip="text")})
  
  output$dirty_plot <- renderPlotly({
    raw_df   <- dat_raw %>% count(home_ownership) %>% mutate(stage="Raw (14 variants)")
    clean_df <- dat %>% count(home_ownership) %>%
      mutate(home_ownership=as.character(home_ownership),stage="Clean (4 levels)")
    pd <- bind_rows(raw_df %>% mutate(home_ownership=as.character(home_ownership)),clean_df)
    p <- ggplot(pd,aes(x=reorder(home_ownership,n),y=n,fill=stage,text=paste0(home_ownership,": ",comma(n)))) +
      geom_col(position="dodge") + coord_flip() +
      scale_fill_manual(values=c("Raw (14 variants)"="#e74c3c","Clean (4 levels)"="#27ae60")) +
      theme_minimal() + labs(x=NULL,y="Count",fill=NULL)
    ggplotly(p,tooltip="text")})
  
  output$date_fmt_plot <- renderPlotly({
    fmt_counts <- tibble(Format=c("YYYY-MM-DD","MM/DD/YYYY","DD-Mon-YYYY / Other"),
                         Count=c(84839,24002,nrow(dat_raw)-84839-24002))
    p <- ggplot(fmt_counts,aes(x=reorder(Format,Count),y=Count,fill=Format,
                               text=paste0(Format,": ",comma(Count)))) +
      geom_col() + coord_flip() + scale_fill_brewer(palette="Set2") +
      theme_minimal() + theme(legend.position="none") + labs(x=NULL,y="Row Count")
    ggplotly(p,tooltip="text")})
  
  output$var_tbl <- renderDT({
    tibble(Variable=names(dat_raw),
           Type=sapply(dat_raw,function(x)class(x)[1]),
           Unique=sapply(dat_raw,function(x)n_distinct(x,na.rm=TRUE)),
           Missing=sapply(dat_raw,function(x)sum(is.na(x))),
           `% Missing`=round(sapply(dat_raw,function(x)mean(is.na(x))*100),1)) %>%
      datatable(rownames=FALSE,options=list(pageLength=10,scrollX=TRUE),
                class="table-striped table-bordered table-sm") %>%
      formatStyle("% Missing",background=styleInterval(c(5,20),c("white","#fff3cd","#f8d7da")))})
  
  # ── TAB 2: INTEGRITY ────────────────────────────────────────────
  output$vb_psi <- renderValueBox(valueBox(psi_val,"PSI (Train \u2192 Test)",icon=icon("arrows-rotate"),color="blue"))
  output$vb_psi_status <- renderValueBox({
    col <- case_when(psi_status=="Stable"~"green",psi_status=="Moderate Shift"~"orange",TRUE~"red")
    valueBox(psi_status,"Population Stability",icon=icon("gauge"),color=col)})
  output$vb_train_n <- renderValueBox(valueBox(comma(nrow(train_dat)),"Train Records",icon=icon("database"),color="purple"))
  output$vb_test_n  <- renderValueBox(valueBox(comma(nrow(test_dat)), "Test Records", icon=icon("vial"),color="teal"))
  
  output$split_plot <- renderPlotly({
    pd <- dat %>% group_by(set,default_flag) %>% summarise(count=n(),.groups="drop") %>%
      group_by(set) %>% mutate(pct=count/sum(count),Status=ifelse(default_flag==1,"Default","No Default"),
                               lbl=paste0(comma(count),"\n(",percent(pct,.1),")"))
    p <- ggplot(pd,aes(x=set,y=count,fill=Status,text=lbl)) +
      geom_col(position="stack",width=0.55) +
      geom_text(aes(label=lbl),position=position_stack(vjust=.5),color="white",size=3.5,fontface="bold") +
      scale_fill_manual(values=c("No Default"="#2c3e50","Default"="#e74c3c")) +
      theme_minimal(base_size=13) + labs(x="Split",y="Count",fill=NULL)
    ggplotly(p,tooltip="text")})
  
  output$psi_plot <- renderPlotly({
    p <- ggplot(psi_df,aes(x=Bin)) +
      geom_col(aes(y=Train_Pct,fill="Train"),alpha=.7,position="dodge") +
      geom_col(aes(y=Test_Pct, fill="Test"), alpha=.7,position="dodge") +
      scale_fill_manual(values=c("Train"="#2980b9","Test"="#e74c3c")) +
      theme_minimal(base_size=11) + theme(axis.text.x=element_text(angle=35,hjust=1)) +
      labs(x="Score Bin",y="% of Population",fill=NULL,
           title=paste0("PSI = ",psi_val,"  |  Status: ",psi_status))
    ggplotly(p)})
  
  output$housing_box_plot <- renderPlotly({
    var <- input$var_select; g_med <- median(dat[[var]],na.rm=TRUE)
    y_cap <- quantile(dat[[var]],.95,na.rm=TRUE)
    reg_dat <- dat %>% filter(as.character(region)==input$reg_select,!is.na(.data[[var]]))
    p <- ggplot(reg_dat,aes(x=home_ownership,y=.data[[var]],fill=home_ownership)) +
      geom_boxplot(outlier.shape=NA,alpha=.75,width=.5) +
      geom_hline(yintercept=g_med,color="red",linetype="dashed",linewidth=1) +
      scale_y_continuous(labels=comma,limits=c(0,y_cap)) +
      scale_fill_brewer(palette="Set1") + theme_minimal(base_size=13) +
      theme(legend.position="none") +
      labs(title=paste("Region:",input$reg_select),x="Home Ownership",y=var)
    ggplotly(p)})
  
  # ── TAB 3: UNIVARIATE ───────────────────────────────────────────
  uni_res <- reactive({ compute_woe_iv(dat,input$uni_var,n_bins=input$uni_bins) })
  
  output$uni_iv <- renderValueBox(valueBox(round(uni_res()$iv,4),"IV",icon=icon("star"),color="blue"))
  output$uni_strength <- renderValueBox({
    iv_val <- uni_res()$iv
    s   <- case_when(iv_val<.02~"Useless",iv_val<.10~"Weak",iv_val<.30~"Medium",iv_val<.50~"Strong",TRUE~"Very Strong")
    col <- case_when(s=="Useless"~"red",s=="Weak"~"orange",s=="Medium"~"yellow",TRUE~"green")
    valueBox(s,"Predictive Strength",icon=icon("gauge"),color=col)})
  output$uni_miss <- renderValueBox({
    req(input$uni_var)
    pct <- if(!input$uni_var %in% names(dat_raw)) 0.0
    else round(mean(is.na(dat_raw[[input$uni_var]]))*100,1)
    if(is.nan(pct)||is.na(pct)) pct <- 0.0
    valueBox(paste0(pct,"%"),"Missing Rate",icon=icon("circle-question"),color=if(pct>10)"red"else"green")})
  output$uni_dr <- renderValueBox({
    dr <- round(mean(dat$default_flag[!is.na(dat[[input$uni_var]])])*100,1)
    valueBox(paste0(dr,"%"),"Overall Default Rate",icon=icon("triangle-exclamation"),color="orange")})
  
  output$uni_dist <- renderPlotly({
    var <- input$uni_var
    d   <- dat %>% filter(!is.na(.data[[var]])) %>%
      mutate(Status=ifelse(default_flag==1,"Default","No Default"))
    if(is.numeric(dat[[var]])){
      p <- ggplot(d,aes(x=.data[[var]],fill=Status)) +
        geom_histogram(aes(y=after_stat(density)),bins=input$uni_bins,alpha=.6,position="identity") +
        scale_fill_manual(values=c("No Default"="#3498db","Default"="#e74c3c")) +
        theme_minimal() + labs(x=var,y="Density",fill=NULL)
    } else {
      d2 <- d %>% count(.data[[var]],Status)
      p  <- ggplot(d2,aes(x=.data[[var]],y=n,fill=Status)) +
        geom_col(position="fill") +
        scale_fill_manual(values=c("No Default"="#3498db","Default"="#e74c3c")) +
        scale_y_continuous(labels=percent) + coord_flip() +
        theme_minimal() + labs(x=var,y="Proportion",fill=NULL)
    }
    ggplotly(p)})
  
  output$uni_dr_plot <- renderPlotly({
    wd <- uni_res()$woe_df
    p  <- ggplot(wd,aes(x=reorder(bin_label,default_rate),y=default_rate,fill=default_rate,
                        text=paste0("Bin: ",bin_label,"\nDefault Rate: ",percent(default_rate,.1),"\nn = ",comma(n)))) +
      geom_col() + geom_hline(yintercept=mean(dat$default_flag),color="red",linetype="dashed",linewidth=1) +
      coord_flip() + scale_fill_gradient(low="#f1c40f",high="#c0392b") +
      scale_y_continuous(labels=percent) + theme_minimal() + theme(legend.position="none") +
      labs(x="Bin",y="Default Rate")
    ggplotly(p,tooltip="text")})
  
  output$woe_plot <- renderPlotly({
    wd <- uni_res()$woe_df
    p  <- ggplot(wd,aes(x=reorder(bin_label,woe),y=woe,fill=woe>0,
                        text=paste0("Bin: ",bin_label,"\nWoE: ",round(woe,3)))) +
      geom_col() + geom_hline(yintercept=0,linewidth=.5) + coord_flip() +
      scale_fill_manual(values=c("FALSE"="#2980b9","TRUE"="#e74c3c"),guide="none") +
      theme_minimal() + labs(x="Bin",y="Weight of Evidence")
    ggplotly(p,tooltip="text")})
  
  output$woe_tbl <- renderDT({
    uni_res()$woe_df %>%
      select(Bin=bin_label,N=n,Events=events,`Non-Events`=non_events,
             `Default Rate`=default_rate,WoE=woe,`IV Part`=iv_part) %>%
      mutate(`Default Rate`=percent(`Default Rate`,.1),WoE=round(WoE,4),`IV Part`=round(`IV Part`,5)) %>%
      datatable(rownames=FALSE,options=list(pageLength=8),class="table-striped table-sm")})
  
  # ── TAB 4: BIVARIATE ────────────────────────────────────────────
  output$biv_scatter <- renderPlotly({
    xv <- input$biv_x; yv <- input$biv_y
    d  <- dat %>% sample_n(min(5000,nrow(dat))) %>%
      filter(!is.na(.data[[xv]]),!is.na(.data[[yv]])) %>%
      mutate(color_var=as.character(.data[[input$biv_color]]))
    p <- ggplot(d,aes(x=.data[[xv]],y=.data[[yv]],color=color_var,
                      text=paste0(xv,": ",round(.data[[xv]],2),"\n",yv,": ",round(.data[[yv]],2),
                                  "\n",input$biv_color,": ",color_var))) +
      geom_point(alpha=.35,size=1.2) + theme_minimal() + labs(color=input$biv_color)
    ggplotly(p,tooltip="text")})
  
  output$dr_heatmap <- renderPlotly({
    d <- dat %>% group_by(region,loan_purpose) %>%
      summarise(dr=mean(default_flag),n=n(),.groups="drop") %>%
      filter(n>100) %>% mutate(across(c(region,loan_purpose),as.character))
    p <- ggplot(d,aes(x=loan_purpose,y=region,fill=dr,
                      text=paste0(region," | ",loan_purpose,"\nDefault Rate: ",percent(dr,.1),"\nn = ",comma(n)))) +
      geom_tile(color="white",linewidth=.5) +
      scale_fill_gradient2(low="#27ae60",mid="#f39c12",high="#c0392b",
                           midpoint=mean(dat$default_flag),labels=percent) +
      theme_minimal(base_size=11) + theme(axis.text.x=element_text(angle=35,hjust=1)) +
      labs(x=NULL,y=NULL,fill="Default\nRate")
    ggplotly(p,tooltip="text")})
  
  output$corr_heatmap <- renderPlotly({
    num_vars <- c("age","annual_income","employment_length_years","credit_utilisation_pct",
                  "dti_ratio","interest_rate","loan_amount","num_delinquencies_2yr",
                  "num_hard_inquiries_6mo","total_revolving_balance","risk_stress",
                  "income_to_loan","default_flag")
    cm  <- cor(dat[,num_vars],use="complete.obs")
    cdf <- as.data.frame(as.table(cm)) %>% rename(r=Freq)
    p <- ggplot(cdf,aes(x=Var1,y=Var2,fill=r,text=paste0(Var1," vs ",Var2,"\nr = ",round(r,3)))) +
      geom_tile(color="white") +
      scale_fill_gradient2(low="#2980b9",mid="white",high="#e74c3c",midpoint=0,limits=c(-1,1)) +
      theme_minimal(base_size=11) + theme(axis.text.x=element_text(angle=40,hjust=1)) +
      labs(x=NULL,y=NULL,fill="r")
    ggplotly(p,tooltip="text")})
  
  # ── TAB 5: FEATURE IMPORTANCE ───────────────────────────────────
  output$iv_plot <- renderPlotly({
    sc_col <- c("Useless (< 0.02)"="#95a5a6","Weak (0.02\u20130.10)"="#f39c12",
                "Medium (0.10\u20130.30)"="#3498db","Strong (0.30\u20130.50)"="#27ae60",
                "Very Strong (> 0.50)"="#8e44ad")
    p <- ggplot(iv_summary %>% filter(!is.na(iv)),
                aes(x=reorder(variable,iv),y=iv,fill=strength,
                    text=paste0(variable,"\nIV: ",round(iv,4),"\nStrength: ",strength))) +
      geom_col() + coord_flip() + scale_fill_manual(values=sc_col) +
      geom_hline(yintercept=.10,linetype="dashed",color="gray50",linewidth=.7) +
      geom_hline(yintercept=.30,linetype="dashed",color="#2980b9",linewidth=.7) +
      theme_minimal(base_size=12) + labs(x=NULL,y="Information Value",fill="Strength")
    ggplotly(p,tooltip="text")})
  
  output$iv_guide <- renderTable({
    tibble(`IV Range`=c("< 0.02","0.02\u20130.10","0.10\u20130.30","0.30\u20130.50","> 0.50"),
           Strength=c("Useless","Weak","Medium","Strong","Very Strong"),
           `Use in Model?`=c("No","Caution","Yes","Yes \u2713","Yes \u2713"))
  },striped=TRUE,bordered=TRUE,spacing="s",align="lcl")
  
  output$region_dr <- renderPlotly({
    d <- dat %>% group_by(region) %>%
      summarise(dr=mean(default_flag),n=n(),.groups="drop") %>% mutate(region=as.character(region))
    p <- ggplot(d,aes(x=reorder(region,dr),y=dr,fill=dr,
                      text=paste0(region,"\n",percent(dr,.1)," (n=",comma(n),")"))) +
      geom_col() + geom_hline(yintercept=mean(dat$default_flag),linetype="dashed",color="red",linewidth=.8) +
      coord_flip() + scale_fill_gradient(low="#27ae60",high="#c0392b") +
      scale_y_continuous(labels=percent) + theme_minimal() + theme(legend.position="none") +
      labs(x=NULL,y="Default Rate")
    ggplotly(p,tooltip="text")})
  
  output$purpose_dr <- renderPlotly({
    d <- dat %>% group_by(loan_purpose) %>%
      summarise(dr=mean(default_flag),n=n(),.groups="drop") %>% mutate(loan_purpose=as.character(loan_purpose))
    p <- ggplot(d,aes(x=reorder(loan_purpose,dr),y=dr,fill=dr,
                      text=paste0(loan_purpose,"\n",percent(dr,.1)," (n=",comma(n),")"))) +
      geom_col() + geom_hline(yintercept=mean(dat$default_flag),linetype="dashed",color="red",linewidth=.8) +
      coord_flip() + scale_fill_gradient(low="#27ae60",high="#c0392b") +
      scale_y_continuous(labels=percent) + theme_minimal() + theme(legend.position="none") +
      labs(x=NULL,y="Default Rate")
    ggplotly(p,tooltip="text")})
  
  output$temporal_dr <- renderPlotly({
    d <- dat %>% filter(!is.na(app_year),!is.na(app_month),app_year>=2020,app_year<=2025) %>%
      group_by(app_year,app_month) %>%
      summarise(dr=mean(default_flag),n=n(),.groups="drop") %>%
      mutate(label=paste0(app_month," ",app_year))
    p <- ggplot(d,aes(x=app_month,y=dr,colour=factor(app_year),group=factor(app_year),
                      text=paste0(label,"\nDefault Rate: ",percent(dr,.1),"\nn = ",comma(n)))) +
      geom_line(linewidth=1.2) + geom_point(size=2) +
      scale_y_continuous(labels=percent) + scale_colour_brewer(palette="Set1") +
      theme_minimal() + labs(x=NULL,y="Default Rate",colour="Year")
    ggplotly(p,tooltip="text")})
  
  # ── TAB 6: MODEL PERFORMANCE ────────────────────────────────────
  output$vb_auc_c <- renderValueBox(valueBox(auc_c, "Champion AUC", icon=icon("trophy"),color="green"))
  output$vb_auc_b <- renderValueBox(valueBox(auc_b, "Baseline AUC", icon=icon("chart-line"),color="orange"))
  output$vb_gini  <- renderValueBox(valueBox(gini_c,"Gini (Champion)",icon=icon("star"),color="blue"))
  output$vb_ks    <- renderValueBox(valueBox(ks_stat,"KS Statistic",icon=icon("arrows-left-right"),color="purple"))
  
  # NEW
  # output$roc_plot <- renderPlotly({
  #   ap      <- active_preds()
  #   roc_live <- roc(test_s$default_flag, ap$probs, quiet=TRUE)
  #   live_label <- paste0(ap$label," (AUC=",ap$auc,")")
  #   roc_live_df <- data.frame(
  #     fpr=1-roc_live$specificities,
  #     tpr=roc_live$sensitivities,
  #     model=live_label)
  #   plot_df <- bind_rows(
  #     roc_live_df,
  #     data.frame(fpr=1-roc_b$specificities, tpr=roc_b$sensitivities,
  #                model=base_label))
  #   p <- ggplot(plot_df,aes(x=fpr,y=tpr,colour=model)) +
  #     geom_line(linewidth=1.3) +
  #     geom_abline(slope=1,intercept=0,linetype="dashed",colour="gray50") +
  #     coord_equal() + theme_minimal(base_size=12) +
  #     labs(x="False Positive Rate",y="True Positive Rate",colour=NULL)
  #   ggplotly(p)})
  # 
  
  output$roc_plot <- renderPlotly({
    ap       <- active_preds()
    roc_live <- roc(test_s$default_flag, ap$probs, quiet=TRUE)
    live_label <- paste0(ap$label," (AUC=",ap$auc,")")
    
    roc_live_df <- data.frame(
      fpr=1-roc_live$specificities,
      tpr=roc_live$sensitivities,
      model=live_label
    )
    
    plot_df <- bind_rows(
      roc_live_df,
      data.frame(fpr=1-roc_b$specificities, tpr=roc_b$sensitivities,
                 model=base_label)
    )
    
    p <- ggplot(plot_df, aes(x=fpr, y=tpr, colour=model)) +
      geom_line(linewidth=1.3) +
      geom_abline(slope=1, intercept=0, linetype="dashed", colour="gray50") +
      # Crucial: DO NOT include coord_equal() here as it blocks browser window resizing
      theme_minimal(base_size=12) +
      labs(x="False Positive Rate", y="True Positive Rate", colour=NULL)
    
    # Convert to plotly and pass explicit window resizing options
    ggplotly(p) %>%
      layout(
        autosize = TRUE,
        xaxis = list(range=c(0,1), autorange=FALSE),
        yaxis = list(range=c(0,1), autorange=FALSE),
        uirevision = live_label
      )
  })
  
  output$score_dist_plot <- renderPlotly({
    # NEW
    t  <- input$model_thresh
    ap <- active_preds()
    score_dist_live <- data.frame(
      score  = ap$probs,
      Status = ifelse(test_s$default_flag==1,"Default","No Default"))
    p <- ggplot(score_dist_live,aes(x=score,fill=Status)) +
      geom_density(alpha=.55) +
      geom_vline(xintercept=t,linetype="dashed",
                 colour=if(is_optimal())"#f1c40f"else"black",
                 linewidth=if(is_optimal())2 else 1) +
      scale_fill_manual(values=c("No Default"="#3498db","Default"="#e74c3c")) +
      scale_x_continuous(labels=percent) +
      theme_minimal(base_size=12) +
      labs(x="Predicted Default Probability",y="Density",fill=NULL,
           title=paste0("Threshold = ",t,if(is_optimal())"  \U0001f3c6 OPTIMAL"else""))
    ggplotly(p)})
  
  output$conf_matrix_plot <- renderPlotly({
    t <- input$model_thresh
    ap     <- active_preds()
    pred_c <- factor(ifelse(ap$probs>=t,1,0),levels=c(0,1))
    actual_c <- factor(test_s$default_flag,levels=c(0,1))
    cm_df <- as.data.frame(table(Predicted=pred_c,Actual=actual_c)) %>%
      mutate(fill_c=ifelse(as.character(Predicted)==as.character(Actual),"Correct","Incorrect"),
             Predicted=paste0("Predicted ",Predicted), Actual=paste0("Actual ",Actual))
    fill_vals <- if(is_optimal()) c("Correct"="#f1c40f","Incorrect"="#e74c3c")
    else c("Correct"="#27ae60","Incorrect"="#e74c3c")
    p <- ggplot(cm_df,aes(x=Actual,y=Predicted,fill=fill_c)) +
      geom_tile(color="white",linewidth=1.5) +
      geom_text(aes(label=comma(Freq)),size=10,fontface="bold",color="white") +
      scale_fill_manual(values=fill_vals) +
      theme_minimal(base_size=14) + theme(legend.position="none") + labs(x=NULL,y=NULL)
    ggplotly(p)})
  
  # Replace lines 1609-1619
  output$coef_plot <- renderPlotly({
    p <- ggplot(coef_df, aes(x=reorder(term,estimate), y=estimate, fill=estimate>0)) +
      geom_col(aes(text=paste0(term,"\n\u03b2: ",round(estimate,3),
                               "\nz: ",round(statistic,2),
                               "\np: ",ifelse(p.value<.001,"< 0.001",round(p.value,3))))) +
      geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error),
                    width=.35, colour="black") +
      geom_hline(yintercept=0, linewidth=.5) + coord_flip() +
      scale_fill_manual(values=c("FALSE"="#2980b9","TRUE"="#e74c3c"), guide="none") +
      theme_minimal(base_size=11) + labs(x=NULL, y="Coefficient (log-odds)")
    ggplotly(p, tooltip="text")
  })
  
  output$calib_plot <- renderPlotly({
    p <- ggplot(calib_df,aes(x=mean_pred,y=mean_actual,size=n,
                             text=paste0("Decile ",decile,"\nMean Predicted: ",percent(mean_pred,.1),
                                         "\nActual Default Rate: ",percent(mean_actual,.1),"\nn = ",comma(n)))) +
      geom_abline(slope=1,intercept=0,linetype="dashed",colour="gray50",linewidth=1) +
      geom_point(colour="#27ae60",alpha=.85) +
      geom_line(colour="#27ae60",linewidth=1,aes(group=1)) +
      scale_x_continuous(labels=percent,limits=c(0,NA)) +
      scale_y_continuous(labels=percent,limits=c(0,NA)) +
      scale_size(range=c(3,10)) + theme_minimal(base_size=12) + theme(legend.position="none") +
      labs(x="Mean Predicted Probability",y="Actual Default Rate",title="Reliability Diagram (by decile)")
    ggplotly(p,tooltip="text")})
  
  output$gain_plot <- renderPlotly({
    p <- ggplot(gain_pts, aes(x = cum_n, y = cum_defaults)) +
      geom_ribbon(aes(ymin = cum_n, ymax = cum_defaults), fill = "#8e44ad", alpha = .12) +
      geom_line(aes(text = paste0("Top ", percent(cum_n, .1), " \u2192 Captures ",
                                  percent(cum_defaults, .1), " of defaults\nLift: ",
                                  round(lift, 2))),
                colour = "#8e44ad", linewidth = 1.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "gray50") +
      scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
      theme_minimal(base_size = 12) +
      labs(x = "% of Applicants Screened (high risk first)", y = "% of Defaults Captured",
           title = paste0("Gain Curve  |  AUC = ", auc_c))
    ggplotly(p, tooltip = "text")
  })
  
  output$decile_tbl <- renderDT({
    max_ks_dec <- which.max(decile_df$ks)
    decile_df %>%
      select(Decile=decile,N=n,Defaults=defaults,`Non-Defaults`=non_defaults,
             `Default Rate`=default_rate,`Avg Score`=avg_score,
             `Cum Capture %`=cum_capture,Lift=lift,KS=ks) %>%
      mutate(`Default Rate`=percent(`Default Rate`,.1),`Avg Score`=round(`Avg Score`,3)) %>%
      datatable(rownames=FALSE,options=list(pageLength=10,dom="t"),
                class="table-striped table-bordered table-sm") %>%
      formatStyle("KS",backgroundColor=styleEqual(round(decile_df$ks[max_ks_dec],4),"#fff3cd")) %>%
      formatStyle("Lift",color=styleInterval(c(1,2),c("#e74c3c","#f39c12","#27ae60")))})
  
  # ── TAB 7: SCORECARD ────────────────────────────────────────────
  output$vb_sc_pdo   <- renderValueBox(valueBox(PDO,"PDO (Points to Double Odds)",icon=icon("arrow-up-right-dots"),color="blue"))
  output$vb_sc_base  <- renderValueBox(valueBox(600,"Base Score (at avg odds)",icon=icon("sliders"),color="purple"))
  output$vb_sc_median<- renderValueBox(valueBox(round(median(scorecard_scores)),"Median Score (Test)",icon=icon("chart-simple"),color="green"))
  
  sc_pred_score <- reactive({
    req(input$sc_util,input$sc_dti,input$sc_int,input$sc_age,input$sc_income,input$sc_delinq)
    prof <- med_profile
    prof$credit_utilisation_pct <- input$sc_util
    prof$dti_ratio               <- input$sc_dti
    prof$interest_rate           <- input$sc_int
    prof$age                     <- input$sc_age
    prof$annual_income           <- input$sc_income
    prof$num_delinquencies_2yr   <- input$sc_delinq
    prof$log_income              <- log1p(input$sc_income)
    prof$risk_stress             <- input$sc_int*input$sc_dti
    prof$income_to_loan          <- input$sc_income/(median(dat$loan_amount)+1)
    prof$delinq_flag             <- as.integer(input$sc_delinq>0)
    tryCatch({
      prof_s <- predict(preProc,prof)
      p_out  <- predict(m_champ,newdata=prof_s,type="response")
      lo     <- log(p_out/(1-p_out))
      list(prob=round(p_out*100,1), score=round(A_sc-B_sc*lo))
    },error=function(e)list(prob=NA,score=NA))})
  
  output$vb_sc_calc <- renderValueBox({
    r <- sc_pred_score(); sc <- if(is.na(r$score))"—"else r$score
    valueBox(sc,"Calculator Score",icon=icon("calculator"),color="orange")})
  
  output$sc_calc_display <- renderUI({
    r <- sc_pred_score()
    if(is.na(r$prob)) return(p("Error \u2014 check sidebar inputs.",style="color:red;"))
    risk_col <- if(r$prob>30)"#e74c3c"else if(r$prob>15)"#f39c12"else"#27ae60"
    risk_lbl <- if(r$prob>30)"HIGH RISK"else if(r$prob>15)"MEDIUM RISK"else"LOW RISK"
    tagList(div(style="text-align:center;padding:20px;",
                div(style=paste0("font-size:64px;font-weight:900;color:",risk_col,";line-height:1;margin-bottom:8px;"),r$score),
                div(style="font-size:14px;color:#7f8c8d;margin-bottom:16px;","Scorecard Points"),
                div(style=paste0("display:inline-block;background:",risk_col,";color:white;
        border-radius:20px;padding:6px 18px;font-weight:700;font-size:13px;"),risk_lbl),
                hr(),
                div(style="font-size:18px;font-weight:700;",paste0("Predicted Default Probability: ",r$prob,"%")),
                div(style="font-size:12px;color:#95a5a6;margin-top:8px;","Other features held at test-set median.")))})
  
  output$sc_dist_plot <- renderPlotly({
    p <- ggplot(score_dist_sc,aes(x=score,fill=Status)) +
      geom_density(alpha=.55,adjust=1.2) +
      scale_fill_manual(values=c("No Default"="#3498db","Default"="#e74c3c")) +
      theme_minimal(base_size=13) +
      labs(x="Scorecard Points",y="Density",fill=NULL,title=paste0("Score Separation  |  Gini = ",gini_c))
    ggplotly(p)})
  
  output$sc_prob_plot <- renderPlotly({
    p <- ggplot(score_lookup, aes(x=score, y=prob)) +
      geom_area(fill="#2980b9", alpha=.12) +
      geom_line(aes(text=paste0("Score: ",score,"\nDefault Prob: ",prob,"%")),
                color="#2980b9", linewidth=1.5) +
      geom_vline(xintercept=600, linetype="dashed", color="gray50") +
      scale_y_continuous(labels=function(x) paste0(x,"%")) +
      theme_minimal(base_size=12) +
      labs(x="Scorecard Points", y="Default Probability (%)",
           title="Score \u2192 Default Probability")
    ggplotly(p, tooltip="text")
  })
  
  output$sc_coef_tbl <- renderDT({
    scorecard_coef_df %>%
      mutate(`β (log-odds)` = round(`β (log-odds)`, 4)) %>%
      datatable(rownames=FALSE,options=list(pageLength=8,dom="tip", pagingType="simple"),
                class="table-striped table-bordered table-sm") %>%
      formatStyle("Direction",
                  color=styleEqual(c("Increases Risk \u25b2","Decreases Risk \u25bc"),c("#e74c3c","#27ae60"))) %>%
      formatStyle("Points per SD",
                  background=styleColorBar(range(scorecard_coef_df$`Points per SD`),"#f39c12"),
                  backgroundSize="100% 80%",backgroundRepeat="no-repeat",backgroundPosition="center")})
  
  output$sc_decile_tbl <- renderDT({
    decile_df %>%
      select(Decile=decile,N=n,Defaults=defaults,`Default Rate`=default_rate,
             `Avg Prob`=avg_score,`Cum Capture %`=cum_capture,Lift=lift,KS=ks) %>%
      mutate(`Default Rate`=percent(`Default Rate`,.1),`Avg Prob`=round(`Avg Prob`,4)) %>%
      datatable(rownames=FALSE,options=list(pageLength=10,dom="t"),
                class="table-striped table-bordered table-sm") %>%
      formatStyle("Lift",backgroundColor=styleInterval(c(1,2),c("#f8d7da","#fff3cd","#d4edda"))) %>%
      formatStyle("KS",fontWeight="bold")})
  
  # ================================================================
  # TAB 8: ADVERSE ACTION — COEFFICIENT ATTRIBUTION
  # ================================================================
  selected_app <- reactive({
    req(input$aa_applicant)
    idx <- as.integer(input$aa_applicant)
    list(
      idx        = idx,
      prob       = p_champ[idx],
      score      = scorecard_scores[idx],
      decision   = if(p_champ[idx] >= input$aa_thresh) "DECLINED" else "APPROVED",
      actual     = test_s$default_flag[idx],
      row_s      = test_s[idx, , drop=FALSE],
      row_raw    = test_dat[idx, , drop=FALSE]
    )})
  
  aa_contribs <- reactive({
    app <- selected_app()
    get_contributions(app$row_s, m_champ)})
  
  # Value boxes
  output$aa_score <- renderValueBox({
    app <- selected_app()
    col <- if(app$decision=="DECLINED")"red"else"green"
    valueBox(app$score,"Scorecard Points",icon=icon("hashtag"),color=col)})
  output$aa_prob  <- renderValueBox({
    app <- selected_app()
    valueBox(paste0(round(app$prob*100,1),"%"),"Default Probability",
             icon=icon("percent"),color=if(app$prob>.30)"red"else if(app$prob>.15)"orange"else"green")})
  output$aa_decision <- renderValueBox({
    app <- selected_app()
    if(app$decision=="DECLINED")
      valueBox("\u274c DECLINED","Model Decision",icon=icon("ban"),color="red")
    else
      valueBox("\u2705 APPROVED","Model Decision",icon=icon("check-circle"),color="green")})
  output$aa_top_risk <- renderValueBox({
    contribs <- aa_contribs()
    if(is.null(contribs)) return(valueBox("N/A","Top Risk Factor",icon=icon("question"),color="red"))
    top_factor <- contribs %>% filter(contribution>0) %>% slice(1)
    label <- if(nrow(top_factor)>0) top_factor$label[1] else "None"
    valueBox(label,"#1 Risk Factor",icon=icon("triangle-exclamation"),color="orange")})
  
  # ── Waterfall Chart ──────────────────────────────────────────────
  output$aa_waterfall <- renderPlotly({
    contribs <- aa_contribs()
    req(!is.null(contribs))
    app <- selected_app()
    
    # Add intercept as baseline
    intercept_val <- coef(m_champ)["(Intercept)"]
    baseline_row  <- data.frame(group="(Baseline)",label="Population Baseline (Intercept)",
                                contribution=intercept_val,direction="baseline",
                                reason="The population-average log-odds before any features are considered.",
                                stringsAsFactors=FALSE)
    
    plot_df <- bind_rows(baseline_row, contribs) %>%
      arrange(desc(abs(contribution))) %>%
      slice_head(n=22) %>%   # top 20 features + baseline
      mutate(
        bar_col = case_when(
          direction=="baseline" ~ "#7f8c8d",
          contribution > 0      ~ "#e74c3c",
          TRUE                  ~ "#27ae60"),
        short_label = ifelse(nchar(label)>28, paste0(substr(label,1,25),"..."), label),
        raw_val    = sapply(group, function(g) format_raw_value(g, app$row_raw)),
        hover_text = paste0(
          "<b>", label, "</b><br>",
          ifelse(direction != "baseline",
                 paste0("Applicant value: <b>", raw_val, "</b><br>"), ""),
          "Log-odds contribution: ",
          ifelse(contribution >= 0, "+", ""), round(contribution, 4), "<br>",
          "Direction: ", ifelse(contribution > 0, "\u25b2 Increases risk",
                                ifelse(direction == "baseline", "Population baseline",
                                       "\u25bc Reduces risk"))))
    
    # Sort: baseline first, then by contribution magnitude
    plot_df <- plot_df %>%
      mutate(order_val = ifelse(direction=="baseline", 1e9, abs(contribution))) %>%
      arrange(order_val) %>%
      mutate(short_label=factor(short_label, levels=short_label))
    
    plot_ly(plot_df,
            x = ~contribution,
            y = ~short_label,
            type = "bar",
            orientation = "h",
            marker = list(color=~bar_col,
                          line=list(color="rgba(255,255,255,0.3)",width=0.8)),
            text = ~paste0(ifelse(contribution>0,"+",""),round(contribution,3)),
            textposition = "outside",
            hovertext = ~hover_text,
            hoverinfo = "text") %>%
      layout(
        xaxis = list(title="Contribution to Log-Odds (Risk Score)",
                     zeroline=TRUE,zerolinecolor="#bdc3c7",
                     gridcolor="rgba(0,0,0,0.05)",tickfont=list(size=11)),
        yaxis = list(title="",tickfont=list(size=11)),
        plot_bgcolor  = "#f8f9fa",
        paper_bgcolor = "white",
        margin = list(l=20,r=60,t=10,b=40),
        shapes = list(list(type="line",x0=0,x1=0,y0=-0.5,y1=nrow(plot_df)-0.5,
                           line=list(color="#2c3e50",width=2))),
        annotations = list(list(
          x=app$prob, y=nrow(plot_df),
          text=paste0("<b>Final: ",round(app$prob*100,1),"% default probability<br>Score: ",app$score,"</b>"),
          showarrow=FALSE,bgcolor="#2c3e50",font=list(color="white",size=11),
          borderpad=4,bordercolor="#2c3e50"))) %>%
      config(displayModeBar=FALSE)})
  
  # ── Adverse Action Notice ────────────────────────────────────────
  output$aa_notice_title <- renderUI({
    app <- selected_app()
    if(app$decision=="DECLINED")
      HTML('<span style="color:#ecf0f1;">\u274c Adverse Action Notice</span>')
    else
      HTML('<span style="color:#27ae60;">\u2705 Conditional Approval Notice</span>')})
  
  output$aa_notice <- renderUI({
    contribs <- aa_contribs()
    req(!is.null(contribs))
    app <- selected_app()
    
    if(app$decision=="DECLINED"){
      top3 <- contribs %>% filter(contribution>0) %>% slice_head(n=3)
      protective <- contribs %>% filter(contribution<0) %>% arrange(contribution) %>% slice_head(n=2)
      
      # Path to approval tips
      path_tips <- c(
        "Credit Utilisation Ratio"        = "Reduce outstanding credit card balances below 30% of limits.",
        "Debt-to-Income Ratio"            = "Pay down existing debt or increase verified income.",
        "Delinquencies (Past 2 Years)"    = "Maintain a clean payment record for 12+ consecutive months.",
        "Loan Amount"                     = "Consider applying for a smaller loan amount.",
        "Loan Interest Rate"              = "Improve your overall credit profile to qualify for better rates.",
        "Interest \u00d7 Debt Stress Index" = "Simultaneously reduce debt balance and interest costs.",
        "Hard Credit Inquiries (6 months)" = "Avoid applying for new credit for at least 6 months.",
        "Credit Utilisation Exceeds 80%"  = "Reduce credit utilisation below 30% as a priority.",
        "Income Level"                    = "Provide additional documented income or a guarantor.",
        "Employment Tenure"               = "Re-apply after achieving 12+ months at current employer."
      )
      
      tagList(
        div(class="aa-decision-declined",
            div(class="aa-header", style="color:#e74c3c;","\u274c LOAN APPLICATION: DECLINED"),
            div(class="aa-subtext","Your application has been declined based on the following principal reasons.
            You have the right to request a copy of your credit report."),
            
            tags$hr(style="border-color:#333;"),
            tags$p(tags$b(style="color:#ecf0f1;font-size:13px;","PRIMARY ADVERSE ACTION REASONS:")),
            
            if(nrow(top3)>=1){
              div(class="reason-card",
                  div(class="reason-clearfix",
                      span(class="reason-number","01"),
                      div(style="overflow:hidden;",
                          span(class="reason-label", top3$label[1]),
                          span(class="reason-detail", top3$reason[1]),
                          span(style="font-size:11px;color:#e74c3c;font-weight:700;",
                               paste0("Your ", tolower(top3$label[1]), ": ",
                                      format_raw_value(top3$group[1], app$row_raw))))))},
            
            if(nrow(top3)>=2){
              div(class="reason-card",
                  div(class="reason-clearfix",
                      span(class="reason-number","02"),
                      div(style="overflow:hidden;",
                          span(class="reason-label", top3$label[2]),
                          span(class="reason-detail", top3$reason[2]),
                          span(style="font-size:11px;color:#e74c3c;font-weight:700;",
                               paste0("Your ", tolower(top3$label[2]), ": ",
                                      format_raw_value(top3$group[2], app$row_raw))))))},
            
            if(nrow(top3)>=3){
              div(class="reason-card",
                  div(class="reason-clearfix",
                      span(class="reason-number","03"),
                      div(style="overflow:hidden;",
                          span(class="reason-label", top3$label[3]),
                          span(class="reason-detail", top3$reason[3]),
                          span(style="font-size:11px;color:#e74c3c;font-weight:700;",
                               paste0("Your ", tolower(top3$label[3]), ": ",
                                      format_raw_value(top3$group[3], app$row_raw))))))},
            
            if(nrow(protective)>0) tags$hr(style="border-color:#333;"),
            if(nrow(protective)>0)
              tags$p(tags$b(style="color:#27ae60;font-size:12px;","\u2705 Factors in your favour:")),
            lapply(seq_len(min(nrow(protective),2)), function(i){
              div(class="protective-card",
                  paste0("\u2714 ", protective$label[i], " \u2014 Your value: ",
                         format_raw_value(protective$group[i], app$row_raw)))
            }),
            
            tags$hr(style="border-color:#333;"),
            tags$p(tags$b(style="color:#f1c40f;font-size:12px;","\U0001f4a1 Path to Approval:")),
            lapply(seq_len(min(nrow(top3),3)), function(i){
              tip_label <- top3$label[i]
              tip <- if(tip_label %in% names(path_tips)) path_tips[tip_label]
              else "Improve the metrics highlighted above."
              div(class="path-tip", paste0("\u2192 ", tip))
            })
        )
      )
    } else {
      # APPROVED notice
      div(class="aa-decision-approved",
          div(class="aa-header",style="color:#27ae60;","\u2705 APPLICATION: CONDITIONALLY APPROVED"),
          div(class="aa-subtext",paste0(
            "Your application meets our risk criteria at the current threshold (",
            round(input$aa_thresh,3),"). Predicted default probability: ",
            round(app$prob*100,1),"%. Scorecard: ",app$score," points.")),
          hr(style="border-color:#333;"),
          p(style="color:#bdc3c7;font-size:13px;",
            "No adverse action notice is required. Subject to final income verification and documentation.")
      )
    }
  })
  
  # ── Applicant Profile Table ──────────────────────────────────────
  output$aa_profile_tbl <- renderDT({
    app <- selected_app()
    raw_row <- app$row_raw
    
    display_vars <- c("age","annual_income","employment_length_years","credit_utilisation_pct",
                      "dti_ratio","interest_rate","loan_amount","num_delinquencies_2yr",
                      "num_hard_inquiries_6mo","total_revolving_balance","months_since_last_delinquency",
                      "pct_accounts_current","months_at_current_address","home_ownership",
                      "loan_purpose","region","phone_verified")
    avail_vars <- display_vars[display_vars %in% names(raw_row)]
    
    profile_data <- data.frame(
      Feature   = avail_vars,
      Applicant = sapply(avail_vars, function(v) as.character(raw_row[[v]])[1]),
      `Pop. Mean/Mode` = sapply(avail_vars, function(v) {
        col <- dat[[v]]
        if(is.numeric(col)) round(mean(col,na.rm=TRUE),2)
        else names(sort(table(col),decreasing=TRUE))[1]
      }),
      stringsAsFactors = FALSE
    )
    
    datatable(profile_data, rownames=FALSE,
              options=list(pageLength=18,dom="t",scrollY="360px"),
              class="table-striped table-sm table-bordered") %>%
      formatStyle("Applicant",fontWeight="bold",color="#2980b9")
  })
  
  # ── AI Adverse Action Letter ─────────────────────────────────────
  aa_letter_val <- reactiveVal(NULL)
  
  observeEvent(input$gen_aa_letter, {
    aa_letter_val(NULL)
    app      <- selected_app()
    contribs <- aa_contribs()
    req(!is.null(contribs))
    
    top3 <- contribs %>% filter(contribution>0) %>% slice_head(n=3)
    protective <- contribs %>% filter(contribution<0) %>% slice_head(n=2)
    
    context <- paste0(
      "APPLICANT CREDIT DECISION DETAILS\n",
      "Decision: ", app$decision, "\n",
      "Scorecard Points: ", app$score, "\n",
      "Predicted Default Probability: ", round(app$prob*100,1), "%\n",
      "Decision Threshold: ", round(input$aa_thresh,3), "\n\n",
      "TOP 3 ADVERSE ACTION REASONS (with actual applicant values):\n",
      paste0(seq_len(nrow(top3)), ". ", top3$label,
             " — Applicant value: ", sapply(top3$group, format_raw_value, raw_row=app$row_raw),
             "\n", collapse=""),
      "\nFACTORS IN APPLICANT'S FAVOUR:\n",
      paste0("- ", protective$label,
             " — Applicant value: ", sapply(protective$group, format_raw_value, raw_row=app$row_raw),
             "\n", collapse=""),
      "\nNOTE: Do NOT mention log-odds or model scores in the letter. ",
      "Use plain English referencing the actual values above."
    )
    
    tryCatch({
      agent <- chat_mistral(model="mistral-small",
                            system_prompt=paste0(
                              "You are a Compliance Officer writing regulatory-compliant adverse action notices. ",
                              "Follow ECOA (Equal Credit Opportunity Act) and FCRA standards. ",
                              "Be professional, empathetic, and precise. Use plain English. ",
                              "Structure: (1) Decision statement, (2) Principal reasons (numbered), ",
                              "(3) FCRA rights, (4) Credit bureau contact info, (5) Closing."))
      response <- agent$chat(paste0(
        "Write a formal adverse action notice for the following loan application decision.\n\n",
        context,
        "\n\nThe letter must:\n",
        "1. Open with a clear statement of the decision\n",
        "2. List exactly the top 3 specific reasons (using the names provided)\n",
        "3. Include FCRA consumer rights statement (right to obtain free credit report within 60 days)\n",
        "4. Include a credit bureau contact line (use generic placeholder)\n",
        "5. Close professionally\n",
        "6. Be 200-300 words total — regulatory conciseness is required\n",
        "Use ## Markdown headers for each section."))
      aa_letter_val(response)
    }, error=function(e) aa_letter_val(paste0("**AI Error:** ", e$message,
                                              "\n\nEnsure MISTRAL_API_KEY is set in secrets.R and ellmer is installed.")))
  })
  
  output$aa_letter_output <- renderUI({
    letter <- aa_letter_val()
    if(is.null(letter))
      return(div(style="color:#7f8c8d;font-size:12px;padding:10px 0;",
                 "Click the button above to generate an AI-written adverse action letter for this applicant."))
    div(style="background:#f8f9fa;border:1px solid #dee2e6;border-radius:8px;padding:16px;
               font-size:12px;max-height:420px;overflow-y:auto;",
        HTML(markdown_html(letter)))
  })
  
  
  # ================================================================
  # PROFIT SIMULATOR
  # ================================================================
  biz_profit_df <- reactive({
    req(input$biz_revenue, input$biz_loss)
    rev <- input$biz_revenue; los <- input$biz_loss
    biz_df %>%
      mutate(
        gross_revenue = (tn + fn) * rev,
        default_loss  = fn * los,
        net_profit    = gross_revenue - default_loss
      )
  })
  
  biz_profit_curr <- reactive({
    req(input$biz_thresh)
    biz_profit_df() %>%
      filter(abs(threshold - input$biz_thresh) == min(abs(threshold - input$biz_thresh))) %>%
      slice(1)
  })
  
  output$profit_plot <- renderPlotly({
    df  <- biz_profit_df()
    cr  <- biz_profit_curr()
    opt <- df %>% filter(net_profit == max(net_profit)) %>% slice(1)
    p <- ggplot(df, aes(x=threshold, y=net_profit,
                        text=paste0("Threshold: ",threshold,
                                    "\nNet Profit: R",comma(round(net_profit)),
                                    "\nApproval Rate: ",percent(approved_pct,.1),
                                    "\nDefault Rate: ",percent(default_rate_approved,.1)))) +
      geom_line(color="#27ae60", linewidth=1.4) +
      geom_hline(yintercept=0, linetype="solid", color="gray40", linewidth=.5) +
      geom_vline(xintercept=cr$threshold,  linetype="dashed", color="#2980b9",  linewidth=1.1) +
      geom_vline(xintercept=opt$threshold, linetype="dotted", color="#f1c40f",  linewidth=1.1) +
      geom_point(data=cr,  aes(x=threshold,y=net_profit), color="#2980b9", size=5, inherit.aes=FALSE) +
      geom_point(data=opt, aes(x=threshold,y=net_profit), color="#f1c40f", size=5, inherit.aes=FALSE) +
      scale_y_continuous(labels=comma) +
      theme_minimal(base_size=12) +
      labs(x="Decision Threshold", y="Net Profit (R)",
           title=paste0("Profit-maximising threshold: ",opt$threshold,
                        "  \u2192  Max profit: R",comma(round(opt$net_profit))))
    ggplotly(p, tooltip="text")
  })
  
  output$biz_net_profit <- renderValueBox({
    cr  <- biz_profit_curr()
    col <- if(cr$net_profit > 0) "green" else "red"
    valueBox(paste0("R ",comma(round(cr$net_profit))), "Net Portfolio Profit",
             icon=icon("coins"), color=col)
  })
  output$biz_gross_revenue <- renderValueBox({
    cr <- biz_profit_curr()
    valueBox(paste0("R ",comma(round(cr$gross_revenue))), "Gross Revenue",
             icon=icon("arrow-trend-up"), color="blue")
  })
  output$biz_total_loss <- renderValueBox({
    cr <- biz_profit_curr()
    valueBox(paste0("R ",comma(round(cr$default_loss))), "Default Losses",
             icon=icon("triangle-exclamation"), color="orange")
  })
  
  # ── AI Scenario Advisor ──────────────────────────────────────────
  biz_ai_text <- reactiveVal(NULL)
  
  output$biz_ai_output <- renderUI({
    txt <- biz_ai_text()
    if(is.null(txt)) return(NULL)
    if(txt == "__thinking__")
      return(div(style="padding:10px;color:#27ae60;font-size:13px;",
                 "\U0001f916 Analysing scenario..."))
    div(style="background:#1a252f;border-left:3px solid #27ae60;border-radius:6px;
               padding:12px 16px;font-size:12px;margin:10px 0 4px;color:#ecf0f1;",
        HTML(markdown_html(txt)))
  })
  
  observeEvent(input$biz_ai_advice, {
    biz_ai_text("__thinking__")
    cr   <- biz_profit_curr()
    curr <- biz_curr_row()
    opt  <- biz_profit_df() %>% filter(net_profit == max(net_profit)) %>% slice(1)
    
    thresh_gap <- round(opt$threshold - best_thresh, 3)
    thresh_direction <- if(thresh_gap > 0) "higher" else "lower"
    thresh_reason <- if(thresh_gap > 0)
      paste0("At R", input$biz_revenue, " revenue and R", input$biz_loss,
             " loss, the penalty for missed defaults is severe enough to justify tighter screening than the statistical optimum.")
    else
      paste0("At R", input$biz_revenue, " revenue and R", input$biz_loss,
             " loss, the revenue opportunity from approving more loans outweighs the incremental default cost.")
    
    ctx <- paste0(
      "=== FULL DASHBOARD SNAPSHOT ===\n\n",
      
      "-- Model Stats --\n",
      "Champion AUC: ", auc_c, " | Baseline AUC: ", auc_b, "\n",
      "Gini: ", gini_c, " | KS Statistic: ", ks_stat, "\n",
      "Population default rate: ", round(mean(dat$default_flag)*100, 1), "%\n\n",
      
      "-- Current Threshold: ", cr$threshold, " --\n",
      "Approval Rate: ",              percent(curr$approved_pct, .1), "\n",
      "Default Rate (Approved): ",    percent(curr$default_rate_approved, .1), "\n",
      "Precision: ",                  percent(curr$precision, .1), "\n",
      "Recall: ",                     percent(curr$recall, .1), "\n",
      "F1 Score: ",                   round(curr$f1, 3), "\n",
      "Accuracy: ",                   percent(curr$accuracy, .1), "\n\n",
      
      "-- Profit Simulator (test-set scale) --\n",
      "Revenue assumption per approved loan: R", input$biz_revenue, "\n",
      "Loss assumption per default: R",          input$biz_loss, "\n",
      "Gross Revenue: R",   comma(round(cr$gross_revenue)), "\n",
      "Default Losses: R",  comma(round(cr$default_loss)),  "\n",
      "Net Profit: R",      comma(round(cr$net_profit)),     "\n\n",
      
      "-- Threshold Comparison --\n",
      "Youden-optimal threshold (statistical): ", round(best_thresh, 3), "\n",
      "Profit-maximising threshold (business): ", opt$threshold, "\n",
      "Max achievable net profit: R", comma(round(opt$net_profit)), "\n",
      "Current net profit gap vs optimal: R", comma(round(opt$net_profit - cr$net_profit)), "\n",
      "Profit-optimal is ", abs(thresh_gap), " ", thresh_direction, " than Youden-optimal.\n",
      "Why they differ: ", thresh_reason, "\n\n",
      
      "-- Risk Signals --\n",
      "Default rate among approved loans vs population: ",
      round(curr$default_rate_approved / mean(dat$default_flag), 2), "x ratio\n",
      "Approved loan count (test set): ", curr$approved, "\n",
      "Defaulters slipping through (FN): ", curr$fn, "\n",
      "Good customers wrongly declined (FP): ", curr$fp
    )
    
    tryCatch({
      agent <- chat_mistral(
        model = "mistral-small",
        system_prompt = paste0(
          AI_SYSTEM_PROMPT,
          "\n\n=== BUSINESS DASHBOARD ADVISOR ROLE ===\n",
          "You are now advising a credit risk manager reviewing the full Business Decision Dashboard.\n",
          "You have access to ALL dashboard metrics: approval rate, default rate, precision, recall, ",
          "the Volume vs Risk trade-off, Precision-Recall curve, Lorenz/Gini concentration, and the profit simulator.\n\n",
          "CRITICAL — THRESHOLD RECONCILIATION:\n",
          "The Youden-optimal threshold (", round(best_thresh, 3), ") is the STATISTICAL optimum — it maximises ",
          "TPR minus FPR treating all errors as equally costly.\n",
          "The PROFIT-maximising threshold is a BUSINESS optimum — it maximises net revenue given the user's ",
          "revenue-per-loan and loss-per-default assumptions. These WILL differ.\n",
          "When a user's profit-optimal threshold differs from Youden, ALWAYS explain WHY ",
          "(e.g. 'the profit model is telling you to tighten/loosen because default losses ",
          "dominate/revenue dominates at your current assumptions'). Never let the user think ",
          "something is broken — it is working correctly, just optimising a different objective.\n\n",
          "DANGER FLAGS TO CALL OUT:\n",
          "- Default rate among approved > 2x population default rate = dangerous\n",
          "- Recall < 40% = catching less than half of all defaulters\n",
          "- Approval rate > 80% at high default rates = volume trap\n",
          "- Net profit negative = current threshold is destroying value\n",
          "- Large gap (> R50k) between current and optimal profit = major opportunity cost\n\n",
          "FORMAT: Respond in 4 short sections with bold headers: ",
          "**Risk Assessment**, **Threshold Guidance**, **Profit Insight**, **Recommendation**. ",
          "Each section max 2 sentences. Be specific with numbers from the snapshot."
        )
      )
      resp <- agent$chat(paste0(
        "Analyse the full dashboard snapshot below and give your structured assessment:\n\n", ctx))
      biz_ai_text(resp)
    }, error = function(e) {
      biz_ai_text(paste0("**AI unavailable:** ", e$message,
                         "\n\nEnsure MISTRAL_API_KEY is set in secrets.R."))
    })
  })
  
  # ================================================================
  # TAB 9: BUSINESS DASHBOARD
  # ================================================================
  biz_curr_row <- reactive({
    req(input$biz_thresh)
    biz_df %>%
      filter(abs(threshold-input$biz_thresh)==min(abs(threshold-input$biz_thresh))) %>%
      slice(1)})
  
  output$biz_approved  <- renderValueBox(valueBox(percent(biz_curr_row()$approved_pct[1],.1),
                                                  "Approval Rate",icon=icon("check-circle"),color="green"))
  output$biz_dr        <- renderValueBox(valueBox(percent(biz_curr_row()$default_rate_approved[1],.1),
                                                  "Default Rate (Approved)",icon=icon("triangle-exclamation"),color="red"))
  output$biz_precision <- renderValueBox(valueBox(percent(biz_curr_row()$precision[1],.1),
                                                  "Precision",icon=icon("crosshairs"),color="blue"))
  output$biz_recall    <- renderValueBox(valueBox(percent(biz_curr_row()$recall[1],.1),
                                                  "Recall",icon=icon("bell"),color="orange"))
  
  output$vol_risk_plot <- renderPlotly({
    cr <- biz_curr_row()
    p  <- ggplot(biz_df,aes(x=approved_pct,y=default_rate_approved,
                            text=paste0("Threshold: ",threshold,"\nApproval Rate: ",percent(approved_pct,.1),
                                        "\nDefault Rate (Approved): ",percent(default_rate_approved,.1)))) +
      geom_line(color="#2980b9",linewidth=1.3) +
      geom_point(data=cr,aes(x=approved_pct,y=default_rate_approved),
                 color="red",size=5,inherit.aes=FALSE) +
      geom_hline(yintercept=mean(dat$default_flag),linetype="dashed",color="gray50",linewidth=.7) +
      scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent) +
      theme_minimal(base_size=13) +
      labs(x="Approval Rate",y="Default Rate Among Approved",
           title=paste0("Threshold = ",input$biz_thresh," \u2192 Approving ",percent(cr$approved_pct[1],.1)))
    ggplotly(p,tooltip="text")})
  
  output$prec_recall_plot <- renderPlotly({
    cr <- biz_curr_row()
    p  <- ggplot(biz_df,aes(x=recall,y=precision,
                            text=paste0("Threshold: ",threshold,"\nPrecision: ",percent(precision,.1),
                                        "\nRecall: ",percent(recall,.1)))) +
      geom_line(color="#9b59b6",linewidth=1.3) +
      geom_point(data=cr,aes(x=recall,y=precision),color="red",size=5,inherit.aes=FALSE) +
      scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent) +
      theme_minimal(base_size=13) + labs(x="Recall",y="Precision")
    ggplotly(p,tooltip="text")})
  
  output$lorenz_plot <- renderPlotly({
    p <- ggplot(lorenz_pts,aes(x=cum_pop,y=cum_default)) +
      geom_line(color="#e74c3c",linewidth=1.5) +
      geom_abline(slope=1,intercept=0,linetype="dashed",color="gray60") +
      geom_ribbon(aes(ymin=cum_pop,ymax=cum_default),fill="#e74c3c",alpha=.12) +
      scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent) +
      theme_minimal(base_size=13) +
      labs(x="% of Population (high risk \u2192 low risk)",y="% of Defaults Captured",
           title=paste0("Lorenz Curve   |   Gini = ",gini_c))
    ggplotly(p)})
  
  # ================================================================
  # COMPLETE FIXED TAB 10 SERVER SECTION — v3 (final)
  # ================================================================
  
  # ================================================================
  # TAB 10: AI CREDIT ANALYST CHAT
  # ================================================================
  library(later)
  
  ai_agent_obj   <- reactiveVal(NULL)
  chat_history   <- reactiveVal(list())
  ai_is_thinking <- reactiveVal(FALSE)
  
  quick_prompts <- list(
    explain_model         = "Explain our champion logistic regression credit model: how it works, what makes it better than the baseline, and why logistic regression is appropriate here.",
    worst_segment         = "Which risk segment in our portfolio is the most dangerous? Identify the combination of region, loan purpose, and key features that drive the highest default concentrations.",
    optimal_threshold     = paste0("Our Youden optimal threshold is ", round(best_thresh, 3), ". Explain what this means, and recommend whether we should set it higher or lower depending on different business objectives."),
    feature_engineering   = "Explain the rationale behind each of our feature engineering decisions: log transforms, risk_stress interaction, high_util_flag, income_to_loan ratio, the delinquency treatment, and the natural cubic spline on age.",
    regulatory            = "Which features in our model might draw regulatory scrutiny under fair lending laws (ECOA, FCRA)? What is a protected class and how does it relate to credit modelling?",
    champion_vs_baseline  = paste0("Compare our champion model (AUC=", auc_c, ") against the baseline (AUC=", auc_b, "). Quantify the lift and explain what Gini (", gini_c, ") and KS (", ks_stat, ") mean in practical credit terms."),
    portfolio_strategy    = paste0("Based on our model (AUC=", auc_c, ", Gini=", gini_c, ", default rate=", round(mean(dat$default_flag)*100,1), "%), recommend a portfolio strategy for a retail lender."),
    adverse_action_letter = "Write a sample ECOA/FCRA-compliant adverse action letter for a declined applicant whose top 3 denial reasons are: (1) High credit utilisation, (2) Elevated DTI, (3) Two delinquencies in the past 24 months."
  )
  
  observeEvent(input$quick_prompt, {
    req(input$quick_prompt %in% names(quick_prompts))
    updateTextAreaInput(session, "ai_input", value = quick_prompts[[input$quick_prompt]])
  })
  
  observeEvent(input$clear_chat, {
    chat_history(list())
    ai_agent_obj(NULL)
    ai_is_thinking(FALSE)
  })
  
  observeEvent(input$send_ai, {
    msg <- trimws(input$ai_input)
    req(nchar(msg) > 3)
    req(!isTRUE(ai_is_thinking()))
    
    updateTextAreaInput(session, "ai_input", value = "")
    
    # All reactive READS happen here (inside observeEvent = valid reactive context)
    hist_with_user <- c(chat_history(), list(list(role = "user", content = msg)))
    chat_history(hist_with_user)   # write: show user message immediately
    ai_is_thinking(TRUE)           # write: show spinner immediately
    
    # Snapshot for the closure — no reactive reads inside later::later
    local_hist <- hist_with_user
    local_msg  <- msg
    
    # Build or reuse agent (must stay on main thread — R6 object, not serialisable)
    agent <- ai_agent_obj()
    if (is.null(agent)) {
      agent <- tryCatch(
        chat_mistral(model = "mistral-small", system_prompt = AI_SYSTEM_PROMPT),
        error = function(e) NULL
      )
      ai_agent_obj(agent)
    }
    
    if (is.null(agent)) {
      chat_history(c(local_hist, list(list(
        role    = "assistant",
        content = "**Connection Error:** Could not initialise the AI agent. Please ensure `MISTRAL_API_KEY` is set in `secrets.R`."
      ))))
      ai_is_thinking(FALSE)
      return()
    }
    
    local_agent <- agent   # close over reference — later stays on main thread
    
    # Defer the blocking HTTP call; 50ms gives Shiny time to flush the spinner
    later::later(function() {
      response <- tryCatch(
        local_agent$chat(local_msg),
        error = function(e) paste0(
          "**API Error:** ", e$message,
          "\n\nCheck your MISTRAL_API_KEY and network connection."
        )
      )
      # Only WRITES here — legal outside reactive context
      chat_history(c(local_hist, list(list(role = "assistant", content = response))))
      ai_is_thinking(FALSE)
    }, delay = 0.05)
    
    NULL
  })
  
  # renderUI — returns ONLY tag objects, no side effects
  output$chat_history_ui <- renderUI({
    hist     <- chat_history()
    thinking <- ai_is_thinking()
    
    # Empty state
    if (length(hist) == 0 && !thinking) {
      return(
        div(style = "padding:40px;text-align:center;flex:1;display:flex;
                     flex-direction:column;align-items:center;justify-content:center;",
            div(style = "font-size:40px;margin-bottom:12px;", "\U0001f916"),
            div(style = "font-size:16px;font-weight:700;color:#ecf0f1;margin-bottom:8px;",
                "AI Credit Analyst"),
            div(style = "font-size:13px;color:#7f8c8d;",
                "I have full knowledge of your DataQuest 2026 model.", br(),
                "Ask me anything or click a Quick Prompt above to get started.")
        )
      )
    }
    
    # Message bubbles — NO floats, flexbox parent handles alignment
    msgs <- lapply(hist, function(m) {
      if (m$role == "user") {
        div(class = "chat-msg-user", m$content)
      } else {
        rendered <- tryCatch(
          HTML(markdown_html(m$content)),
          error = function(e) pre(m$content)   # plain-text fallback
        )
        div(class = "chat-msg-ai", rendered)
      }
    })
    
    if (thinking) {
      msgs <- c(msgs, list(
        div(class = "ai-typing", "\U0001f916 AI is thinking...")
      ))
    }
    
    do.call(tagList, msgs)
  })
  
  # Auto-scroll — separate observe, never inside renderUI
  observe({
    req(length(chat_history()) > 0 || isTRUE(ai_is_thinking()))
    chat_history()
    ai_is_thinking()
    runjs("
      setTimeout(function() {
        var el = document.getElementById(\'chat_scroll\');
        if (el) el.scrollTop = el.scrollHeight;
      }, 150);
    ")
  })
  
  
} # end server

shinyApp(ui, server)
