library(dplyr)
library(lubridate)
library(duckdb)
library(ggplot2)
library(ClinicalUtilityRecal)

here::i_am('prevents-model-validation.Rproj')

# Output folder
dir.create('figures', showWarnings = FALSE)

# Custom functions for DRY analysis
source('utils.R')


####################
# Analytic datasets
#
####################
con <- dbConnect(duckdb(), 'data/prevents_data.duckdb')

pce <- dbGetQuery(con, "
        select *
        from pce_first_eligible_encounter_with_predictors
        where
          raceth in ('nh black', 'nh white')
          and sex in ('female', 'male')
")


prevents_ascvd <- dbGetQuery(con, "
        select *
        from prevents_first_ascvd_eligible_encounter_with_predictors
        where sex in ('female', 'male')
")


prevents_total_cvd <- dbGetQuery(con, "
        select *
        from prevents_first_total_cvd_eligible_encounter_with_predictors
        where sex in ('female', 'male')
")



# Disconnect to avoid "File is already open ..." error on re-run
dbDisconnect(con)

# Add model-based risk predictions
pce <- calculate_pce_risks(pce)
prevents_ascvd <- calculate_prevents_ascvd_risks(prevents_ascvd)
prevents_total_cvd <- calculate_prevents_total_cvd_risks(prevents_total_cvd)



################################################################################
#                               DEMOGRAPHICS
#
################################################################################

# PCE
table1_pce <- create_demographics_table(
  data = pce %>% mutate(egfr = NA_real_),
  outcome_field = ascvd_5yr_flag,
  outcome_label = 'ASCVD',
  save_path = 'figures/table1-pce-ascvd.docx')


# PREVENTS (ASCVD)
table1_prevents_ascvd <- create_demographics_table(
  data = prevents_ascvd,
  outcome_field = ascvd_10yr_flag, 
  outcome_label = 'ASCVD',
  save_path = 'figures/table1-prevents-ascvd.docx')


# PREVENTS (Total CVD)
table1_prevents_total_cvd <- create_demographics_table(
  data = prevents_total_cvd,
  outcome_field = total_cvd_10yr_flag, 
  outcome_label = 'Total CVD',
  save_path = 'figures/table1-prevents-total-cvd.docx')



################################################################################
#                             TREATMENT DROP-IN
#
################################################################################

# PCE 5-yr
drop_in_pce_5yr <- pce %>% 
  filter(statin_first_date <= contact_date %m+% years(5)) %>% 
  mutate(model = 'PCE 5 yr',
         statin_drop_in = if_else(
           statin_first_date <= ascvd_followup_end_date, 1, 0, 0)
  ) %>% 
  group_by(model) %>% 
  summarize(drop_in_count = sum(statin_drop_in))

# PCE 10-yr
drop_in_pce_10yr <- pce %>% 
  mutate(model = 'PCE 10 yr',
         statin_drop_in = if_else(
           statin_first_date <= ascvd_followup_end_date, 1, 0, 0)
  ) %>% 
  group_by(model) %>% 
  summarize(drop_in_count = sum(statin_drop_in))

# PREVENTS ASCVD
dropin_prevents_ascvd <- prevents_ascvd %>% 
  mutate(model = 'PREVENTS ASCVD',
         statin_drop_in = if_else(
           statin_first_date <= ascvd_followup_end_date, 1, 0, 0)
  ) %>% 
  group_by(model) %>% 
  summarize(drop_in_count = sum(statin_drop_in)) 

# PREVENTS (Total CVD)
dropin_prevents_total_cvd <- prevents_total_cvd %>% 
  mutate(model = 'PREVENTS Total CVD',
         statin_drop_in = if_else(
           statin_first_date <= total_cvd_followup_end_date, 1, 0, 0)
  ) %>% 
  group_by(model) %>% 
  summarize(drop_in_count = sum(statin_drop_in)) 



##############
# ALL METRICS
#
##############
dropin_all <- rbind(
  drop_in_pce_5yr,
  drop_in_pce_10yr,
  dropin_prevents_ascvd,
  dropin_prevents_total_cvd
)

dropin_all %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(path='figures/drop-in-counts.docx')


################################################################################
#                               VALIDATION
#
################################################################################
# PCE 5-yr
binned_calibration_plot(
  data = pce,
  p = pce_risk_5yr,
  outcome = ascvd_5yr_flag, 
  nbins = 10)

ggsave(filename='figures/validation-pce-ascvd-5yr.png')


# PCE 10-yr
binned_calibration_plot(
  data = pce,
  p = pce_risk_10yr,
  outcome = ascvd_10yr_flag, 
  nbins = 10)

ggsave(filename='figures/validation-pce-ascvd-10yr.png')


# PREVENTS ASCVD
smoothed_calibration_plot(
  data = prevents_ascvd,
  outcome = ascvd_10yr_flag,
  p = prevents_ascvd_risk_10yr)

ggsave(filename='figures/validation-prevents-ascvd.png')


# PREVENTS Total CVD
smoothed_calibration_plot(
  data = prevents_total_cvd,
  outcome = total_cvd_10yr_flag,
  p = prevents_total_cvd_risk_10yr)

ggsave(filename='figures/validation-prevents-total-cvd.png')



################################################################################
#                           CALIBRATION METRICS
#
################################################################################

######
# PCE
#
# 5-yr
######xb, mean_xb,

metrics_pce_5yr <- survival_calibration_metrics(
  data = pce, 
  outcome = ascvd_5yr_flag,
  p = pce_risk_5yr,
  xb = xb,
  mean_xb = pce_mean_xb,
  survival_time = ascvd_followup_years) %>% 
  mutate(Model = 'PCE 5yr')


#######
# PCE
#
# 10-yr
#######
metrics_pce_10yr <- survival_calibration_metrics(
  data = pce, 
  outcome = ascvd_10yr_flag,
  p = pce_risk_10yr,
  xb = xb,
  mean_xb = pce_mean_xb,
  survival_time = ascvd_followup_years) %>% 
  mutate(Model = 'PCE 10yr')


###########
# PREVENTS
#
# ASCVD
###########
metrics_prevents_ascvd <- binary_calibration_metrics(
  data = prevents_ascvd, 
  outcome = ascvd_10yr_flag, 
  p = prevents_ascvd_risk_10yr, 
  xb = log_odds_ascvd) %>% 
  mutate(Model = 'PREVENTS ASCVD')


###########
# PREVENTS
#
# Total CVD
###########

metrics_prevents_total_cvd <- binary_calibration_metrics(
  data = prevents_total_cvd, 
  outcome = total_cvd_10yr_flag, 
  p = prevents_total_cvd_risk_10yr, 
  xb = log_odds_total_cvd) %>% 
  mutate(Model = 'PREVENTS Total CVD')


##############
# ALL METRICS
#
##############
metrics_all <- tidyr::pivot_wider(
  rbind(
    metrics_pce_5yr,
    metrics_pce_10yr,
    metrics_prevents_ascvd,
    metrics_prevents_total_cvd
  ),
  id_cols = 'Metric',
  names_from = 'Model',
  values_from = 'Value'
)

metrics_all %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(path='figures/calibration-metrics.docx')


################################################################################
#                               NET BENEFIT
#
################################################################################

#############
# PCE 5-yr 
#
# Full Range
#############
png('figures/net-benefit-pce-5yr-full.png')

dcurves::dca(
  survival::Surv(ascvd_followup_years, ascvd_5yr_flag) ~ pce_risk_5yr, 
  data = pce,
  time = 5,
  label = list(pce_risk_5yr = 'PCE')
) %>%
  plot(smooth = TRUE) %>% 
  ggplot2::labs(x = "Treatment Threshold Probability")

ggsave(filename = 'figures/net-benefit-pce-5yr-full.png')
dev.off()


#############
# PCE 5-yr 
#
# Detail
#############
png('figures/net-benefit-pce-5yr-detail.png')

dcurves::dca(
  survival::Surv(ascvd_followup_years, ascvd_5yr_flag) ~ pce_risk_5yr, 
  data = pce,
  time = 5,
  label = list(pce_risk_5yr = 'PCE'),
  thresholds = (1:20 / 100)
) %>%
  plot(smooth = TRUE) %>% 
  ggplot2::labs(x = "Treatment Threshold Probability")
dev.off()


#############
# PCE 10-yr 
#
# Full Range
#############
png('figures/net-benefit-pce-10yr-full.png')

dcurves::dca(
  survival::Surv(ascvd_followup_years, ascvd_10yr_flag) ~ pce_risk_10yr, 
  data = pce,
  time = 10,
  label = list(pce_risk_10yr = 'PCE')
) %>%
  plot(smooth = TRUE) %>% 
  ggplot2::labs(x = "Treatment Threshold Probability")
dev.off()


#############
# PCE 10-yr 
#
# Detail
#############
png('figures/net-benefit-pce-10yr-detail.png')

dcurves::dca(
  survival::Surv(ascvd_followup_years, ascvd_10yr_flag) ~ pce_risk_10yr, 
  data = pce,
  time = 5,
  label = list(pce_risk_10yr = 'PCE'),
  thresholds = 1:25 / 100
) %>%
  plot(smooth = TRUE) %>% 
  ggplot2::labs(x = "Treatment Threshold Probability")
dev.off()


#################
# PREVENTS ASCVD 
#
# Full Range
#################
baseline.model <- rmda::decision_curve(
  formula = ascvd_10yr_flag ~ prevents_ascvd_risk_10yr, 
  data = prevents_ascvd,
  fitted.risk = TRUE, 
  policy = 'opt-in', 
  confidence.intervals = 'none')

png('figures/net-benefit-prevents-ascvd-full.png')

rmda::plot_decision_curve(
  baseline.model, 
  standardize=FALSE, 
  cost.benefit.axis = FALSE,
  confidence.intervals = FALSE,
  curve.names = c("PREVENTS ~ ASCVD within 10 Years"),
  col = c('black', 'red', 'blue'),
  lty = c(2, 1, 1),
  ylim = c(-0.05, 0.05), 
  xlim = c(0, 1)
)
dev.off()


#################
# PREVENTS ASCVD 
#
# Detail
#################
baseline.model <- rmda::decision_curve(
  formula = ascvd_10yr_flag ~ prevents_ascvd_risk_10yr, 
  data = prevents_ascvd,
  fitted.risk = TRUE, 
  policy = 'opt-in', 
  thresholds = c(0.0, 0.10),
  confidence.intervals = 'none')

png('figures/net-benefit-prevents-ascvd-detail.png')

rmda::plot_decision_curve(
  baseline.model, 
  standardize=FALSE, 
  cost.benefit.axis = FALSE,
  confidence.intervals = FALSE,
  curve.names = c("PREVENTS ~ ASCVD within 10 Years"),
  col = c('black', 'red', 'blue'),
  lty = c(2, 1, 1),
  ylim = c(-0.04, 0.04)
)
dev.off()


#####################
# PREVENTS Total CVD 
#
# Full Range
#####################
baseline.model <- rmda::decision_curve(
  formula = total_cvd_10yr_flag ~ prevents_total_cvd_risk_10yr, 
  data = prevents_total_cvd,
  fitted.risk = TRUE, 
  policy = 'opt-in', 
  confidence.intervals = 'none')

png('figures/net-benefit-prevents-total-cvd-full.png')

rmda::plot_decision_curve(
  baseline.model, 
  standardize=FALSE, 
  cost.benefit.axis = FALSE,
  confidence.intervals = FALSE,
  curve.names = c("PREVENTS ~ Total CVD within 10 Years"),
  col = c('black', 'red', 'blue'),
  lty = c(2, 1, 1),
  ylim = c(-0.05, 0.1), 
  xlim = c(0, 1)
)
dev.off()

#####################
# PREVENTS Total CVD 
#
# Detail
#####################
baseline.model <- rmda::decision_curve(
  formula = total_cvd_10yr_flag ~ prevents_total_cvd_risk_10yr, 
  data = prevents_total_cvd,
  fitted.risk = TRUE, 
  policy = 'opt-in', 
  thresholds = c(0.0, 0.25),
  confidence.intervals = 'none')

png('figures/net-benefit-prevents-total-cvd-detail.png')

rmda::plot_decision_curve(
  baseline.model, 
  standardize=FALSE, 
  cost.benefit.axis = FALSE,
  confidence.intervals = FALSE,
  curve.names = c("PREVENTS ~ Total CVD within 10 Years"),
  col = c('black', 'red', 'blue'),
  lty = c(2, 1, 1),
  ylim = c(-0.05, 0.10)
)
dev.off()



################################################################################
#                           SENSITIVITY ANALYSES
#
################################################################################

#########################
# PCE 5-yr 
#
# 0.5% misclassification
#########################
misclassified_df <- misclassify_percentage_of_noncases(
  data = pce, 
  outcome = ascvd_5yr_flag,
  percentage = 0.005)

metrics_misclassified_pce_5yr_005 <- survival_calibration_metrics(
  data = misclassified_df, 
  outcome = ascvd_5yr_flag,
  p = pce_risk_5yr,
  xb = xb,
  mean_xb = pce_mean_xb,
  survival_time = ascvd_followup_years) %>% 
  mutate(
    Model = 'PCE 5yr',
    Misclassification = 0.005)


#########################
# PCE 5-yr 
#
# 1% misclassification
#########################
misclassified_df <- misclassify_percentage_of_noncases(
  data = pce, 
  outcome = ascvd_5yr_flag,
  percentage = 0.01)

metrics_misclassified_pce_5yr_01 <- survival_calibration_metrics(
  data = misclassified_df, 
  outcome = ascvd_5yr_flag,
  p = pce_risk_5yr,
  xb = xb,
  mean_xb = pce_mean_xb,
  survival_time = ascvd_followup_years) %>% 
  mutate(
    Model = 'PCE 5yr',
    Misclassification = 0.01)


#########################
# PCE 10-yr 
#
# 0.5% misclassification
#########################
misclassified_df <- misclassify_percentage_of_noncases(
  data = pce, 
  outcome = ascvd_10yr_flag,
  percentage = 0.005)

metrics_misclassified_pce_10yr_005 <- survival_calibration_metrics(
  data = misclassified_df, 
  outcome = ascvd_10yr_flag,
  p = pce_risk_10yr,
  xb = xb,
  mean_xb = pce_mean_xb,
  survival_time = ascvd_followup_years) %>% 
  mutate(
    Model = 'PCE 10yr',
    Misclassification = 0.005)


#########################
# PCE 10-yr 
#
# 1% misclassification
#########################
misclassified_df <- misclassify_percentage_of_noncases(
  data = pce, 
  outcome = ascvd_10yr_flag,
  percentage = 0.01)

metrics_misclassified_pce_10yr_01 <- survival_calibration_metrics(
  data = misclassified_df, 
  outcome = ascvd_10yr_flag,
  p = pce_risk_10yr,
  xb = xb,
  mean_xb = pce_mean_xb,
  survival_time = ascvd_followup_years) %>% 
  mutate(
    Model = 'PCE 10yr',
    Misclassification = 0.01)


#########################
# PREVENTS ASCVD 
#
# 0.5% misclassification
#########################
misclassified_df <- misclassify_percentage_of_noncases(
  data = prevents_ascvd, 
  outcome = ascvd_10yr_flag,
  percentage = 0.005)

metrics_misclassified_prevents_ascvd_005 <- binary_calibration_metrics(
  data = prevents_ascvd, 
  outcome = ascvd_10yr_flag, 
  p = prevents_ascvd_risk_10yr, 
  xb = log_odds_ascvd) %>% 
  mutate(
    Model = 'PREVENTS ASCVD',
    Misclassification = 0.005)


#########################
# PREVENTS ASCVD 
#
# 1% misclassification
#########################
misclassified_df <- misclassify_percentage_of_noncases(
  data = prevents_ascvd, 
  outcome = ascvd_10yr_flag,
  percentage = 0.01)

metrics_misclassified_prevents_ascvd_01 <- binary_calibration_metrics(
  data = prevents_ascvd, 
  outcome = ascvd_10yr_flag, 
  p = prevents_ascvd_risk_10yr, 
  xb = log_odds_ascvd) %>% 
  mutate(
    Model = 'PREVENTS ASCVD',
    Misclassification = 0.01)


#########################
# PREVENTS Total CVD 
#
# 0.5% misclassification
#########################
misclassified_df <- misclassify_percentage_of_noncases(
  data = prevents_total_cvd, 
  outcome = total_cvd_10yr_flag,
  percentage = 0.005)

metrics_misclassified_prevents_total_cvd_005 <- binary_calibration_metrics(
  data = prevents_total_cvd, 
  outcome = total_cvd_10yr_flag, 
  p = prevents_total_cvd_risk_10yr, 
  xb = log_odds_total_cvd) %>% 
  mutate(
    Model = 'PREVENTS Total CVD',
    Misclassification = 0.005)


#########################
# PREVENTS Total CVD 
#
# 0.5% misclassification
#########################
misclassified_df <- misclassify_percentage_of_noncases(
  data = prevents_total_cvd, 
  outcome = total_cvd_10yr_flag,
  percentage = 0.01)

metrics_misclassified_prevents_total_cvd_01 <- binary_calibration_metrics(
  data = prevents_total_cvd, 
  outcome = total_cvd_10yr_flag, 
  p = prevents_total_cvd_risk_10yr, 
  xb = log_odds_total_cvd) %>% 
  mutate(
    Model = 'PREVENTS Total CVD',
    Misclassification = 0.01)


################################
# ALL MISCLASSIFICATION METRICS
#
################################
metrics_all <- rbind(
  metrics_misclassified_pce_5yr_005,
  metrics_misclassified_pce_5yr_01,
  metrics_misclassified_pce_10yr_005,
  metrics_misclassified_pce_10yr_01,
  metrics_misclassified_prevents_ascvd_005,
  metrics_misclassified_prevents_ascvd_01,
  metrics_misclassified_prevents_total_cvd_005,
  metrics_misclassified_prevents_total_cvd_01
)

metrics_all %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(
    path='figures/calibration-metrics-misclassification.docx')
