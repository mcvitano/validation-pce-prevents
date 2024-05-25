library(dplyr)

create_demographics_table <- function(data, 
                                      outcome_field, 
                                      outcome_label,
                                      save_path) {
  
  # Returns an HTML table of demographics with three columns:
  #   (1) Overall
  #   (2) With outcome
  #   (3) Without outcome
  
  tbl_overall <- {{data}} %>%
    gtsummary::tbl_summary(
      include = c(age_yrs, sex, raceth,
                  #language_primary, relationship_status,
                  insurance, current_smoker_flag, current_hypertensive_med_flag,
                  diabetes_flag, hypertension_flag, dyslipidemia_flag,
                  #ldl_chol,
                  hdl_chol, total_chol, systolic_bp, egfr),
      
      label = list(
        age_yrs ~ 'Age (yrs)',
        sex ~ 'Sex',
        raceth ~ 'Race/Ethnicity',
        #language_primary ~ 'Language (primary)',
        #relationship_status ~ 'Relationship status',
        insurance ~ 'Insurance',
        current_smoker_flag ~ 'Current smoker',
        current_hypertensive_med_flag ~ 'Current treatment for hypertension',
        diabetes_flag ~ 'Diabetes',
        dyslipidemia_flag ~ 'Dyslipidemia',
        hypertension_flag ~ 'Hypertension',
        #ldl_chol ~ 'LDL cholesterol',
        hdl_chol ~ 'HDL cholesterol',
        total_chol ~ 'Total cholesterol',
        systolic_bp ~ 'Systolic blood pressure',
        egfr ~ 'Estimated GFR'
      )
    )
  
  tbl_by_outcome <- {{data}} %>%
    # for plot labels
    mutate(outcome = if_else(
      {{outcome_field}} == 1, 
      glue::glue('Incident {outcome_label}'), 
      glue::glue('No {outcome_label}'))
    ) %>% 
    
    gtsummary::tbl_summary(
      include = c(age_yrs, sex, raceth, 
                  #language_primary, relationship_status,
                  insurance, current_smoker_flag, current_hypertensive_med_flag,
                  diabetes_flag, hypertension_flag, dyslipidemia_flag,
                  #ldl_chol, 
                  hdl_chol, total_chol, systolic_bp, egfr,
                  outcome),
      
      by = outcome,
      
      label = list(
        age_yrs ~ 'Age (yrs)',
        sex ~ 'Sex',
        raceth ~ 'Race/Ethnicity',
        #language_primary ~ 'Language (primary)',
        #relationship_status ~ 'Relationship status',
        insurance ~ 'Insurance',
        current_smoker_flag ~ 'Current smoker',
        current_hypertensive_med_flag ~ 'Current treatment for hypertension',
        diabetes_flag ~ 'Diabetes',
        dyslipidemia_flag ~ 'Dyslipidemia',
        hypertension_flag ~ 'Hypertension',
        #ldl_chol ~ 'LDL cholesterol',
        hdl_chol ~ 'HDL cholesterol',
        total_chol ~ 'Total cholesterol',
        systolic_bp ~ 'Systolic blood pressure',
        egfr ~ 'Estimated GFR'
      )
    )
  
  tbl_merged <- gtsummary::tbl_merge(
    tbls = list(tbl_overall, tbl_by_outcome),
    tab_spanner = c('Overall', 'By Outcome')
  )
  
  tbl_merged %>% 
    gtsummary::as_gt() %>% 
    gt::gtsave(save_path)
  
  return(tbl_merged)
}



calculate_pce_risks <- function(data) {
  # Sourced from:
  #   2013 ACC/AHA Guideline on the Assessment of Cardiovascular Risk
  #   J Am Coll Cardiol. 2014 July 1; 63(25 0 0): 2935–2959.
  #   doi:10.1016/j.jacc.2013.11.005.
  #
  #   Validation of the Atherosclerotic Cardiovascular Disease
  #     Pooled Cohort Risk Equations (Supplementary material)
  #   JAMA. 2014 Apr 9;311(14):1406-15.
  #   doi: 10.1001/jama.2014.2630.
  
  new_df <- {{data}} %>% 
    mutate(
      ln_age = log(age_yrs),
      ln_age_squared = log(age_yrs)^2,
      ln_tc = log(total_chol),
      ln_age_ln_tc = ln_age * ln_tc,
      ln_hdl = log(hdl_chol),
      ln_age_ln_hdl = ln_age * ln_hdl,
      ln_treated_sbp = log(systolic_bp) * current_hypertensive_med_flag,
      ln_age_ln_treated_sbp = ln_age * ln_treated_sbp,
      ln_untreated_sbp = ifelse(current_hypertensive_med_flag==0,
                                log(systolic_bp), 0),
      ln_age_ln_untreated_sbp = ln_age * ln_untreated_sbp,
      ln_age_current_smoker_flag = ln_age * current_smoker_flag,
      
      xb = case_when(
        sex=='female' & raceth=='nh black' 
        ~ 17.114 * ln_age +
          #ln_age_squared
          0.940 * ln_tc +
          #ln_age_ln_tc
          -18.920 * ln_hdl +
          4.475 * ln_age_ln_hdl +
          29.291 * ln_treated_sbp +
          -6.432 * ln_age_ln_treated_sbp +
          27.820 * ln_untreated_sbp +
          -6.087 * ln_age_ln_untreated_sbp +
          0.691 * current_smoker_flag +
          #ln_age_current_smoker_flag
          0.874 * diabetes_flag,
        
        sex=='female' & raceth=='nh white' 
        ~ -29.799 * ln_age +
          4.884 * ln_age_squared +
          13.540 * ln_tc +
          -3.114 * ln_age_ln_tc +
          -13.578 * ln_hdl +
          3.149 * ln_age_ln_hdl +
          2.019 * ln_treated_sbp +
          #ln_age_log10_treated_sbp +
          1.957 * ln_untreated_sbp +
          #ln_age_ln_untreated_sbp +
          7.574 * current_smoker_flag +
          -1.665 * ln_age_current_smoker_flag +
          0.661 * diabetes_flag,
        
        sex=='male' & raceth=='nh black' 
        ~ 2.469 * ln_age +
          #ln_age_squared
          0.302 * ln_tc +
          #ln_age_ln_tc
          -0.307 * ln_hdl +
          #ln_age_ln_hdl +
          1.916 * ln_treated_sbp +
          #ln_age_log10_treated_sbp +
          1.809 * ln_untreated_sbp +
          #ln_age_ln_untreated_sbp +
          0.549 * current_smoker_flag +
          #ln_age_current_smoker_flag
          0.645 * diabetes_flag,
        
        sex=='male' & raceth=='nh white' 
        ~ 12.344 * ln_age +
          #ln_age_squared
          11.853 * ln_tc +
          -2.664 * ln_age_ln_tc +
          -7.990 * ln_hdl +
          1.769 * ln_age_ln_hdl +
          1.797 * ln_treated_sbp +
          #ln_age_log10_treated_sbp +
          1.764 * ln_untreated_sbp +
          #ln_age_ln_untreated_sbp +
          7.837 * current_smoker_flag +
          -1.795 * ln_age_current_smoker_flag +
          0.658 * diabetes_flag
      ),
      
      so_t5 = case_when(
        sex=='female' & raceth=='nh black' ~ 0.98194,
        sex=='female' & raceth=='nh white' ~ 0.98898,
        sex=='male' & raceth=='nh black' ~ 0.95726,
        sex=='male' & raceth=='nh white' ~ 0.96254
      ),
      
      so_t10 = case_when(
        sex=='female' & raceth=='nh black' ~ 0.9533,
        sex=='female' & raceth=='nh white' ~ 0.9665,
        sex=='male' & raceth=='nh black' ~ 0.8954,
        sex=='male' & raceth=='nh white' ~ 0.9144
      ),
      
      pce_mean_xb = case_when(
        sex=='female' & raceth=='nh black' ~ 86.61,
        sex=='female' & raceth=='nh white' ~ -29.19,
        sex=='male' & raceth=='nh black' ~ 19.54,
        sex=='male' & raceth=='nh white' ~ 61.18
      ),
      
      pce_risk_5yr = round(1 - so_t5^exp(xb - pce_mean_xb), 4),
      
      pce_risk_10yr = round(1 - so_t10^exp(xb - pce_mean_xb), 4)
    ) 
}



calculate_prevents_ascvd_risks <- function(data) {
  # Sourced from:
  #   Development and Validation of the American Heart Association's
  #     PREVENT Equations (Supplementary material)
  #   Circulation. 2024 Feb 6;149(6):430-449.
  #   doi: 10.1161/CIRCULATIONAHA.123.067626.
  
  new_df <- {{data}} %>% 
    # rename to match PREVENTS equations in publication
    mutate(
      age = age_yrs,
      Tc = total_chol,
      HDL = hdl_chol,
      SBP = systolic_bp,
      diabetes = diabetes_flag,
      current_smoker = current_smoker_flag,
      eGFR = egfr
    ) %>% 
    
    mutate(
      x1 = (age - 55) /10,
      x2 = (Tc - HDL) * 0.02586 - 3.5,
      x3 = (HDL * 0.02586 - 1.3) /0.3, 
      x4 = (pmin(SBP, 110) - 110) /20, 
      x5 = (pmax(SBP, 110) - 130) /20, 
      x6 = diabetes,
      x7 = current_smoker, 
      x8 = (pmin(eGFR, 60) - 60) / -15, 
      x9 = (pmax(eGFR, 60) - 90) / -15,
      x10 = current_hypertensive_med_flag, 
      #x11 = (if using statin)
      x12 = current_hypertensive_med_flag * ((pmax(SBP, 110) - 130) /20), 
      #x13 = (if using statin) * (Tc - HDL - 3.5)
      x14 = ((age - 55) /10) * (((Tc - HDL) * 0.02586) - 3.5),
      x15 = ((age - 55) /10) * ((HDL * 0.02586 - 1.3) /0.3),
      x16 = ((age - 55) /10) * ((pmax(SBP, 110) - 130) /20),
      x17 = ((age - 55) /10) * diabetes,
      x18 = ((age - 55) /10) * current_smoker,
      x19 = ((age - 55) /10) * ((pmin(eGFR, 60) - 60) / -15),
      
      
      log_odds_ascvd = case_when(
        sex=='female' 
        ~ -3.819975 + (0.719883 * x1)
        + (0.1176967 * x2)
        - (0.151185 * x3)
        - (0.0835358 * x4)
        + (0.3592852 * x5)
        + (0.8348585 * x6) 
        + (0.4831078 * x7)
        + (0.4864619 * x8)
        + (0.0397779 * x9)
        + (0.2265309 * x10)
        #- (0.0592374 * x11)
        - (0.0395762 * x12)
        #+ (0.0844423 * x13)
        - (0.0567839 * x14)
        + (0.0325692 * x15)
        - (0.1035985 * x16)
        - (0.2417542 * x17)
        - (0.0791142 * x18)
        - (0.1671492 * x19),
        
        
        sex=='male'
        ~ -3.500655 + (0.7099847 * x1)
        + (0.1658663 * x2)
        - (0.1144285 * x3)
        - (0.2837212 * x4)
        + (0.3239977 * x5)
        + (0.7189597 * x6) 
        + (0.3956973 * x7)
        + (0.3690075 * x8)
        + (0.0203619 * x9)
        + (0.2036522 * x10)
        #- (0.0865581 * x11)
        - (0.0322916 * x12)
        #+ (0.114563 * x13)
        - (0.0300005 * x14)
        + (0.0232747 * x15)
        - (0.0927024 * x16)
        - (0.2018525 * x17)
        - (0.0970527 * x18)
        - (0.1217081 * x19)
      ),
      
      prevents_ascvd_risk_10yr = 
        exp(log_odds_ascvd) / (1 + exp(log_odds_ascvd))
      
    ) 
}



calculate_prevents_total_cvd_risks <- function(data) {
  # Sourced from:
  #   Development and Validation of the American Heart Association's
  #     PREVENT Equations (Supplementary material)
  #   Circulation. 2024 Feb 6;149(6):430-449.
  #   doi: 10.1161/CIRCULATIONAHA.123.067626.
  
  new_df <- {{data}} %>% 
    # rename to match PREVENTS equations in publication
    mutate(
      age = age_yrs,
      Tc = total_chol,
      HDL = hdl_chol,
      SBP = systolic_bp,
      diabetes = diabetes_flag,
      current_smoker = current_smoker_flag,
      eGFR = egfr
    ) %>% 
    
    mutate(
      x1 = (age - 55) /10,
      x2 = (Tc - HDL) * 0.02586 - 3.5,
      x3 = (HDL * 0.02586 - 1.3) /0.3, 
      x4 = (pmin(SBP, 110) - 110) /20, 
      x5 = (pmax(SBP, 110) - 130) /20, 
      x6 = diabetes,
      x7 = current_smoker, 
      x8 = (pmin(eGFR, 60) - 60) / -15, 
      x9 = (pmax(eGFR, 60) - 90) / -15,
      x10 = current_hypertensive_med_flag, 
      #x11 = (if using statin)
      x12 = current_hypertensive_med_flag * ((pmax(SBP, 110) - 130) /20), 
      #x13 = (if using statin) * (Tc - HDL - 3.5)
      x14 = ((age - 55) /10) * (((Tc - HDL) * 0.02586) - 3.5),
      x15 = ((age - 55) /10) * ((HDL * 0.02586 - 1.3) /0.3),
      x16 = ((age - 55) /10) * ((pmax(SBP, 110) - 130) /20),
      x17 = ((age - 55) /10) * diabetes,
      x18 = ((age - 55) /10) * current_smoker,
      x19 = ((age - 55) /10) * ((pmin(eGFR, 60) - 60) / -15),
      
      log_odds_total_cvd = case_when(
        sex=='female'
        ~ -3.307728 + (0.7939329 * x1)
        + (0.0305239 * x2)
        - (0.1606857 * x3)
        - (0.2394003 * x4)
        + (0.360078 * x5)
        + (0.8667604 * x6) 
        + (0.5360739 * x7)
        + (0.6045917 * x8)
        + (0.0433769 * x9)
        + (0.3151672 * x10)
        #- (0.1477655 * x11)
        - (0.0663612 * x12)
        #+ (0.1197879 * x13)
        - (0.0819715 * x14)
        + (0.0306769 * x15)
        - (0.0946348 * x16)
        - (0.27057 * x17)
        - (0.078715 * x18)
        - (0.1637806 * x19),
        
        sex=='male'
        ~ -3.031168 + (0.7688528 * x1)
        + (0.0736174 * x2)
        - (0.0954431 * x3)
        - (0.4347345 * x4)
        + (0.3362658 * x5)
        + (0.7692857 * x6) 
        + (0.4386871 * x7)
        + (0.5378979 * x8)
        + (0.0164827 * x9)
        + (0.288879 * x10)
        #- (0.1337349 * x11)
        - (0.0475924 * x12)
        #+ (0.150273 * x13)
        - (0.0517874 * x14)
        + (0.0191169 * x15)
        - (0.1049477 * x16)
        - (0.2251948 * x17)
        - (0.0895067 * x18)
        - (0.1543702 * x19)
      ),
      
      # calculate risks
      prevents_total_cvd_risk_10yr = 
        exp(log_odds_total_cvd) / (1 + exp(log_odds_total_cvd))
      
    ) 
}



#' binary_calibration_metrics
#' 
#' @description
#' Returns a dataframe of calibration metrics for a binomial model
#'
#' @details
#' (1) Number of events
#' (2) AUC (c-statistic)
#' (3) Calibration-in-the-large
#' (4) Calibration intercept
#' (5) Calibration slope
#' 
#'@param data Dataframe
#'@param outcome Binary indicator of outcome status
#'@param p Model-predicted risk/probability of outcome
#'@param xb Linear predictor as re-constructed in the *validation* data
#' 
#'@return Dataframe
binary_calibration_metrics <- function(data, outcome, p, xb) {
  outcome <- deparse(substitute(outcome))
  p <- deparse(substitute(p))
  xb <- deparse(substitute(xb))

  # c-statistic (~ AUC for binary outcome)
  auc <- signif(as.numeric(pROC::roc(
    data[[outcome]] ~ data[[p]], data, 
    plot=FALSE, ci=TRUE)$ci), 2)
  
  # Mean Calibration (expected cases/observed cases)
  # Is this based on:
  # (1) overall risk?
  # (2) number of cases?
  # (3) risk in the non-cases?
  #exp_risk <- mean(data[data[[outcome]] == 0, ][[p]])
  exp_risk <- signif(mean(data[[p]]), 2)
  exp_cases <- ceiling(exp_risk*nrow(data))
  obs_risk <- signif(mean(data[[outcome]]), 2)
  obs_cases <- sum(data[[outcome]])
  
  #eo <-  signif(exp_cases/obs_cases, 3)
  
  #eo_lb <- signif(((sqrt(exp_cases) - 1.95 * 0.5)^2)/obs_cases, 3)
  #eo_ub <- signif(((sqrt(exp_cases) + 1.95 * 0.5)^2)/obs_cases, 3)
  
  # Calibration Intercept
  intercept_model <- glm(
    data[[outcome]] ~ offset(data[[xb]]), family='binomial', data = data)
  intercept <- signif(coef(intercept_model), 2)
  intercept_cls <- signif(confint.default(intercept_model), 2)
  
  # Calibration Slope
  slope_model <- glm(
    data[[outcome]] ~ data[[xb]], family='binomial', data = data)
  slope <- signif(coef(slope_model)[2], 2)
  slope_cls <- signif(confint.default(slope_model)[c(2, 4)], 2) #c(1, 3)intercept
  
  data.frame(
    'Metric' = c(
      'AUC', 
      "Expected cases (risk)",
      "Observed cases (risk)",
      "Calibration-in-the-large",
      'Intercept', 
      'Slope'),
    
    'Value' = c(
      glue::glue('{auc[2]} ({auc[1]}, {auc[3]})'),
      glue::glue('{exp_cases} ({exp_risk})'), 
      glue::glue('{obs_cases} ({obs_risk})'), 
      signif(exp_risk/obs_risk, 2),
      glue::glue('{intercept} ({intercept_cls[1]}, {intercept_cls[2]})'),
      glue::glue('{slope[1]} ({slope_cls[1]}, {slope_cls[2]})'))
  ) 
}


#' survival_calibration_metrics
#' 
#' @description
#' Returns a dataframe of calibration metrics for a Cox PH model
#'
#' @details
#' (1) Number of events
#' (2) Harrell's c-statistic
#' (3) Calibration-in-the-large
#' (4) Calibration slope
#' 
#'@param data Dataframe
#'@param outcome Binary indicator of outcome status
#'@param p Model-predicted risk/probability of outcome
#'@param xb Linear predictor as re-constructed in the *validation* data
#'@param mean_xb Mean linear predictor reported from the *derivation* data
#'@param survival_time Time to outcome
#' 
#'@return Dataframe
survival_calibration_metrics <- function(
    data, outcome, p, xb, mean_xb,  survival_time) {
  
  survival_time <- deparse(substitute(survival_time))
  outcome <- deparse(substitute(outcome))
  p <- deparse(substitute(p))
  xb <- deparse(substitute(xb))
  mean_xb <- deparse(substitute(mean_xb))
  
  surv_object <- survival::Surv(
    time = data[[survival_time]], 
    event = data[[outcome]])
  
  # Harrell's c-statistic
  harrells_c <- signif(dynpred::cindex(
    formula = surv_object ~ data[[p]],
    data = data)$cindex, 3)
  
  # Mean Calibration (expected cases/observed cases)
  #exp_risk <- mean(data[data[[outcome]] == 0, ][[p]])
  exp_risk <- signif(mean(data[[p]]), 2)
  exp_cases <- ceiling(exp_risk*nrow(data))
  obs_risk <- signif(mean(data[[outcome]]), 2)
  obs_cases <- sum(data[[outcome]])
  
  #eo <-  signif(exp_cases/obs_cases, 3)
  
  #eo_lb <- signif(((sqrt(exp_cases) - 1.95 * 0.5)^2)/obs_cases, 3)
  #eo_ub <- signif(((sqrt(exp_cases) + 1.95 * 0.5)^2)/obs_cases, 3)
  
  # Calibration slope
  centered_xb <- data[[xb]] - data[[mean_xb]]
  
  fit_val <- survival::coxph(
    surv_object ~ centered_xb,
    data)
  
  slope <- signif(coef(fit_val), 2)
  slope_cls <- signif(confint(fit_val), 2)
  
  
  metrics_pce_5yr <- data.frame(
    'Metric' = c(
      "Harrell's c-index",
      "Expected cases (risk)",
      "Observed cases (risk)",
      "Calibration-in-the-large",
      "Slope"
    ),
    
    'Value' = c(
      harrells_c,
      glue::glue('{exp_cases} ({exp_risk})'), 
      glue::glue('{obs_cases} ({obs_risk})'), 
      signif(exp_risk/obs_risk, 2),
      glue::glue('{slope} ({slope_cls[1]}, {slope_cls[2]})')
    )
  ) 
}
  

#' binned_calibration_plot
#' 
#' @description
#' Plots expected vs. observed event rate, groupoing risk into bins
#'
#' @details
#' Adds a legend
#' Adds a histogram based on the *count* of observed events
#' 
#'@param data Dataframe
#'@param outcome Binary indicator of outcome status
#'@param p Model-predicted risk/probability of outcome
#'@param nbins Number of risk groups to create
#' 
#'@return None
binned_calibration_plot <- function(
    data, p, outcome, nbins=10){
  
  point_shape <- 1
  line_type <- "dashed"
  
  # Risk groups (deciles)
  risk_groups <- mutate( {{data}} , bin = ntile( {{p}} , {{nbins}} )) %>% 
    group_by(bin) %>%
    mutate(n = n(), 
           bin_pred = mean( {{p}} ), 
           bin_prob = mean( {{outcome}} ), 
           se = sqrt((bin_prob * (1 - bin_prob)) / n), 
           ul = bin_prob + 1.96 * se, 
           ll = bin_prob - 1.96 * se) %>% 
    # may produce *WARNINGS* due to "rows containing missing values"
    # (removed by lower bound of confiddence limit < 0)
    mutate(ll = replace(ll, which(ll<0), 0)) %>% 
    ungroup() 

  # Base plot
  g1 <- risk_groups %>%
    ggplot(aes(x=bin_pred, y=bin_prob)) +
    geom_point(shape=point_shape, stroke = 1, size=1) +
    geom_linerange(aes(ymin = ll, ymax = ul), linetype = line_type) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_abline(intercept = 0, 
                slope = 1, 
                linetype = "dashed")
  
  # Format legend
  g2 <- g1 +
    geom_point(aes(color = "Risk Group"), 
               shape=point_shape, stroke=1, size=1) +
    geom_linerange(aes(ymin = ll, 
                       ymax = ul, 
                       color = "95% CI"),
                   linetype = line_type) +
    scale_color_manual(name = "Legend", 
                       values = c("#000000", "#000000")) +
    guides(colour = guide_legend(
      override.aes = list(
        linetype = c(line_type, "blank"), 
        shape = c(NA, point_shape)))) +
    xlab("") +
    ylab("Observed Probability") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.border = element_blank(), 
          axis.line = element_line())

  # Distribution plot        
  g3 <- ggplot(data, aes(x = {{p}} )) +
    geom_histogram(fill = "black", binwidth = 0.0125, boundary = 0) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    xlab("Predicted Probability") +
    ylab("") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  # Combine
  plot(cowplot::plot_grid(
    g2, g3, 
    align = "v", nrow = 2, 
    axis="tblr", 
    rel_heights = c(1, 1/3)))
  
}



#' smoothed_calibration_plot
#' 
#' @description
#' Plots expected vs. observed event rate, smoothing estimates using a GAM
#'
#' @details
#' Adds a legend
#' Adds a histogram based on the *percentage* of observed events
#' 
#'@param data Dataframe
#'@param outcome Binary indicator of outcome status
#'@param p Model-predicted risk/probability of outcome
#' 
#'@return None
smoothed_calibration_plot <- function(
    data, outcome, p){
  
  xlim <- c(0, 1)
  ylim <- c(0, 1)
  x_title <- ''
  
  xlim_hist <- c(0, 1)
  ylim_hist <- c(0, 1)
  x_rug_title <-  'Predicted Risk'
  
  outcome <- deparse(substitute(outcome))
  p <- deparse(substitute(p))
  
  # Base plot
  g1 <- ggplot(data, 
               aes(x=.data[[p]], y=.data[[outcome]], 
                   color='Smooth')) +
    geom_smooth() +
    # Add legend (tied to 'color' parameter in aes() of g1)
    scale_colour_manual(
      name="Legend", 
      values=c("blue")) +
    coord_cartesian(xlim = xlim, 
                    ylim = ylim) + 
    geom_abline(intercept = 0, 
                slope = 1, 
                linetype = "dashed") +
    labs(title = NULL, 
         x = x_title,
         y = "Observed Event Rate")

  # Distribution plot        
  g2 <- ggplot(data, aes(x = .data[[p]] )) +
    geom_histogram(
      # As a percentage (20 bins)
      binwidth = (xlim[2] - xlim[1])/20, 
      aes(y = after_stat(count)/sum(..count..)),
      #binwidth = 0.0125,
      #fill = "black", 
      boundary = 0) +
    labs(title = NULL, 
         x = x_rug_title, 
         y = "Percentage") +
    coord_cartesian(xlim = xlim_hist,
                    ylim = ylim_hist)
  
  # Combine
  plot(cowplot::plot_grid(
    g1, g2, 
    align = "v", nrow = 2, 
    axis="tblr", 
    rel_heights = c(1, 1/3)))
}


misclassify_percentage_of_noncases <- function(
    data, outcome, percentage) {
  
  outcome = deparse(substitute(outcome))
  
  non_cases <- data[data[[outcome]] == 0, ]
  n_sample <- ceiling(nrow(non_cases) * percentage)
  sampled_rows <- sample(x = nrow(non_cases), size = n_sample)
  indices <- non_cases[sampled_rows, ]
  
  misclassified_df <- data
  
  misclassified_df[row(indices), outcome] <- 1
  
  return(misclassified_df)
}
