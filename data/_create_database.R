library(dplyr)
library(duckdb)

here::i_am('prevents-model-validation.Rproj')


con <- dbConnect(duckdb(), 'data/prevents_data.duckdb')


# Even after dumping garbage collection to reclaim memory
#   RStudio still reads about 2 GB of memory in-use.
#
# This must have something to do with the open handle to duckdb
#
# Restarting the R session successfully reclaims all memory
#   allocated to RStudio



#############################################################
# DEMOGRAPHICS
#
# Original file has corrupted line-endings
# File is comma-separated (vs. tab-separated like the others)
#
# 2024-05-14:
#   Added file containing last face-to-face encounter
#   Fits in at the patient-level
#############################################################
pats <- vroom::vroom('data/raw/patients_fixed.txt', delim=',',
                     col_select = c(PAT_ID, PAT_MRN_ID, BIRTH_DATE, PAT_SEX,
                                    ETHNIC_GROUP_NM, PATIENT_RACE, LANGUAGE_NM,
                                    MARITAL_STATUS_NM, DEATH_DATE)
) %>% 
  rename_with(tolower)


# Add "most recently completed encounter"
last_encs <- vroom::vroom('data/raw/last_face_to_face_encounters.txt',
                          delim = '\t',
                          col_select = c(PAT_ID, 
                                         LAST_DISCHARGE_OR_CONTACT_DATE)
) %>% 
  rename_with(tolower)

pats <- pats %>% left_join(last_encs)

# 96,583 patients
duckdb::dbWriteTable(con, 'patients', pats)

rm(last_encs)

#############################################################
# DIAGNOSES
#
#############################################################
dxs <- vroom::vroom('data/raw/diagnoses.txt', delim='\t',
                    col_select = c(PAT_ID, PAT_ENC_CSN_ID, HSP_ACCOUNT_ID,
                                   CONTACT_DATE, DIAGNOSIS_SOURCE,
                                   CURRENT_ICD10_LIST)
) %>% 
  rename_with(tolower)

duckdb::dbWriteTable(con, 'diagnoses', dxs)

rm(dxs)

#############################################################
# ENCOUNTERS
#
# File has multiple duplicates of rows
# Using only necessary fields and keeping distinct values
#############################################################
encs <- vroom::vroom('data/raw/encounters.txt', 
                     delim='\t', 
                     col_select = c(PAT_ID, PAT_ENC_CSN_ID, HSP_ACCOUNT_ID,
                                    CONTACT_DATE,
                                    APPT_STATUS_NM, DEPARTMENT_NAME, 
                                    PRODUCT_TYPE_NM, CVD_SCORE_LIST)
                     ) %>% 
  distinct() %>% 
  rename_with(tolower)

duckdb::dbWriteTable(con, 'encounters', encs)

rm(encs)


#############################################################
# LAB RESULTS
#
#############################################################
labs <- vroom::vroom('data/raw/labs.txt', delim='\t',
                     col_select = c(PAT_ID, PAT_ENC_CSN_ID, COMPONENT_ID, 
                                    COMPONENT_NM, RESULT_TIME, ORD_VALUE)
                     ) %>% 
  rename_with(tolower)

duckdb::dbWriteTable(con, 'labs', labs)

rm(labs)

#############################################################
# MEDICATION ORDERS
#
#############################################################
meds <- vroom::vroom('data/raw/medications.txt', delim='\t',
                     col_select = c(PAT_ID, PAT_ENC_CSN_ID, ORDER_INST, 
                                    START_DATE, END_DATE,
                                    ATC_CODE, ATC_TITLE)
                     ) %>% 
  rename_with(tolower)

duckdb::dbWriteTable(con, 'medications', meds)

rm(meds)

#############################################################
# SOCIAL HISTORY
#
#############################################################
shx <- vroom::vroom('data/raw/social_history.txt', delim='\t') %>%
  rename_with(tolower)

duckdb::dbWriteTable(con, 'social_history', shx)

rm(shx)

#############################################################
# VITAL SIGNS
#
#############################################################
vitals <- vroom::vroom('data/raw/vital_stats.txt', delim='\t',
                       col_select = c(PAT_MRN_ID, PAT_ENC_CSN_ID, FLO_MEAS_ID,
                                      FLO_MEAS_NAME, RECORDED_TIME, MEAS_VALUE)
) %>% 
  rename_with(tolower)

duckdb::dbWriteTable(con, 'vitals', vitals)

rm(vitals)

#############################################################
# NDI MORTALITY
#
# NDI-provided identifiers (e.g., death certificate number,
#   all data for non-matches with no IRB approval, etc.)
#   must remain on an on-premises server (H://)
#############################################################
ndi <- 
  vroom::vroom('H:/Documents/ndi/data/ndi-compiled-manifest.csv',
               col_select = c(pat_mrn_id, ndi_state_death_code,
                              ndi_year_death_full, ndi_death_cert_no,
                              ndi_status_code),
               col_types = c('c')) %>% 
  # matches only
  filter(ndi_status_code == 1) %>% 
  distinct()


ndi_cause <- 
  vroom::vroom('H:/Documents/ndi/data/ndi-compiled-cause-of-death.csv',
               col_select = c(pat_mrn_id, ndi_state_death_code,
                              ndi_year_death_full, ndi_death_cert_no,
                              ndi_month_death, ndi_day_death,
                              ndi_cause_underlying,
                              starts_with('ndi_cause_record')),
               col_types = 'c')


ndi_matches <- ndi %>% 
  inner_join(pats %>% distinct(pat_id, pat_mrn_id)) %>% 
  inner_join(ndi_cause, 
             join_by(pat_mrn_id, ndi_state_death_code,
                     ndi_year_death_full, ndi_death_cert_no)) %>% 
  mutate(
    death_date = 
      as.Date(paste0(ndi_year_death_full, '/', 
                     ndi_month_death, '/',
                     ndi_day_death)),
    
    death_cause = case_when(
      # I2[12]  = myocardial infarction
      # I6[123] = stroke
      # I50     = heart failure
      if_any(starts_with('ndi_cause'), 
             ~ grepl('I2[12]|I6[123]', .)) ~ 'ascvd',
      
      if_any(starts_with('ndi_cause'), 
             ~ grepl('I2[12]|I6[123]|I50', .)) ~ 'total cvd',
      
      TRUE ~ 'other'
    )
  ) %>% 
  select(pat_id, death_date, death_cause)


dbWriteTable(con, 'ndi_cause_of_death', ndi_matches)

rm(ndi)
rm(ndi_cause)
rm(ndi_matches)
rm(pats)
gc()

##############################################################
# Create table of ASCVD eligible encounters (>=1 per patient)
#
##############################################################
ascvd_eligible_encounters <- 
  dbGetQuery(con, statement = readr::read_file(
    'data/_create_table_ascvd_eligible_encounters.sql'))


dbWriteTable(con, 'ascvd_eligible_encounters', ascvd_eligible_encounters)

# 35,552 patients
dbGetQuery(con, "select count(distinct pat_id) from ascvd_eligible_encounters")


rm(ascvd_eligible_encounters)
gc()


##################################################################
# Create table of Total CVD eligible encounters (>=1 per patient)
#
##################################################################
total_cvd_eligible_encounters <- 
  dbGetQuery(con, statement = readr::read_file(
    'data/_create_table_total_cvd_eligible_encounters.sql'))


dbWriteTable(con, 'total_cvd_eligible_encounters', total_cvd_eligible_encounters)

# 35,346 patients
dbGetQuery(con, "select count(distinct pat_id) from total_cvd_eligible_encounters")


rm(total_cvd_eligible_encounters)
gc()

#################################################################
# Create table of index encounters for PCE model (1 per patient)
#
#################################################################
pce_first_eligible_encounter_with_predictors <-
  dbGetQuery(con, statement = readr::read_file(
    'data/_create_table_pce_first_eligible_encounter_with_predictors.sql'))


dbWriteTable(con, 'pce_first_eligible_encounter_with_predictors', 
             pce_first_eligible_encounter_with_predictors)


# 25,648 patients
dbGetQuery(con, "select count(pat_id)
                 from pce_first_eligible_encounter_with_predictors")


######################################################################
# Create table of index encounters for PREVENTS model (1 per patient)
# -- ASCVD outcome
#
######################################################################
prevents_first_ascvd_eligible_encounter_with_predictors <-
  dbGetQuery(con, statement = readr::read_file(
    'data/_create_table_prevents_first_ascvd_eligible_encounter_with_predictors.sql'))


dbWriteTable(con, 'prevents_first_ascvd_eligible_encounter_with_predictors', 
             prevents_first_ascvd_eligible_encounter_with_predictors)


# 23,887
dbGetQuery(con, "select count(pat_id)
                 from prevents_first_ascvd_eligible_encounter_with_predictors")

######################################################################
# Create table of index encounters for PREVENTS model (1 per patient)
# -- Total CVD outcome
#
######################################################################
prevents_first_total_cvd_eligible_encounter_with_predictors <-
  dbGetQuery(con, statement = readr::read_file(
    'data/_create_table_prevents_first_total_cvd_eligible_encounter_with_predictors.sql'))


dbWriteTable(con, 'prevents_first_total_cvd_eligible_encounter_with_predictors', 
             prevents_first_total_cvd_eligible_encounter_with_predictors)


# 23,746
dbGetQuery(con, "select count(pat_id)
                 from prevents_first_total_cvd_eligible_encounter_with_predictors")


# Double check existing tables
dbGetQuery(con, "show tables")


# Disconnect from database
dbDisconnect(con)
