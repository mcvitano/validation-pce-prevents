/*

_create_table_pce_first_eligible_encounter_with_predictors.sql

SQL:    duckdb
Author: MCvitano01@jpshealth.org
Date:   2024-05-10


We will apply a 180-day look-back period for predictor availability at 
each eligible encounter.

The earliest encounter at which information for all predictors are available 
will be considered the index encounter. Therefore, no patient will enter 
the cohort more than once.

Predictors for the PCE model include:
  - sex
  - raceth
  - age
  - total cholesterol
  - HDL cholesterol
  - systolic blood pressure
  - diabetes (yes,no)
  - current smoking status (yes, no)
  - current hypertensive medication (yes, no)


The PCE model stratifies on sex and race while the PREVENTS model does not. 
These sex/race filters are applied to the dataset in the analysis rather than 
here in the SQL so that the PCE table may be used as the base table for building 
the PREVENTS table (cuts duplicate computation and likely increases overlap 
between the two datasets in case additional analyses are requested).
*/

with

cholesterol_labs as (

  select
    pat_id,

    cast(result_time as date) as chol_date,
    
    cast(
      regexp_replace(ord_value, '=|>|<|,', '') as integer
    ) as ord_value_numeric, 
    
    case
      when component_id in (
        2999, 30400410, 1811463, 1230000143, 1230001313)
        then 'ldl_chol'
        
      when component_id in (1726268, 123000012)
        then 'hdl_chol'
        
      else 'total_chol'
    
    end as lab_type
    
  from labs
  
  where
    component_id in (
      2999, 30400410, 1811463, 1230000143, 1230001313,
      1726268, 123000012,
      1510312, 1557760, 1230000048)
      
    -- numeric, non-negative
    and try_cast(
      regexp_replace(ord_value, '=|>|<|,', '') as integer) > 0 
      
),

pivoted_cholesterol_labs as (
  -- Cholesterol labs are nearly always resulted as a group
  -- since some (e.g., LDL) requires a calculation involving
  -- the other components.
  -- Thus, using a pivot table greatly decreases the search-space
  -- for encounters with all predictors present.
  pivot cholesterol_labs
  
  on lab_type
  
  -- use average of values (if multiple measurements on same day)
  using avg(ord_value_numeric) 

),

blood_pressure_at_encounter as (

  select
    pat_enc_csn_id,
    
    min(recorded_time) as systolic_bp_time,
    
    round(
      avg(
        cast(regexp_extract(meas_value, '[0-9]*') as integer)
      )
    ) as systolic_bp,
    
    
    
  from vitals
  
    --inner join patients
    --  on vitals.pat_mrn_id = patients.pat_mrn_id
  
  where
    flo_meas_id = 5
    -- do not accept single digit measurements (error or death)
    and length(regexp_extract(meas_value, '[0-9]*')) in (2, 3)
    
  group by pat_enc_csn_id
    
),

ascvd_events as (
  -- Excludes heart failure
  select
    pat_id,
    min(ascvd_date) as ascvd_date
    
  from (
    select
      pat_id,
      min(contact_date) as ascvd_date
    
    from diagnoses
  
    where
      current_icd10_list like '%I21%'     -- acute myocardial infarction
      or current_icd10_list like '%I22%'  -- subsequent ST- and non-ST elevation
      or current_icd10_list like '%I61%'  -- nontraumatic intracerebral hemorrhage
      or current_icd10_list like '%I62%'  -- other nontraumatic intracranial
      or current_icd10_list like '%I63%'  -- cerebral infarction

    group by pat_id
  
    union
  
    select
      pat_id,
      death_date as ascvd_date
  
    from ndi_cause_of_death
  
    where death_cause = 'ascvd'
  )
  
  group by pat_id
),


pce_eligible_encounters_with_predictors as (

  -- Pull together fields for encounters at which all predictor
  -- variables are available ... except 'current hypertension treatment'
  -- which will be added in a subsequent step.
  select
    *,
    -- Adds:
    --  chol_date,
    --  ldl_chol,
    --  hdl_chol,
    --  total_chol,
    --  systolic_bp_time,
    --  systolic_bp,
    
    row_number() over (
      partition by encs.pat_id
      order by contact_date
    ) as patient_encounter_row_num
    
  from ascvd_eligible_encounters as encs
      
    inner join blood_pressure_at_encounter
      using (pat_enc_csn_id)
      --on encs.pat_enc_csn_id = blood_pressure_at_encounter.pat_enc_csn_id
  
    left join pivoted_cholesterol_labs
      using (pat_id)
      --on encs.pat_id = pivoted_cholesterol_labs.pat_id
   
  where 1=1
    and (date_diff('day', chol_date, contact_date) between 0 and 180)
    and ldl_chol is not null
    and hdl_chol is not null
    and total_chol is not null
    --and (date_diff('day', systolic_bp_time, contact_date) between 0 and 180)

),

index_encounters_with_predictors as (

  -- Filter to a single 'index' encounter per patient
  select *,

  from pce_eligible_encounters_with_predictors
  
  where patient_encounter_row_num = 1
  
),

hypertensive_meds as (
  -- Hypertensive treatment prescription of at least 30 days in length
  -- (single administration while an inpatient does not count)
  select  
    pat_id,
    end_date,
    
    case
      when order_inst <= start_date
        or start_date is null
        then order_inst
        
      when start_date <= order_inst
        or order_inst is null
        then start_date
    
    end as min_order_start_date
    
  from medications
  
  where
    atc_code like '%C02%'     -- antihypertensives
    or atc_code like '%C03%'  -- diuretics
    or atc_code like '%C07%'  -- beta blocking agents
    or atc_code like '%C08%'  -- calcium channel blockers
    or atc_code like '%C09%'  -- agents acting on the renin-angiotensin system
    
    -- at least 30 supply ordered
    and date_diff('day', order_inst, end_date) >=30
    
),

current_hypertensive_meds as (
  -- Get hypertension medication indicator of whether treatment prescription
  -- was active at the time of the 'index' encounter
  select
    hypertensive_meds.pat_id,
    min(min_order_start_date) as current_hypertensive_med_start_date,
    min(end_date) as current_hypertensive_med_end_date
    
  from hypertensive_meds
  
    inner join index_encounters_with_predictors
      on hypertensive_meds.pat_id
        = index_encounters_with_predictors.pat_id

  where
    contact_date between min_order_start_date and end_date
      
  group by hypertensive_meds.pat_id

),

complete_dataset as (
  
  -- Add current_hypertension_med indicator,
  --     ASCVD outcome date,
  --     convenience flags (0/1)
  -- to the final dataset
  select
    *,
  
    case
      -- at any time in the past (no lookback specified)
      when diabetes_date <= contact_date
        then 1
      else 0
    end as diabetes_flag,
    
    case
      -- at any time in the past (no lookback specified)
      when hypertension_date <= contact_date
        then 1
      else 0
    end as hypertension_flag,
    
    case
      -- at any time in the past (no lookback specified)
      when dyslipidemia_date <= contact_date
        then 1
      else 0
    end as dyslipidemia_flag,
    
    case
      -- look-back period for "current" smoking status is 180 days
      when date_diff('day', smoking_status_date, contact_date) >= 0
        and date_diff('day', smoking_status_date, contact_date) <= 180
        then 1
      else 0
    end as current_smoker_flag,
    
    case
      -- 30+ day supply of medication overlapping the index encounter
      -- see CTE "current_hypertensive_meds"
      when current_hypertensive_med_start_date is not null
        then 1
      else 0
    end as current_hypertensive_med_flag,
    
    case
      when date_diff('year', contact_date, ascvd_date) <= 5
        then 1
      else 0
    end as ascvd_5yr_flag,
    
    case
      when date_diff('year', contact_date, ascvd_date) <= 10
        then 1
      else 0
    end as ascvd_10yr_flag
    
  from index_encounters_with_predictors encs
  
    left join current_hypertensive_meds
      using (pat_id)
      --on encs.pat_id = current_hypertensive_meds.pat_id
    
    left join ascvd_events
      using (pat_id)
      --on encs.pat_id = ascvd_events.pat_id
      
),

censoring_components as (
  
  -- Calculate end of follow-up for ASCVD
  -- Normally, this would be min(event, death, last observation, end of study)
  --  but the inclusion of event data beyond patients' last observation
  --  (from the National Death Index; NDI) requires a two-step approach
  select
    pat_id,
    contact_date,
    ascvd_date,
    death_date,
    ndi_death_date,
    last_discharge_or_contact_date,

    -- ASCVD
    -- min(event, end of study)
    -- to account for events captured after last observation
    ( select min(v)
      from (values (ascvd_date),
                   (date_add(contact_date, interval 10 year))
            ) as value(v)
    ) as ascvd_min_event_or_admin_censoring,

    -- min(event, death, last observation, end of study)
    ( select min(v)
      from (values (ascvd_date),
                   (death_date),
                   (ndi_death_date),
                   (last_discharge_or_contact_date),
                   (date_add(contact_date, interval 10 year))
            ) as value (v)
      ) as ascvd_min_event_or_last_observation
      
          
  from complete_dataset
         
),

followup_time as (
  
  -- Calculate the follow-up time for both ASCVD
  
  select
    pat_id,

    -- ASCVD
    case
      when ascvd_date > last_discharge_or_contact_date
      -- for events captured after last observation
      then ascvd_min_event_or_admin_censoring
      -- for everyone else
      else ascvd_min_event_or_last_observation
    end as ascvd_followup_end_date,
    
    case
      when ascvd_date > last_discharge_or_contact_date
      then round(date_diff('day', 
                contact_date, ascvd_min_event_or_admin_censoring)/365.25, 2)
        
      else round(date_diff('day', 
                contact_date, ascvd_min_event_or_last_observation)/365.25, 2)
    end as ascvd_followup_years
    
  from censoring_components
  
)

select *

from complete_dataset

  left join followup_time
    using (pat_id)
    --on complete_dataset.pat_id = followup_time.pat_id