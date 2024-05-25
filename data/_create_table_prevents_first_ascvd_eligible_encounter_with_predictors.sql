/*

_create_table_prevents_first_eligible_encounter_with_predictors.sql

SQL:    duckdb
Author: MCvitano01@jpshealth.org
Date:   2024-05-10


We will apply a 180-day look-back period for predictor availability at 
each eligible encounter.

The earliest encounter at which information for all predictors are available 
will be considered the index encounter. Therefore, no patient will enter 
the cohort more than once.

The PREVENTS model uses the same predictors as the PCE model, excluding raceth:
  - sex
  - age
  - total cholesterol
  - HDL cholesterol
  - systolic blood pressure
  - diabetes (yes,no)
  - current smoking status (yes, no)
  - current hypertensive medication (yes, no)
  
Plus:
  - estimated glomerular filtration rate
  
  
Thus, the base table is:
  - pce_first_ascvd_eligible_encounter_with_predictors
  
Rather than:
  - ascvd_eligible_encounters


The PREVENTS model stratifies on sex (only). This sex filter (i.e., remove 
patients with 'unknown' sex) is applied to the dataset in the analysis rather 
than here in the SQL for transparency sake.
*/
with

glomerular_filtration_rate as (

  select
    labs.pat_id,
    result_time,
    
    cast(result_time as date) as egfr_date,
    
    cast(
      regexp_replace(ord_value, '=|>|<|,', '') as integer
    ) as egfr
    
  from labs
  
    left join patients
      on labs.pat_id = patients.pat_id
  
  where
    -- numeric values only
    try_cast(
      regexp_replace(ord_value, '=|>|<|,', '') as integer
    ) is not null
    
    and (
      (pat_sex = 'Female' and component_id = 1232000244)
      
      or (pat_sex = 'Male' and component_id = 1233000244)
      
      or (patient_race in (
          'BLACK OR AFRICAN AMERICAN;', 
          'BLACK OR AFRICAN AMERICAN; PATIENT REFUSED;'
          ) and component_id in (2415, 3735))
          
      or (patient_race not in (
          'BLACK OR AFRICAN AMERICAN;', 
          'BLACK OR AFRICAN AMERICAN; PATIENT REFUSED;'
          ) and component_id in (2414, 3736))
          
      or component_id in(
        244, 291, 3733, 3740, 3746, 1232000244)
    )
    
),

prevents_eligible_encounters_with_predictors as (

  select
    *,
    
    -- closest in time to eligible encounter
    row_number() over (
      partition by pce.pat_id
      order by result_time desc
    ) as egfr_row_number
  
  -- PCE-eligible dataset
  from pce_first_eligible_encounter_with_predictors as pce
  
    inner join glomerular_filtration_rate
      using (pat_id)
      --on pce.pat_id = glomerular_filtration_rate.pat_id
      
  where
    (date_diff('day', egfr_date, contact_date) between 0 and 180)

)

select * exclude(egfr_row_number)

from prevents_eligible_encounters_with_predictors

where egfr_row_number = 1

