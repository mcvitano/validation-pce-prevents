/*

_create_table_eligible_encounters.sql

SQL:    duckdb
Author: MCvitano01@jpshealth.org
Date:   2024-05-10


Our eligibility criteria will be based on the population relevant to the
USPSTF guidelines for statin prescribing summarized below. 

We will apply these criteria for patients at each PCMH encounter and the 
180-day look-back period for predictor availability will be applied at 
each eligible encounter.

The earliest encounter at which information for all predictors are available 
will be considered the index encounter. Therefore, no patient will enter 
the cohort more than once.

Criteria:
  - In-person or telehealth primary care encounter at one of 12 PCMHs
    between July 1, 2013 and December 31, 2018
  -	Aged 40 – 75 years
  -	One or more cardiovascular risk factors:
     - Dyslipidemia
     - Diabetes (type I or type II)
     - Hypertension (essential/primary)
     - Current tobacco smoker
     
In addition, the following criteria will be applied:
  - No prior CVD event
  -	No prior statin prescription
*/

with

statins as (
  -- statin definition from PREVENTS documentation
  -- doi 0.1161/CIRCULATIONAHA.123.067626
  -- Supplemental table S21
  select
    pat_id,
    min(
      case
        when start_date < order_inst
          then start_date
        else order_inst
      end) as statin_first_date
      
  from medications
  
  where 
    atc_code like '%C10AA%'    -- HMG CoA reductase inhibitors
    or atc_code like '%C10B%'  -- Lipid modifying agents, combinations

  group by pat_id
  
),

ascvd_events as (
  -- CVD definition from PREVENTS documentation
  -- doi 0.1161/CIRCULATIONAHA.123.067626
  -- Supplemental table S22
  select
    pat_id,
    min(contact_date) as ascvd_first_date
    
  from diagnoses
  
  where
    current_icd10_list like '%I21%'     -- acute myocardial infarction
    or current_icd10_list like '%I22%'  -- subsequent ST- and non-ST elevation
    or current_icd10_list like '%I61%'  -- nontraumatic intracerebral hemorrhage
    or current_icd10_list like '%I62%'  -- other nontraumatic intracranial
    or current_icd10_list like '%I63%'  -- cerebral infarction

  group by pat_id
  
),

dyslipidemia as (
  -- Sourced from UpToDate; high-cholesterol-and-lipids-beyond-the-basics
  select
    pat_id,
    min(result_time) as dyslipidemia_date
    
  from labs
  
  where
    (
      -- total cholesterol >= 200
      component_id in (1510312, 1557760, 1230000048)
      and try_cast(
        regexp_replace(ord_value, '=|>|<|,', '') as integer) >= 200
    )
    or (
      -- LDL >= 130
      component_id in (2999, 30400410, 1811463, 1230000143, 1230001313)
      and try_cast(
        regexp_replace(ord_value, '=|>|<|,', '') as integer) >= 130
    )
    or (
      -- non-HDL >= 160
      component_id in (1811467)
      and try_cast(
        regexp_replace(ord_value, '=|>|<|,', '') as integer) >= 160
    )
    or (
      -- triglycerides >= 150
      component_id in (1552156, 1230000190)
      and try_cast(
        regexp_replace(ord_value, '=|>|<|,', '')as integer) >= 150
    )
 
  group by pat_id
  
),

diabetes_dx as (
  
  select
    pat_id,
    min(contact_date) as diabetes_dx_date
    
  from diagnoses
  
  where
    current_icd10_list like '%E10%'     -- type I diabetes
    or current_icd10_list like '%E11%'  -- type II diabetes
    or current_icd10_list like '%E13%'  -- other specified diabetes

  group by pat_id

),

diabetes_meds as (

  select
    pat_id,
    min(
      case
        when start_date < order_inst
          then start_date
        else order_inst
      end
    ) as diabetes_med_date
    
  from medications
  
  where
    atc_code like '%A10%'   -- diabetes treatment
    and atc_code not like '%A10BA%'  -- metformin (often used off-label)
    
  group by pat_id

),

diabetes_definition as (
  
  select
    pat_id,
    min(diabetes_dx_date) as diabetes_dx_date,
    min(diabetes_med_date) as diabetes_med_date,
    min(
      case
        when diabetes_dx_date <= diabetes_med_date
          or diabetes_med_date is null
          then diabetes_dx_date
          
        when diabetes_med_date <= diabetes_dx_date
          or diabetes_dx_date is null
          then diabetes_med_date
          
      end
    ) as diabetes_date
      
  from (
    select *
    from diabetes_dx
    
    union by name
    
    select * 
    from diabetes_meds
  )
    
  group by pat_id

),

hypertension_dx as (

  select
    pat_id,
    min(contact_date) as hypertension_dx_date
    
  from diagnoses
  
  where
    current_icd10_list like '%I10%'      -- essential (primary) hypertension
    or current_icd10_list like '%I11%'   -- hypertensive heart disease 
    or current_icd10_list like '%I12%'   -- hypertensive chronic kidney disease
    or current_icd10_list like '%I13%'   -- hypertensive heart and CKD
    or current_icd10_list like '%I15%'   -- secondary hypertension
    
  group by pat_id

),

hypertension_meds_all as (
  -- hypertension medication list from PREVENTS documentation
  -- doi 0.1161/CIRCULATIONAHA.123.067626
  -- Supplemental table S21
  select
    pat_id,
    order_inst,
    start_date,
    
    row_number() over(
      partition by pat_id
      order by order_inst
    ) as hypertension_order_row
    
  from medications
  
  where
    atc_code like '%C02%'     -- antihypertensives
    or atc_code like '%C03%'  -- diuretics
    or atc_code like '%C07%'  -- beta blocking agents
    or atc_code like '%C08%'  -- calcium channel blockers
    or atc_code like '%C09%'  -- agents acting on the renin-angiotensin system

),

hypertension_meds_second as (
  -- Get second (2nd) hypertension medication order
  --  to increase specificity (versus requiring a single order)
  -- Sourced from
  --  Fransoo R, Mahar A, The Need to Know Team, Anderson A, Prior H, Koseva I, McCulloch S, Jarmasz J, Burchill S. The 2019 RHA Indicators Atlas. Winnipeg, MB: Manitoba Centre for Health Policy, 2019.
  select
    pat_id,
    min(
      case
        when order_inst <= start_date
          or start_date is null
          then order_inst
          
        when start_date <= order_inst
          or order_inst is null
          then start_date
      end
    ) as hypertension_med_date
    
  from hypertension_meds_all
  
  where
    -- second medication order as per Fransoo et. al. 2019
    hypertension_order_row = 2
    
  group by pat_id
  
),

hypertension_definition as (
  -- Combine hypertension medication and diagnosis dates
  select
    pat_id,
    min(hypertension_dx_date) as hypertension_dx_date,
    min(hypertension_med_date) as hypertension_med_date,
    -- set equal to minimum of (medication, diagnosis) date
    min(
      case
        when hypertension_dx_date <= hypertension_med_date
          or hypertension_med_date is null
          then hypertension_dx_date
          
        when hypertension_med_date <= hypertension_dx_date
          or hypertension_dx_date is null
          then hypertension_med_date
          
      end
    ) as hypertension_date
      
  from (
    select *
    from hypertension_dx
    
    union by name
    
    select * 
    from hypertension_meds_second
  )
    
  group by pat_id
  
),

current_smoker as (
  
  select
    pat_id,
    contact_date as smoking_status_date
  
  from social_history
  
  where
    smoking_tob_use_nm in (
      'Some Days',
      'Every Day',
      'Light Smoker',
      'Heavy Smoker'
      -- 'Former Smoker',
      -- 'Smoker, Current Status Unknown'
    )
    
),

demographics as (

  select
    pat_id,
    pat_mrn_id,
    birth_date,
    death_date,
    
    cast(last_discharge_or_contact_date as date)
      as last_discharge_or_contact_date,

    lower(coalesce(pat_sex, 'unknown')) as sex,

    case
        -- original value is 'Hispanic, Latino or Spanish ethnicity'
        -- but was shortened to 'Hispanic' because the comma (',')
        -- was causing trouble when attempting to read the 
        -- demographics file into memory (the line-endings were also
        -- corrupt)
        when ethnic_group_nm = 'Hispanic'
            then 'hispanic'
        when patient_race ilike '%black%'
            then 'nh black'
        when
            patient_race like 'asian%'
            or patient_race ilike '%indian%'
            or patient_race ilike '%hawaiian%'
            or patient_race ilike '%other%'
            then 'nh other'
        when patient_race ilike '%caucasian%'
            then 'nh white'
        else 'unknown'
    end as raceth,

    case
        when language_nm = 'English'
            then 'english'
        when language_nm = 'Spanish'
            then 'spanish'
        when
            language_nm in (
                'Deaf (none ASL)',
                'American Sign Language'
            )
            then 'american sign language'
        when
            language_nm = 'Unknown'
            or language_nm is null
            then 'unknown'
        else 'other'
    end as language_primary,

    case
        when
            marital_status_nm in (
                'Divorced',
                'Legally Separated',
                'Single',
                'Windowed'
            )
            then 'single'
        when
            marital_status_nm in (
                'Common Law',
                'Life Partner',
                'Married',
                'Significant Other'
            )
            then 'in a relationship'
        when marital_status_nm = 'Other'
            then 'other'
        else 'unknown'
    end as relationship_status
    
  from patients
  
),

ndi_mortality_records as (
  -- partial set available for some CEHDR registry patients
  select
    pat_id,
    -- *should* include only 1 "match" per patient
    min(death_date) as ndi_death_date
    
  from ndi_cause_of_death
  
  group by pat_id
  
),

pcmh_visits as (

  select
    pat_id,
    pat_enc_csn_id,
    contact_date,
    -- only 'Completed' appointments at the main PCMH clinic(s)
    --  are included in the raw data
    department_name,
    
    case
      when product_type_nm in ('COMMERCIAL', 'NON-CONTRACTED COMMERCIAL')
          then 'commercial'
      when product_type_nm in ('MEDICARE', 'MANAGED MEDICARE')
          then 'medicare'
      when product_type_nm in ('MEDICAID', 'MANAGED MEDICAID')
          then 'medicaid'
      when product_type_nm in ('CHARITY')
          then 'hospital-based medical assistance'
      when product_type_nm in ('GRANTS')
          then 'other'
      when product_type_nm in ('GOVERNMENT OTHER')
          then 'other'
      when product_type_nm in ('SELF PAY', 'SELF PAY PENDING')
          then 'self pay'
      else 'unknown'
    end as insurance
    
  from encounters
  
  where
    contact_date >= '2013-07-01'
    and contact_date <= '2018-12-31'

)

select
  pcmh_visits.pat_id,
  pat_mrn_id,
  pat_enc_csn_id,
  contact_date,
  department_name,
  insurance,
  sex,
  raceth,
  language_primary,
  relationship_status,
  death_date,
  ndi_death_date,
  statin_first_date,
  ascvd_first_date,
  dyslipidemia_date,
  diabetes_dx_date,
  diabetes_med_date,
  diabetes_date,
  hypertension_dx_date,
  hypertension_med_date,
  hypertension_date,
  smoking_status_date,
  
  date_diff('year', birth_date, contact_date) as age_yrs,

  -- On chart review the dozen, or so, instances in which the death date
  --  precedes the final observed contact date all checked out (e.g., the 
  --  death occurred during a hospital admission so the death_date predates
  --  the last_discharge_or_contact_date).
  case
    when last_discharge_or_contact_date > death_date
    then death_date
    
    when last_discharge_or_contact_date > ndi_death_date
    then ndi_death_date
    
    else last_discharge_or_contact_date
    
  end as last_discharge_or_contact_date
  
from pcmh_visits

  left join statins
    on pcmh_visits.pat_id = statins.pat_id
  
  left join ascvd_events
    on pcmh_visits.pat_id = ascvd_events.pat_id

  left join dyslipidemia
    on pcmh_visits.pat_id = dyslipidemia.pat_id

  left join diabetes_definition
    on pcmh_visits.pat_id = diabetes_definition.pat_id

  left join hypertension_definition
    on pcmh_visits.pat_id = hypertension_definition.pat_id

  left join demographics
    on pcmh_visits.pat_id = demographics.pat_id

  left join current_smoker
    on pcmh_visits.pat_id = current_smoker.pat_id
    
  left join ndi_mortality_records
    on pcmh_visits.pat_id = ndi_mortality_records.pat_id
  
where 1=1
  -- no previous statin or CVD event
  and (contact_date < statin_first_date or statin_first_date is null)
  and (contact_date < ascvd_first_date or ascvd_first_date is null)
  
  -- USPSTF guideline: ages 40-75 years
  and date_diff('year', birth_date, contact_date) >= 40
  and date_diff('year', birth_date, contact_date) <= 75
  
  -- USPSTF guideline; must have 1 of 4 conditions
  and (contact_date >= dyslipidemia_date or dyslipidemia_date is null)
  and (contact_date >= diabetes_date or diabetes_date is null)
  and (contact_date >= hypertension_date or hypertension_date is null)
  and (
    dyslipidemia_date is not null
    or diabetes_date is not null
    or hypertension_date is not null
    -- look-back period for 'current' smoking status is 180 days
    or (
      date_diff('day', smoking_status_date, contact_date) >= 0
      and date_diff('day', smoking_status_date, contact_date) <= 180
    )
  )