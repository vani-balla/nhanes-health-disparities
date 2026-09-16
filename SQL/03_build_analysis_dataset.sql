-- ============================================================
-- 03_build_analysis_dataset.sql
-- Project: NHANES Health Disparities
--
-- Purpose:
--   Join harmonized NHANES staging tables into one
--   participant-level analysis dataset.
--
-- Backbone:
--   staging.demographics_clean
--
-- Join strategy:
--   LEFT JOIN preserves the full NHANES demographic sample.
-- ============================================================


-- ============================================================
-- 1. CREATE ANALYSIS-READY PARTICIPANT TABLE
-- ============================================================

DROP TABLE IF EXISTS analytics.analysis_ready;

CREATE TABLE analytics.analysis_ready AS

SELECT

    -- --------------------------------------------------------
    -- DEMOGRAPHICS / SURVEY DESIGN
    -- --------------------------------------------------------

    d.participant_id,
    d.participant_key,
    d.cycle,
    d.time_midpoint,

    d.age,
    d.adult_eligible,
    d.age_group,

    d.sex_raw,
    d.sex,

    d.race_ethnicity_raw,
    d.race_ethnicity,

    d.education_raw,
    d.education_group,

    d.poverty_ratio,
    d.income_group,

    d.pregnancy_status_raw,
    d.pregnancy_status,

    d.obesity_eligible,

    d.stratum,
    d.psu,

    d.interview_weight,
    d.mec_weight,


    -- --------------------------------------------------------
    -- HEALTH INSURANCE
    -- --------------------------------------------------------

    i.insured_raw,
    i.insured,


    -- --------------------------------------------------------
    -- USUAL SOURCE OF CARE
    -- --------------------------------------------------------

    u.usual_care_raw,
    u.has_usual_care,


    -- --------------------------------------------------------
    -- DIAGNOSED DIABETES
    -- --------------------------------------------------------

    diab.diabetes_raw,
    diab.diabetes_dx,


    -- --------------------------------------------------------
    -- DIAGNOSED HYPERTENSION
    -- --------------------------------------------------------

    hyp.hypertension_raw,
    hyp.hypertension_dx,


    -- --------------------------------------------------------
    -- BMI / OBESITY
    -- --------------------------------------------------------

    b.bmi,
    b.obesity,


    -- --------------------------------------------------------
    -- HbA1c
    -- --------------------------------------------------------

    a.hba1c,
    a.hba1c_weight


FROM staging.demographics_clean AS d


LEFT JOIN staging.insurance AS i
    ON d.participant_key = i.participant_key


LEFT JOIN staging.usual_care AS u
    ON d.participant_key = u.participant_key


LEFT JOIN staging.diabetes AS diab
    ON d.participant_key = diab.participant_key


LEFT JOIN staging.hypertension AS hyp
    ON d.participant_key = hyp.participant_key


LEFT JOIN staging.bmi AS b
    ON d.participant_key = b.participant_key


LEFT JOIN staging.hba1c AS a
    ON d.participant_key = a.participant_key;

SELECT
    COUNT(*) AS total_rows,
    COUNT(DISTINCT participant_key) AS unique_participant_keys,
    COUNT(*) - COUNT(DISTINCT participant_key) AS duplicate_keys
FROM analytics.analysis_ready;

SELECT
    cycle,
    COUNT(*) AS n_participants
FROM analytics.analysis_ready
GROUP BY cycle
ORDER BY cycle;

SELECT

    COUNT(*) AS total_participants,

    COUNT(insured_raw)
        AS participants_with_insurance_record,

    COUNT(usual_care_raw)
        AS participants_with_usual_care_record,

    COUNT(diabetes_raw)
        AS participants_with_diabetes_record,

    COUNT(hypertension_raw)
        AS participants_with_hypertension_record,

    COUNT(bmi)
        AS participants_with_bmi_measurement,

    COUNT(hba1c)
        AS participants_with_hba1c_measurement

FROM analytics.analysis_ready;

SELECT *
FROM analytics.analysis_ready
LIMIT 20;

SELECT
    cycle,
    COUNT(*) AS total_n,
    SUM(adult_eligible) AS adult_n,
    COUNT(bmi) AS bmi_nonmissing_n,
    COUNT(hba1c) AS hba1c_nonmissing_n
FROM analytics.analysis_ready
GROUP BY cycle
ORDER BY cycle;