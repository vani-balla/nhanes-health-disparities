-- ============================================================
-- 04_data_quality_and_eda.sql
-- Project: NHANES Health Disparities
--
-- Purpose:
--   Final SQL quality assurance and exploratory analysis
--   before survey-weighted analysis in R.
--
-- IMPORTANT:
--   Descriptive percentages in this script are UNWEIGHTED
--   and are used for QA/exploration only.
--   Final population estimates will be calculated in R
--   using NHANES survey weights, strata, and PSUs.
-- ============================================================


-- ============================================================
-- 1. FINAL ROW / KEY INTEGRITY
-- ============================================================

SELECT
    COUNT(*) AS total_rows,
    COUNT(DISTINCT participant_key) AS unique_participant_keys,
    COUNT(*) - COUNT(DISTINCT participant_key) AS duplicate_keys
FROM analytics.analysis_ready;

-- ============================================================
-- 2. PARTICIPANT COUNTS BY SURVEY PERIOD
-- ============================================================

SELECT
    cycle,
    COUNT(*) AS total_n,
    SUM(adult_eligible) AS adult_n
FROM analytics.analysis_ready
GROUP BY cycle
ORDER BY cycle;

-- ============================================================
-- 3. RAW CATEGORICAL CODE AUDIT
-- ============================================================

SELECT
    'sex_raw' AS variable,
    sex_raw::text AS raw_value,
    COUNT(*) AS n
FROM analytics.analysis_ready
GROUP BY sex_raw

UNION ALL

SELECT
    'race_ethnicity_raw',
    race_ethnicity_raw::text,
    COUNT(*)
FROM analytics.analysis_ready
GROUP BY race_ethnicity_raw

UNION ALL

SELECT
    'education_raw',
    education_raw::text,
    COUNT(*)
FROM analytics.analysis_ready
GROUP BY education_raw

UNION ALL

SELECT
    'pregnancy_status_raw',
    pregnancy_status_raw::text,
    COUNT(*)
FROM analytics.analysis_ready
GROUP BY pregnancy_status_raw

UNION ALL

SELECT
    'insured_raw',
    insured_raw::text,
    COUNT(*)
FROM analytics.analysis_ready
GROUP BY insured_raw

UNION ALL

SELECT
    'usual_care_raw',
    usual_care_raw::text,
    COUNT(*)
FROM analytics.analysis_ready
GROUP BY usual_care_raw

UNION ALL

SELECT
    'diabetes_raw',
    diabetes_raw::text,
    COUNT(*)
FROM analytics.analysis_ready
GROUP BY diabetes_raw

UNION ALL

SELECT
    'hypertension_raw',
    hypertension_raw::text,
    COUNT(*)
FROM analytics.analysis_ready
GROUP BY hypertension_raw

ORDER BY variable, raw_value;

-- ============================================================
-- 4. VERIFY DEMOGRAPHIC RECODING LOGIC
-- ============================================================

WITH expected AS (

    SELECT
        *,

        CASE
            WHEN age IS NULL THEN NULL
            WHEN age >= 20 THEN 1
            ELSE 0
        END AS expected_adult_eligible,

        CASE
            WHEN age BETWEEN 20 AND 39 THEN '20-39'
            WHEN age BETWEEN 40 AND 59 THEN '40-59'
            WHEN age >= 60 THEN '60+'
            ELSE NULL
        END AS expected_age_group,

        CASE
            WHEN sex_raw = 1 THEN 'Male'
            WHEN sex_raw = 2 THEN 'Female'
            ELSE NULL
        END AS expected_sex,

        CASE
            WHEN race_ethnicity_raw = 1 THEN 'Mexican American'
            WHEN race_ethnicity_raw = 2 THEN 'Other Hispanic'
            WHEN race_ethnicity_raw = 3 THEN 'Non-Hispanic White'
            WHEN race_ethnicity_raw = 4 THEN 'Non-Hispanic Black'
            WHEN race_ethnicity_raw = 6 THEN 'Non-Hispanic Asian'
            WHEN race_ethnicity_raw = 7
                THEN 'Other Race including Multiracial'
            ELSE NULL
        END AS expected_race_ethnicity,

        CASE
            WHEN education_raw IN (1, 2)
                THEN 'Less than high school'
            WHEN education_raw = 3
                THEN 'High school/GED'
            WHEN education_raw = 4
                THEN 'Some college/AA'
            WHEN education_raw = 5
                THEN 'College graduate+'
            ELSE NULL
        END AS expected_education_group,

        CASE
            WHEN poverty_ratio IS NULL THEN NULL
            WHEN poverty_ratio < 1 THEN 'Below poverty'
            WHEN poverty_ratio >= 1 AND poverty_ratio < 2
                THEN '100-199% FPL'
            WHEN poverty_ratio >= 2 AND poverty_ratio < 4
                THEN '200-399% FPL'
            WHEN poverty_ratio >= 4
                THEN '400%+ FPL'
            ELSE NULL
        END AS expected_income_group,

        CASE
            WHEN pregnancy_status_raw = 1 THEN 'Pregnant'
            WHEN pregnancy_status_raw = 2 THEN 'Not pregnant'
            WHEN pregnancy_status_raw = 3 THEN 'Cannot ascertain'
            ELSE NULL
        END AS expected_pregnancy_status,

        CASE
            WHEN age IS NULL THEN NULL
            WHEN age < 20 THEN 0
            WHEN age >= 20
                 AND pregnancy_status_raw = 1 THEN 0
            WHEN age >= 20
                 AND (
                     pregnancy_status_raw IN (2, 3)
                     OR pregnancy_status_raw IS NULL
                 )
                THEN 1
            ELSE NULL
        END AS expected_obesity_eligible

    FROM analytics.analysis_ready
)

SELECT
    SUM(
        CASE WHEN adult_eligible
            IS DISTINCT FROM expected_adult_eligible
        THEN 1 ELSE 0 END
    ) AS adult_eligible_mismatches,

    SUM(
        CASE WHEN age_group
            IS DISTINCT FROM expected_age_group
        THEN 1 ELSE 0 END
    ) AS age_group_mismatches,

    SUM(
        CASE WHEN sex
            IS DISTINCT FROM expected_sex
        THEN 1 ELSE 0 END
    ) AS sex_mismatches,

    SUM(
        CASE WHEN race_ethnicity
            IS DISTINCT FROM expected_race_ethnicity
        THEN 1 ELSE 0 END
    ) AS race_mismatches,

    SUM(
        CASE WHEN education_group
            IS DISTINCT FROM expected_education_group
        THEN 1 ELSE 0 END
    ) AS education_mismatches,

    SUM(
        CASE WHEN income_group
            IS DISTINCT FROM expected_income_group
        THEN 1 ELSE 0 END
    ) AS income_mismatches,

    SUM(
        CASE WHEN pregnancy_status
            IS DISTINCT FROM expected_pregnancy_status
        THEN 1 ELSE 0 END
    ) AS pregnancy_mismatches,

    SUM(
        CASE WHEN obesity_eligible
            IS DISTINCT FROM expected_obesity_eligible
        THEN 1 ELSE 0 END
    ) AS obesity_eligibility_mismatches

FROM expected;

-- ============================================================
-- 5. VERIFY HEALTH / ACCESS RECODING LOGIC
-- ============================================================

SELECT

    SUM(
        CASE
            WHEN insured IS DISTINCT FROM
                CASE
                    WHEN insured_raw = 1 THEN 1
                    WHEN insured_raw = 2 THEN 0
                    ELSE NULL
                END
            THEN 1
            ELSE 0
        END
    ) AS insurance_mismatches,


    SUM(
        CASE
            WHEN has_usual_care IS DISTINCT FROM
                CASE
                    WHEN usual_care_raw = 1 THEN 1
                    WHEN usual_care_raw = 2 THEN 0
                    WHEN usual_care_raw = 3 THEN 1
                    ELSE NULL
                END
            THEN 1
            ELSE 0
        END
    ) AS usual_care_mismatches,


    SUM(
        CASE
            WHEN diabetes_dx IS DISTINCT FROM
                CASE
                    WHEN diabetes_raw = 1 THEN 1
                    WHEN diabetes_raw IN (2, 3) THEN 0
                    ELSE NULL
                END
            THEN 1
            ELSE 0
        END
    ) AS diabetes_mismatches,


    SUM(
        CASE
            WHEN hypertension_dx IS DISTINCT FROM
                CASE
                    WHEN hypertension_raw = 1 THEN 1
                    WHEN hypertension_raw = 2 THEN 0
                    ELSE NULL
                END
            THEN 1
            ELSE 0
        END
    ) AS hypertension_mismatches,


    SUM(
        CASE
            WHEN obesity IS DISTINCT FROM
                CASE
                    WHEN obesity_eligible = 0 THEN NULL

                    WHEN obesity_eligible = 1
                         AND bmi IS NULL THEN NULL

                    WHEN obesity_eligible = 1
                         AND bmi >= 30 THEN 1

                    WHEN obesity_eligible = 1
                         AND bmi < 30 THEN 0

                    ELSE NULL
                END
            THEN 1
            ELSE 0
        END
    ) AS obesity_mismatches

FROM analytics.analysis_ready;

-- ============================================================
-- 6. CONTINUOUS VARIABLE RANGE CHECKS
-- ============================================================

SELECT
    MIN(age) AS min_age,
    MAX(age) AS max_age,

    MIN(poverty_ratio) AS min_poverty_ratio,
    MAX(poverty_ratio) AS max_poverty_ratio,

    MIN(bmi) AS min_bmi,
    MAX(bmi) AS max_bmi,

    MIN(hba1c) AS min_hba1c,
    MAX(hba1c) AS max_hba1c

FROM analytics.analysis_ready;

-- ============================================================
-- 7. MISSINGNESS BY SURVEY PERIOD
-- ============================================================

SELECT
    cycle,
    COUNT(*) AS total_n,

    SUM(CASE WHEN insured IS NULL THEN 1 ELSE 0 END)
        AS insured_missing_n,

    SUM(CASE WHEN has_usual_care IS NULL THEN 1 ELSE 0 END)
        AS usual_care_missing_n,

    SUM(CASE WHEN diabetes_dx IS NULL THEN 1 ELSE 0 END)
        AS diabetes_missing_n,

    SUM(CASE WHEN hypertension_dx IS NULL THEN 1 ELSE 0 END)
        AS hypertension_missing_n,

    SUM(CASE WHEN bmi IS NULL THEN 1 ELSE 0 END)
        AS bmi_missing_n,

    SUM(CASE WHEN hba1c IS NULL THEN 1 ELSE 0 END)
        AS hba1c_missing_n

FROM analytics.analysis_ready

GROUP BY cycle
ORDER BY cycle;

-- ============================================================
-- 8. MISSINGNESS PERCENTAGES BY SURVEY PERIOD
-- ============================================================

SELECT
    cycle,

    ROUND(
        100.0 * SUM(CASE WHEN insured IS NULL THEN 1 ELSE 0 END)
        / COUNT(*),
        1
    ) AS insured_missing_pct,

    ROUND(
        100.0 * SUM(CASE WHEN has_usual_care IS NULL THEN 1 ELSE 0 END)
        / COUNT(*),
        1
    ) AS usual_care_missing_pct,

    ROUND(
        100.0 * SUM(CASE WHEN diabetes_dx IS NULL THEN 1 ELSE 0 END)
        / COUNT(*),
        1
    ) AS diabetes_missing_pct,

    ROUND(
        100.0 * SUM(CASE WHEN hypertension_dx IS NULL THEN 1 ELSE 0 END)
        / COUNT(*),
        1
    ) AS hypertension_missing_pct,

    ROUND(
        100.0 * SUM(CASE WHEN bmi IS NULL THEN 1 ELSE 0 END)
        / COUNT(*),
        1
    ) AS bmi_missing_pct,

    ROUND(
        100.0 * SUM(CASE WHEN hba1c IS NULL THEN 1 ELSE 0 END)
        / COUNT(*),
        1
    ) AS hba1c_missing_pct

FROM analytics.analysis_ready

GROUP BY cycle
ORDER BY cycle;

-- ============================================================
-- 9. ADULT ANALYTIC DATA AVAILABILITY
-- ============================================================

SELECT
    cycle,

    COUNT(*) AS adult_n,

    COUNT(insured) AS insured_nonmissing_n,

    COUNT(has_usual_care) AS usual_care_nonmissing_n,

    COUNT(diabetes_dx) AS diabetes_nonmissing_n,

    COUNT(hypertension_dx) AS hypertension_nonmissing_n,

    COUNT(obesity) AS obesity_nonmissing_n,

    COUNT(hba1c) AS hba1c_nonmissing_n

FROM analytics.analysis_ready

WHERE adult_eligible = 1

GROUP BY cycle
ORDER BY cycle;

-- ============================================================
-- 10. UNWEIGHTED ADULT OUTCOME PERCENTAGES
--
-- QA / EXPLORATION ONLY
-- DO NOT report these as national prevalence estimates.
-- Final estimates will use NHANES survey design in R.
-- ============================================================

SELECT
    cycle,

    COUNT(*) AS adult_n,

    ROUND(
        100.0 * AVG(insured),
        1
    ) AS insured_unweighted_pct,

    ROUND(
        100.0 * AVG(has_usual_care),
        1
    ) AS usual_care_unweighted_pct,

    ROUND(
        100.0 * AVG(diabetes_dx),
        1
    ) AS diabetes_unweighted_pct,

    ROUND(
        100.0 * AVG(hypertension_dx),
        1
    ) AS hypertension_unweighted_pct,

    ROUND(
        100.0 * AVG(obesity),
        1
    ) AS obesity_unweighted_pct,

    ROUND(
        AVG(hba1c)::numeric,
        2
    ) AS mean_hba1c_unweighted

FROM analytics.analysis_ready

WHERE adult_eligible = 1

GROUP BY cycle
ORDER BY cycle;

-- ============================================================
-- 11. UNWEIGHTED ADULT SAMPLE SIZE BY RACE/ETHNICITY
-- ============================================================

SELECT
    cycle,
    race_ethnicity,
    COUNT(*) AS n

FROM analytics.analysis_ready

WHERE adult_eligible = 1

GROUP BY
    cycle,
    race_ethnicity

ORDER BY
    cycle,
    race_ethnicity;
