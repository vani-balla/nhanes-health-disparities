-- ============================================================
-- 02_build_staging_tables.sql
-- Project: NHANES Health Disparities
--
-- Purpose:
--   Harmonize NHANES variables across survey periods
--   before creating the final analysis dataset.
-- ============================================================


-- ============================================================
-- 1. DEMOGRAPHICS
-- ============================================================

DROP TABLE IF EXISTS staging.demographics;

CREATE TABLE staging.demographics AS


-- ----------------------------
-- 2013-2014
-- ----------------------------

SELECT
    '2013-2014' AS cycle,
    2014.0 AS time_midpoint,

    seqn::bigint AS participant_id,

    '2013-2014-' || seqn::bigint::text
        AS participant_key,

    ridageyr::integer AS age,
    riagendr::integer AS sex_raw,
    ridreth3::integer AS race_ethnicity_raw,
    dmdeduc2::integer AS education_raw,
    indfmpir::double precision AS poverty_ratio,
    ridexprg::integer AS pregnancy_status_raw,

    sdmvstra::integer AS stratum,
    sdmvpsu::integer AS psu,

    wtint2yr::double precision AS interview_weight,
    wtmec2yr::double precision AS mec_weight

FROM raw.demo_2013_2014


UNION ALL


-- ----------------------------
-- 2015-2016
-- ----------------------------

SELECT
    '2015-2016' AS cycle,
    2016.0 AS time_midpoint,

    seqn::bigint AS participant_id,

    '2015-2016-' || seqn::bigint::text
        AS participant_key,

    ridageyr::integer AS age,
    riagendr::integer AS sex_raw,
    ridreth3::integer AS race_ethnicity_raw,
    dmdeduc2::integer AS education_raw,
    indfmpir::double precision AS poverty_ratio,
    ridexprg::integer AS pregnancy_status_raw,

    sdmvstra::integer AS stratum,
    sdmvpsu::integer AS psu,

    wtint2yr::double precision AS interview_weight,
    wtmec2yr::double precision AS mec_weight

FROM raw.demo_2015_2016


UNION ALL


-- ----------------------------
-- 2017-March 2020
-- ----------------------------

SELECT
    '2017-2020' AS cycle,
    2018.6 AS time_midpoint,

    seqn::bigint AS participant_id,

    '2017-2020-' || seqn::bigint::text
        AS participant_key,

    ridageyr::integer AS age,
    riagendr::integer AS sex_raw,
    ridreth3::integer AS race_ethnicity_raw,
    dmdeduc2::integer AS education_raw,
    indfmpir::double precision AS poverty_ratio,
    ridexprg::integer AS pregnancy_status_raw,

    sdmvstra::integer AS stratum,
    sdmvpsu::integer AS psu,

    wtintprp::double precision AS interview_weight,
    wtmecprp::double precision AS mec_weight

FROM raw.demo_2017_2020


UNION ALL


-- ----------------------------
-- 2021-2023
-- ----------------------------

SELECT
    '2021-2023' AS cycle,
    2022.7 AS time_midpoint,

    seqn::bigint AS participant_id,

    '2021-2023-' || seqn::bigint::text
        AS participant_key,

    ridageyr::integer AS age,
    riagendr::integer AS sex_raw,
    ridreth3::integer AS race_ethnicity_raw,
    dmdeduc2::integer AS education_raw,
    indfmpir::double precision AS poverty_ratio,
    ridexprg::integer AS pregnancy_status_raw,

    sdmvstra::integer AS stratum,
    sdmvpsu::integer AS psu,

    wtint2yr::double precision AS interview_weight,
    wtmec2yr::double precision AS mec_weight

FROM raw.demo_2021_2023;

-- Check number of participants in each survey period
SELECT
    cycle,
    COUNT(*) AS n_participants
FROM staging.demographics
GROUP BY cycle
ORDER BY cycle;

SELECT COUNT(*) AS total_rows
FROM staging.demographics;

SELECT
    COUNT(*) AS total_rows,
    COUNT(DISTINCT participant_key) AS unique_participant_keys,
    COUNT(*) - COUNT(DISTINCT participant_key) AS duplicate_keys
FROM staging.demographics;

SELECT *
FROM staging.demographics
LIMIT 10;

-- ============================================================
-- 2. CLEAN / RECODE DEMOGRAPHIC VARIABLES
-- ============================================================

DROP TABLE IF EXISTS staging.demographics_clean;

CREATE TABLE staging.demographics_clean AS

SELECT
    d.*,

    -- --------------------------------------------------------
    -- Adult eligibility
    -- Adults are defined as age 20+
    -- --------------------------------------------------------
    CASE
        WHEN age IS NULL THEN NULL
        WHEN age >= 20 THEN 1
        ELSE 0
    END AS adult_eligible,


    -- --------------------------------------------------------
    -- Age group
    -- Defined only for adults age 20+
    -- --------------------------------------------------------
    CASE
        WHEN age BETWEEN 20 AND 39 THEN '20-39'
        WHEN age BETWEEN 40 AND 59 THEN '40-59'
        WHEN age >= 60 THEN '60+'
        ELSE NULL
    END AS age_group,


    -- --------------------------------------------------------
    -- Sex
    -- RIAGENDR:
    -- 1 = Male
    -- 2 = Female
    -- --------------------------------------------------------
    CASE
        WHEN sex_raw = 1 THEN 'Male'
        WHEN sex_raw = 2 THEN 'Female'
        ELSE NULL
    END AS sex,


    -- --------------------------------------------------------
    -- Race / ethnicity
    -- RIDRETH3
    -- --------------------------------------------------------
    CASE
        WHEN race_ethnicity_raw = 1
            THEN 'Mexican American'

        WHEN race_ethnicity_raw = 2
            THEN 'Other Hispanic'

        WHEN race_ethnicity_raw = 3
            THEN 'Non-Hispanic White'

        WHEN race_ethnicity_raw = 4
            THEN 'Non-Hispanic Black'

        WHEN race_ethnicity_raw = 6
            THEN 'Non-Hispanic Asian'

        WHEN race_ethnicity_raw = 7
            THEN 'Other Race including Multiracial'

        ELSE NULL
    END AS race_ethnicity,


    -- --------------------------------------------------------
    -- Education
    -- DMDEDUC2
    -- --------------------------------------------------------
    CASE
        WHEN education_raw IN (1, 2)
            THEN 'Less than high school'

        WHEN education_raw = 3
            THEN 'High school/GED'

        WHEN education_raw = 4
            THEN 'Some college/AA'

        WHEN education_raw = 5
            THEN 'College graduate+'

        -- 7 = Refused
        -- 9 = Don't know
        ELSE NULL
    END AS education_group,


    -- --------------------------------------------------------
    -- Family income-to-poverty ratio
    -- INDFMPIR
    -- --------------------------------------------------------
    CASE
        WHEN poverty_ratio IS NULL
            THEN NULL

        WHEN poverty_ratio < 1
            THEN 'Below poverty'

        WHEN poverty_ratio >= 1
             AND poverty_ratio < 2
            THEN '100-199% FPL'

        WHEN poverty_ratio >= 2
             AND poverty_ratio < 4
            THEN '200-399% FPL'

        WHEN poverty_ratio >= 4
            THEN '400%+ FPL'

        ELSE NULL
    END AS income_group,


    -- --------------------------------------------------------
    -- Pregnancy status
    -- RIDEXPRG
    --
    -- Important:
    -- RIDEXPRG is only publicly released for women age 20-44.
    -- Therefore, NULL does NOT automatically mean unknown
    -- pregnancy status for every participant.
    -- --------------------------------------------------------
    CASE
        WHEN pregnancy_status_raw = 1
            THEN 'Pregnant'

        WHEN pregnancy_status_raw = 2
            THEN 'Not pregnant'

        WHEN pregnancy_status_raw = 3
            THEN 'Cannot ascertain'

        ELSE NULL
    END AS pregnancy_status,


    -- --------------------------------------------------------
    -- Eligibility for obesity analysis
    --
    -- Adults age 20+ are eligible except participants
    -- known to be pregnant.
    --
    -- Missing RIDEXPRG is NOT treated as pregnancy because
    -- this variable is only released for women age 20-44.
    -- --------------------------------------------------------
    CASE
        WHEN age IS NULL
            THEN NULL

        WHEN age < 20
            THEN 0

        WHEN age >= 20
             AND pregnancy_status_raw = 1
            THEN 0

        WHEN age >= 20
             AND (
                 pregnancy_status_raw IN (2, 3)
                 OR pregnancy_status_raw IS NULL
             )
            THEN 1

        ELSE NULL
    END AS obesity_eligible


FROM staging.demographics AS d;

SELECT
    COUNT(*) AS total_rows,
    COUNT(DISTINCT participant_key) AS unique_participant_keys,
    COUNT(*) - COUNT(DISTINCT participant_key) AS duplicate_keys
FROM staging.demographics_clean;

SELECT
    adult_eligible,
    COUNT(*) AS n
FROM staging.demographics_clean
GROUP BY adult_eligible
ORDER BY adult_eligible;

SELECT
    age_group,
    COUNT(*) AS n
FROM staging.demographics_clean
GROUP BY age_group
ORDER BY age_group;

SELECT
    participant_key,
    cycle,
    age,
    adult_eligible,
    age_group,
    sex_raw,
    sex,
    race_ethnicity_raw,
    race_ethnicity,
    education_raw,
    education_group,
    poverty_ratio,
    income_group,
    pregnancy_status_raw,
    pregnancy_status,
    obesity_eligible
FROM staging.demographics_clean
LIMIT 20;

-- ============================================================
-- 3. HEALTH INSURANCE
-- Source variable: HIQ011
-- ============================================================

DROP TABLE IF EXISTS staging.insurance;

CREATE TABLE staging.insurance AS

SELECT
    '2013-2014' AS cycle,
    seqn::bigint AS participant_id,
    '2013-2014-' || seqn::bigint::text AS participant_key,
    hiq011::integer AS insured_raw,

    CASE
        WHEN hiq011 = 1 THEN 1
        WHEN hiq011 = 2 THEN 0
        ELSE NULL
    END AS insured

FROM raw.hiq_2013_2014

UNION ALL

SELECT
    '2015-2016',
    seqn::bigint,
    '2015-2016-' || seqn::bigint::text,
    hiq011::integer,

    CASE
        WHEN hiq011 = 1 THEN 1
        WHEN hiq011 = 2 THEN 0
        ELSE NULL
    END

FROM raw.hiq_2015_2016

UNION ALL

SELECT
    '2017-2020',
    seqn::bigint,
    '2017-2020-' || seqn::bigint::text,
    hiq011::integer,

    CASE
        WHEN hiq011 = 1 THEN 1
        WHEN hiq011 = 2 THEN 0
        ELSE NULL
    END

FROM raw.hiq_2017_2020

UNION ALL

SELECT
    '2021-2023',
    seqn::bigint,
    '2021-2023-' || seqn::bigint::text,
    hiq011::integer,

    CASE
        WHEN hiq011 = 1 THEN 1
        WHEN hiq011 = 2 THEN 0
        ELSE NULL
    END

FROM raw.hiq_2021_2023;

-- ============================================================
-- 4. USUAL SOURCE OF CARE
-- Source variable: HUQ030
-- ============================================================

DROP TABLE IF EXISTS staging.usual_care;

CREATE TABLE staging.usual_care AS

SELECT
    '2013-2014' AS cycle,
    seqn::bigint AS participant_id,
    '2013-2014-' || seqn::bigint::text AS participant_key,
    huq030::integer AS usual_care_raw,

    CASE
        WHEN huq030 = 1 THEN 1
        WHEN huq030 = 2 THEN 0
        WHEN huq030 = 3 THEN 1
        ELSE NULL
    END AS has_usual_care

FROM raw.huq_2013_2014

UNION ALL

SELECT
    '2015-2016',
    seqn::bigint,
    '2015-2016-' || seqn::bigint::text,
    huq030::integer,

    CASE
        WHEN huq030 = 1 THEN 1
        WHEN huq030 = 2 THEN 0
        WHEN huq030 = 3 THEN 1
        ELSE NULL
    END

FROM raw.huq_2015_2016

UNION ALL

SELECT
    '2017-2020',
    seqn::bigint,
    '2017-2020-' || seqn::bigint::text,
    huq030::integer,

    CASE
        WHEN huq030 = 1 THEN 1
        WHEN huq030 = 2 THEN 0
        WHEN huq030 = 3 THEN 1
        ELSE NULL
    END

FROM raw.huq_2017_2020

UNION ALL

SELECT
    '2021-2023',
    seqn::bigint,
    '2021-2023-' || seqn::bigint::text,
    huq030::integer,

    CASE
        WHEN huq030 = 1 THEN 1
        WHEN huq030 = 2 THEN 0
        WHEN huq030 = 3 THEN 1
        ELSE NULL
    END

FROM raw.huq_2021_2023;

-- ============================================================
-- 5. DIAGNOSED DIABETES
-- Source variable: DIQ010
-- ============================================================

DROP TABLE IF EXISTS staging.diabetes;

CREATE TABLE staging.diabetes AS

SELECT
    '2013-2014' AS cycle,
    seqn::bigint AS participant_id,
    '2013-2014-' || seqn::bigint::text AS participant_key,
    diq010::integer AS diabetes_raw,

    CASE
        WHEN diq010 = 1 THEN 1
        WHEN diq010 IN (2, 3) THEN 0
        ELSE NULL
    END AS diabetes_dx

FROM raw.diq_2013_2014

UNION ALL

SELECT
    '2015-2016',
    seqn::bigint,
    '2015-2016-' || seqn::bigint::text,
    diq010::integer,

    CASE
        WHEN diq010 = 1 THEN 1
        WHEN diq010 IN (2, 3) THEN 0
        ELSE NULL
    END

FROM raw.diq_2015_2016

UNION ALL

SELECT
    '2017-2020',
    seqn::bigint,
    '2017-2020-' || seqn::bigint::text,
    diq010::integer,

    CASE
        WHEN diq010 = 1 THEN 1
        WHEN diq010 IN (2, 3) THEN 0
        ELSE NULL
    END

FROM raw.diq_2017_2020

UNION ALL

SELECT
    '2021-2023',
    seqn::bigint,
    '2021-2023-' || seqn::bigint::text,
    diq010::integer,

    CASE
        WHEN diq010 = 1 THEN 1
        WHEN diq010 IN (2, 3) THEN 0
        ELSE NULL
    END

FROM raw.diq_2021_2023;

-- ============================================================
-- 6. DIAGNOSED HYPERTENSION
-- Source variable: BPQ020
-- ============================================================

DROP TABLE IF EXISTS staging.hypertension;

CREATE TABLE staging.hypertension AS

SELECT
    '2013-2014' AS cycle,
    seqn::bigint AS participant_id,
    '2013-2014-' || seqn::bigint::text AS participant_key,
    bpq020::integer AS hypertension_raw,

    CASE
        WHEN bpq020 = 1 THEN 1
        WHEN bpq020 = 2 THEN 0
        ELSE NULL
    END AS hypertension_dx

FROM raw.bpq_2013_2014

UNION ALL

SELECT
    '2015-2016',
    seqn::bigint,
    '2015-2016-' || seqn::bigint::text,
    bpq020::integer,

    CASE
        WHEN bpq020 = 1 THEN 1
        WHEN bpq020 = 2 THEN 0
        ELSE NULL
    END

FROM raw.bpq_2015_2016

UNION ALL

SELECT
    '2017-2020',
    seqn::bigint,
    '2017-2020-' || seqn::bigint::text,
    bpq020::integer,

    CASE
        WHEN bpq020 = 1 THEN 1
        WHEN bpq020 = 2 THEN 0
        ELSE NULL
    END

FROM raw.bpq_2017_2020

UNION ALL

SELECT
    '2021-2023',
    seqn::bigint,
    '2021-2023-' || seqn::bigint::text,
    bpq020::integer,

    CASE
        WHEN bpq020 = 1 THEN 1
        WHEN bpq020 = 2 THEN 0
        ELSE NULL
    END

FROM raw.bpq_2021_2023;

-- ============================================================
-- 7. BMI AND OBESITY
-- Source variable: BMXBMI
-- ============================================================

DROP TABLE IF EXISTS staging.bmi;

CREATE TABLE staging.bmi AS

WITH bmi_combined AS (

    SELECT
        '2013-2014' AS cycle,
        seqn::bigint AS participant_id,
        '2013-2014-' || seqn::bigint::text AS participant_key,
        bmxbmi::double precision AS bmi
    FROM raw.bmx_2013_2014

    UNION ALL

    SELECT
        '2015-2016',
        seqn::bigint,
        '2015-2016-' || seqn::bigint::text,
        bmxbmi::double precision
    FROM raw.bmx_2015_2016

    UNION ALL

    SELECT
        '2017-2020',
        seqn::bigint,
        '2017-2020-' || seqn::bigint::text,
        bmxbmi::double precision
    FROM raw.bmx_2017_2020

    UNION ALL

    SELECT
        '2021-2023',
        seqn::bigint,
        '2021-2023-' || seqn::bigint::text,
        bmxbmi::double precision
    FROM raw.bmx_2021_2023
)

SELECT
    b.cycle,
    b.participant_id,
    b.participant_key,
    b.bmi,
    d.obesity_eligible,

    CASE
        -- Participant is not eligible for obesity analysis
        WHEN d.obesity_eligible = 0 THEN NULL

        -- Eligible, but BMI itself is missing
        WHEN d.obesity_eligible = 1
             AND b.bmi IS NULL THEN NULL

        -- BMI >= 30
        WHEN d.obesity_eligible = 1
             AND b.bmi >= 30 THEN 1

        -- BMI below 30
        WHEN d.obesity_eligible = 1
             AND b.bmi < 30 THEN 0

        ELSE NULL
    END AS obesity

FROM bmi_combined AS b

LEFT JOIN staging.demographics_clean AS d
    ON b.participant_key = d.participant_key;

-- ============================================================
-- 8. HbA1c
-- Source variable: LBXGH
-- ============================================================

DROP TABLE IF EXISTS staging.hba1c;

CREATE TABLE staging.hba1c AS

SELECT
    '2013-2014' AS cycle,
    seqn::bigint AS participant_id,
    '2013-2014-' || seqn::bigint::text AS participant_key,
    lbxgh::double precision AS hba1c,
    d.mec_weight AS hba1c_weight

FROM raw.ghb_2013_2014 AS g

LEFT JOIN staging.demographics_clean AS d
    ON '2013-2014-' || g.seqn::bigint::text
       = d.participant_key


UNION ALL


SELECT
    '2015-2016',
    seqn::bigint,
    '2015-2016-' || seqn::bigint::text,
    lbxgh::double precision,
    d.mec_weight

FROM raw.ghb_2015_2016 AS g

LEFT JOIN staging.demographics_clean AS d
    ON '2015-2016-' || g.seqn::bigint::text
       = d.participant_key


UNION ALL


SELECT
    '2017-2020',
    seqn::bigint,
    '2017-2020-' || seqn::bigint::text,
    lbxgh::double precision,
    d.mec_weight

FROM raw.ghb_2017_2020 AS g

LEFT JOIN staging.demographics_clean AS d
    ON '2017-2020-' || g.seqn::bigint::text
       = d.participant_key


UNION ALL


SELECT
    '2021-2023',
    g.seqn::bigint,
    '2021-2023-' || g.seqn::bigint::text,
    g.lbxgh::double precision,
    g.wtph2yr::double precision

FROM raw.ghb_2021_2023 AS g;

-- ============================================================
-- 9. VALIDATE STAGING COMPONENT TABLES
-- ============================================================

SELECT 'insurance' AS table_name, COUNT(*) AS n
FROM staging.insurance

UNION ALL

SELECT 'usual_care', COUNT(*)
FROM staging.usual_care

UNION ALL

SELECT 'diabetes', COUNT(*)
FROM staging.diabetes

UNION ALL

SELECT 'hypertension', COUNT(*)
FROM staging.hypertension

UNION ALL

SELECT 'bmi', COUNT(*)
FROM staging.bmi

UNION ALL

SELECT 'hba1c', COUNT(*)
FROM staging.hba1c;

SELECT
    'insurance' AS table_name,
    COUNT(*) - COUNT(DISTINCT participant_key) AS duplicate_keys
FROM staging.insurance

UNION ALL

SELECT
    'usual_care',
    COUNT(*) - COUNT(DISTINCT participant_key)
FROM staging.usual_care

UNION ALL

SELECT
    'diabetes',
    COUNT(*) - COUNT(DISTINCT participant_key)
FROM staging.diabetes

UNION ALL

SELECT
    'hypertension',
    COUNT(*) - COUNT(DISTINCT participant_key)
FROM staging.hypertension

UNION ALL

SELECT
    'bmi',
    COUNT(*) - COUNT(DISTINCT participant_key)
FROM staging.bmi

UNION ALL

SELECT
    'hba1c',
    COUNT(*) - COUNT(DISTINCT participant_key)
FROM staging.hba1c;

SELECT *
FROM staging.bmi
LIMIT 20;

SELECT *
FROM staging.hba1c
LIMIT 20;