-- ============================================================
-- 01_create_database_structure.sql
-- Project: NHANES Health Disparities
-- Purpose: Create database schemas for the analytics pipeline
-- ============================================================

CREATE SCHEMA IF NOT EXISTS raw;
CREATE SCHEMA IF NOT EXISTS staging;
CREATE SCHEMA IF NOT EXISTS analytics;

-- Verify schema creation
SELECT schema_name
FROM information_schema.schemata
WHERE schema_name IN ('raw', 'staging', 'analytics')
ORDER BY schema_name;