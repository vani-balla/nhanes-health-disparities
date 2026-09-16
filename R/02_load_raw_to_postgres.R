
# 02_load_raw_to_postgres.R
# Purpose:
#   Load validated NHANES XPT files into PostgreSQL.
#
# Pipeline stage:
#   Raw XPT files -> PostgreSQL raw schema
#
# Notes:
#   - No analytical cleaning or recoding occurs here.
#   - Raw numeric values are preserved.
#   - Column names are converted to lowercase only to make
#     PostgreSQL querying easier.


# 1. Load packages

library(haven)
library(DBI)
library(RPostgres)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)
library(rstudioapi)


# 2. Define project folders

raw_root <- file.path("data", "raw")
output_root <- "outputs"

dir.create(
  output_root,
  recursive = TRUE,
  showWarnings = FALSE
)


# 3. Check project structure

if (!dir.exists(raw_root)) {
  stop(
    "Could not find data/raw. ",
    "Make sure the RStudio project root is nhanes-health-disparities."
  )
}


# 4. Find all XPT files

xpt_files <- list.files(
  path = raw_root,
  pattern = "\\.xpt$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(xpt_files) != 28) {
  stop(
    "Expected 28 XPT files but found ",
    length(xpt_files),
    ". Run the raw-file validation script first."
  )
}

cat("Found", length(xpt_files), "XPT files.\n")


# 5. Identify NHANES survey cycle

get_cycle <- function(path) {
  
  clean_path <- str_replace_all(path, "\\\\", "/")
  
  case_when(
    str_detect(clean_path, "2013[-_]2014") ~ "2013-2014",
    str_detect(clean_path, "2015[-_]2016") ~ "2015-2016",
    str_detect(clean_path, "2017[-_]2020") ~ "2017-2020",
    str_detect(clean_path, "2021[-_]2023") ~ "2021-2023",
    TRUE ~ NA_character_
  )
}

# 6. Identify NHANES component

get_component <- function(path) {
  
  file_stem <- tools::file_path_sans_ext(
    basename(path)
  )
  
  file_stem %>%
    str_remove("^P_") %>%
    str_remove("_[HIL]$")
}


# 7. Generate PostgreSQL table name

get_table_name <- function(path) {
  
  cycle <- get_cycle(path)
  component <- get_component(path)
  
  if (is.na(cycle)) {
    stop(
      "Could not identify survey cycle for: ",
      path
    )
  }
  
  paste0(
    tolower(component),
    "_",
    str_replace_all(cycle, "-", "_")
  )
}

# 8. Preview planned table names

file_map <- tibble(
  source_path = xpt_files,
  source_file = basename(xpt_files),
  cycle = map_chr(xpt_files, get_cycle),
  component = map_chr(xpt_files, get_component),
  postgres_table = map_chr(xpt_files, get_table_name)
) %>%
  arrange(cycle, component)

print(file_map, n = Inf)


# 9. PostgreSQL connection settings

db_host <- "localhost"
db_port <- 5432
db_name <- "nhanes_health_disparities"
db_user <- "postgres"

# Password is requested securely when the script runs.
# It is NOT stored inside the script or uploaded to GitHub.
db_password <- askForPassword(
  "Enter your PostgreSQL password"
)

# 10. Connect to PostgreSQL

con <- dbConnect(
  RPostgres::Postgres(),
  host = db_host,
  port = db_port,
  dbname = db_name,
  user = db_user,
  password = db_password
)

cat("\nConnected to PostgreSQL database:",
    db_name, "\n")


# 11. Ensure raw schema exists

dbExecute(
  con,
  "CREATE SCHEMA IF NOT EXISTS raw;"
)

# 12. Function to load one XPT file

load_raw_file <- function(path) {
  
  cycle <- get_cycle(path)
  component <- get_component(path)
  table_name <- get_table_name(path)
  
  cat(
    "\nLoading:",
    basename(path),
    "-> raw.",
    table_name,
    "\n"
  )
  
  # Read original NHANES XPT file
  data <- read_xpt(path)
  
  # Remove SAS value-label metadata while preserving
  # the underlying raw numeric values.
  data <- haven::zap_labels(data)
  data <- haven::zap_formats(data)
  
  # PostgreSQL convention:
  # normalize column names to lowercase.
  #
  # Example:
  # SEQN -> seqn
  # RIDAGEYR -> ridageyr
  # BMXBMI -> bmxbmi
  names(data) <- tolower(names(data))
  
  r_rows <- nrow(data)
  r_columns <- ncol(data)
  
  # Write/replace table in raw schema.
  #
  # overwrite = TRUE makes the ingestion script safely
  # rerunnable during development.
  dbWriteTable(
    con,
    Id(
      schema = "raw",
      table = table_name
    ),
    value = data,
    overwrite = TRUE,
    row.names = FALSE
  )
  
  # Verify number of rows after PostgreSQL ingestion
  postgres_rows <- as.numeric(
    dbGetQuery(
      con,
      paste0(
        'SELECT COUNT(*) AS n ',
        'FROM raw."',
        table_name,
        '";'
      )
    )$n[[1]]
  )
  
  row_count_match <- r_rows == postgres_rows
  
  cat(
    "Rows in R:",
    r_rows,
    "| Rows in PostgreSQL:",
    postgres_rows,
    "| Match:",
    row_count_match,
    "\n"
  )
  
  tibble(
    cycle = cycle,
    component = component,
    source_file = basename(path),
    postgres_schema = "raw",
    postgres_table = table_name,
    r_rows = r_rows,
    postgres_rows = postgres_rows,
    row_count_match = row_count_match,
    n_columns = r_columns
  )
}

# 13. Load all 28 files

load_validation <- map_dfr(
  xpt_files,
  load_raw_file
)


# 14. Print ingestion results

cat("\n====================================\n")
cat("POSTGRESQL RAW LOAD RESULTS\n")
cat("====================================\n")

print(
  load_validation %>%
    arrange(cycle, component),
  n = Inf
)

# 15. Verify PostgreSQL table count

raw_table_count <- as.numeric(
  dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n_tables
    FROM information_schema.tables
    WHERE table_schema = 'raw'
      AND table_type = 'BASE TABLE';
    "
  )$n_tables[[1]]
)

cat(
  "\nRaw PostgreSQL tables found:",
  raw_table_count,
  "\n"
)

# 16. Final validation checks

failed_row_checks <- sum(
  !load_validation$row_count_match
)

cat("\n====================================\n")
cat("RAW DATABASE INGESTION SUMMARY\n")
cat("====================================\n")

cat(
  "Source XPT files:",
  length(xpt_files),
  "\n"
)

cat(
  "Raw PostgreSQL tables:",
  raw_table_count,
  "\n"
)

cat(
  "Row-count mismatches:",
  failed_row_checks,
  "\n"
)

# 17. Save ingestion QA results

write_csv(
  file_map,
  file.path(
    output_root,
    "postgres_raw_table_map.csv"
  )
)

write_csv(
  load_validation,
  file.path(
    output_root,
    "postgres_raw_load_validation.csv"
  )
)


# 18. Disconnect from PostgreSQL

dbDisconnect(con)

cat("\nPostgreSQL connection closed.\n")