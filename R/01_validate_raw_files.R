
# 01_validate_raw_files.R
# Purpose:
#   Validate raw NHANES XPT files before database ingestion.
#   Checks:
#     1. Expected number of files
#     2. Expected variables
#     3. Row/column counts
#     4. SEQN uniqueness




# 1. Load packages

library(haven)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)



# 2. Define folders

raw_root <- file.path("data", "raw")
output_root <- file.path("outputs")

dir.create(
  output_root,
  recursive = TRUE,
  showWarnings = FALSE
)



# 3. Find all XPT files

xpt_files <- list.files(
  path = raw_root,
  pattern = "\\.xpt$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

cat("Total XPT files found:", length(xpt_files), "\n\n")

if (length(xpt_files) != 28) {
  warning(
    "Expected 28 XPT files, but found ",
    length(xpt_files),
    ". Check the raw-data folders."
  )
}


# 4. Define expected variables

expected_by_component <- list(
  
  DEMO = c(
    "SEQN",
    "RIDAGEYR",
    "RIAGENDR",
    "RIDRETH3",
    "DMDEDUC2",
    "INDFMPIR",
    "RIDEXPRG",
    "SDMVSTRA",
    "SDMVPSU"
  ),
  
  BMX = c(
    "SEQN",
    "BMXBMI"
  ),
  
  HIQ = c(
    "SEQN",
    "HIQ011"
  ),
  
  HUQ = c(
    "SEQN",
    "HUQ030"
  ),
  
  DIQ = c(
    "SEQN",
    "DIQ010"
  ),
  
  BPQ = c(
    "SEQN",
    "BPQ020"
  ),
  
  GHB = c(
    "SEQN",
    "LBXGH"
  )
)


# 5. Function identifying cycle

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


# 6. Function identifying component

get_component <- function(path) {
  
  file_stem <- tools::file_path_sans_ext(
    basename(path)
  )
  
  file_stem %>%
    str_remove("^P_") %>%
    str_remove("_[HIL]$")
}


# 7. Inspect each individual file

inspect_file <- function(path) {
  
  cycle <- get_cycle(path)
  component <- get_component(path)
  
  # Stop immediately if the survey cycle cannot be identified
  if (is.na(cycle)) {
    stop(
      "Could not identify survey cycle from path: ",
      path
    )
  }
  
  # Stop immediately if the file component cannot be identified
  if (is.null(expected_by_component[[component]])) {
    stop(
      "Could not identify NHANES component from file: ",
      basename(path)
    )
  }
  
  # Read the XPT file
  data <- read_xpt(path)
  
  # Start with the variables expected for this component
  expected_vars <- expected_by_component[[component]]
  
  # Add cycle-specific survey weights for DEMO files
  if (component == "DEMO") {
    
    if (cycle == "2017-2020") {
      
      expected_vars <- c(
        expected_vars,
        "WTINTPRP",
        "WTMECPRP"
      )
      
    } else {
      
      expected_vars <- c(
        expected_vars,
        "WTINT2YR",
        "WTMEC2YR"
      )
    }
  }
  
  # 2021-2023 HbA1c requires phlebotomy weight
  if (
    component == "GHB" &&
    cycle == "2021-2023"
  ) {
    
    expected_vars <- c(
      expected_vars,
      "WTPH2YR"
    )
  }
  
  # Identify any expected variables that are missing
  missing_vars <- setdiff(
    expected_vars,
    names(data)
  )
  
  # Check whether SEQN is duplicated within the file
  duplicate_seqn <- if ("SEQN" %in% names(data)) {
    
    sum(duplicated(data$SEQN))
    
  } else {
    
    NA_integer_
  }
  
  # Return one summary row for this file
  tibble(
    cycle = cycle,
    file = basename(path),
    component = component,
    n_rows = nrow(data),
    n_columns = ncol(data),
    duplicate_seqn = duplicate_seqn,
    expected_variables = length(expected_vars),
    missing_expected_variables =
      paste(missing_vars, collapse = "; "),
    all_expected_variables_present =
      length(missing_vars) == 0
  )
}


# 8. Run validation on all files

inventory <- map_dfr(
  xpt_files,
  inspect_file
)

# 9. Count files by survey period

cycle_counts <- inventory %>%
  count(cycle, name = "files_found") %>%
  arrange(cycle)


# 10. Print results

cat("\nFILES BY SURVEY PERIOD\n")
print(cycle_counts)

cat("\nFILE VALIDATION RESULTS\n")
print(
  inventory %>%
    select(
      cycle,
      file,
      n_rows,
      duplicate_seqn,
      all_expected_variables_present,
      missing_expected_variables
    )
)

# 11. Save validation results

write_csv(
  inventory,
  file.path(
    output_root,
    "raw_file_inventory.csv"
  )
)

write_csv(
  cycle_counts,
  file.path(
    output_root,
    "raw_file_counts_by_cycle.csv"
  )
)

# 12. Final validation summary

cat("\n-----------------------------\n")
cat("VALIDATION SUMMARY\n")
cat("-----------------------------\n")

cat(
  "Total files:",
  nrow(inventory),
  "\n"
)

cat(
  "Files missing expected variables:",
  sum(!inventory$all_expected_variables_present),
  "\n"
)

cat(
  "Total duplicate SEQN values:",
  sum(inventory$duplicate_seqn, na.rm = TRUE),
  "\n"
)

tibble(
  path = xpt_files,
  detected_cycle = map_chr(xpt_files, get_cycle)
)

sum(is.na(map_chr(xpt_files, get_cycle)))
