#' @title Cohort Segmentation and Net Tuition Revenue Analysis
#' @description 
#' A comprehensive framework for analyzing student cohort progression and net tuition revenue (NTR)
#' across multiple academic programs. This script processes enrollment data, calculates retention
#' metrics, and generates cohort-based financial summaries for strategic enrollment management.
#' 
#' @author jschofield
#' @date 2025-09-07
#' @version 1.0
#' 
#' @details
#' This analysis framework integrates data from multiple sources to:
#' - Calculate term progression and cohort classifications
#' - Process net tuition revenue components (tuition, aid, fees)
#' - Generate retention analytics and risk assessments
#' - Produce standardized output files for downstream analysis
#' 
#' @note 
#' Before running this script, ensure all required data files are available in the
#' designated data directories and update the configuration variables as needed.

# Setup and Configuration -----------------------------------------------------------------------

#' Clear environment for clean execution
rm(list = ls())

#' Load required libraries
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr %>% mutate filter select left_join group_by summarise case_when rename distinct bind_rows
#' @importFrom stringr str_to_title substr
#' @importFrom tidyr pivot_longer unnest_wider
#' @importFrom janitor clean_names
#' @importFrom here here
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(janitor)
  library(data.table)
  library(lubridate)
  library(readxl)
  library(googlesheets4)
  library(ggplot2)
  library(gt)
  library(here)
})

#' Configuration Variables
#' @description Set these variables to match your data environment and file structure

# Data file configuration
DATA_DIR <- here("data")  # Base data directory
OUTPUT_DIR <- here("output")  # Output directory

# Create directories if they don't exist
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Campus A (Primary Campus) data files
CAMPUS_A_MERGED_DATA <- file.path(DATA_DIR, "campus_a_merged_nr.csv")
CAMPUS_A_XREFERENCE <- file.path(DATA_DIR, "campus_a_xreference.csv")
CAMPUS_A_COHORTS <- file.path(DATA_DIR, "campus_a_cohorts_current.csv")
ENROLLMENT_ARCHIVE <- file.path(DATA_DIR, "enrollment_archive.csv")

# Campus B (Business Campus) data files - by term
CAMPUS_B_DATA_DIR <- file.path(DATA_DIR, "campus_b_raw")
CAMPUS_B_XREFERENCE <- file.path(DATA_DIR, "campus_b_xreference.csv")

# Output file naming pattern
OUTPUT_DATE_SUFFIX <- format(Sys.Date(), "%Y%m")

# Column name mappings for different data sources
STUDENT_ID_COL <- "student_id"  # Primary student identifier
TERM_COL <- "term"              # Academic term (e.g., "2024FA")
COHORT_COL <- "cohort"          # Student cohort term

# Utility Functions -----------------------------------------------------------------------------

#' Convert Academic Term to Numeric Value
#' @description Converts academic term strings (e.g., "2024FA") to comparable numeric values
#' @param term Character vector of academic terms in format YYYYTT (e.g., "2024FA", "2025SP")
#' @return Numeric vector with comparable values (e.g., 2023SU -> 20231, 2023FA -> 20232, 2024SP -> 20233)
#' @examples
#' term_code_to_num(c("2024FA", "2025SP", "2024SU"))
#' @export
term_code_to_num <- function(term) {
  term_year <- as.integer(substr(term, 1, 4))
  term_sem <- substr(term, 5, 6)
  
  if (is.na(term_year)) {
    return(-1)
  }
  
  to_add <- switch(term_sem,
                   "SP" = -7,  # Spring (previous calendar year)
                   "FA" = 2,   # Fall
                   "SU" = 1,   # Summer
                   return(-1)  # Invalid term
  )
  
  return(term_year * 10 + to_add)
}

#' Get Next Academic Term
#' @description Calculates the next academic term in sequence
#' @param term_num Numeric term value from term_code_to_num()
#' @param include_summer Logical, whether to include summer terms in progression
#' @return Numeric value representing the next term
#' @export
get_next_term <- function(term_num, include_summer = TRUE) {
  remainder <- term_num %% 10
  
  if (remainder == 3) {  # Spring term
    if (include_summer) {
      return(term_num + 8)  # Next summer
    } else {
      return(term_num + 9)  # Next fall
    }
  } else {
    return(term_num + 1)    # Next sequential term
  }
}

#' Convert Numeric Term to String
#' @description Converts numeric term values back to string format
#' @param term Numeric term value
#' @return Character string in YYYYTT format
#' @export
term_int_to_str <- function(term) {
  year <- floor(term / 10)
  sem <- switch(term %% 10,
                "1" = "SU",  # Summer
                "2" = "FA",  # Fall
                "3" = {      # Spring (increment year)
                  year <- year + 1
                  "SP"
                },
                stop("Invalid term")
  )
  return(paste0(year, sem))
}

#' Calculate Terms Between Two Academic Terms
#' @description Calculates the number of terms between two academic periods
#' @param term_1 First term (numeric)
#' @param term_2 Second term (numeric)
#' @param include_summer Logical, whether to count summer terms
#' @return Number of terms between the two periods
#' @export
get_num_terms_between <- function(term_1, term_2, include_summer = TRUE) {
  dist <- abs(term_1 - term_2)
  
  if (include_summer) {
    tot <- (dist %/% 10) * 3
    dist <- dist %% 10
    tot <- tot + case_when(
      dist == 1 ~ 1,
      dist == 2 ~ 2,
      dist == 8 ~ 1,
      dist == 9 ~ 2,
      TRUE ~ 0
    )
  } else {
    tot <- (dist %/% 10) * 2
    dist <- dist %% 10
    tot <- tot + case_when(
      dist == 1 ~ 1,
      dist == 9 ~ 1,
      TRUE ~ 0
    )
  }
  
  return(tot)
}

#' Calculate Future Term
#' @description Calculates a term that is a specified number of terms ahead
#' @param term Starting term (numeric)
#' @param terms_ahead Number of terms to advance
#' @param include_summer Logical, whether to include summer terms
#' @return Numeric value of the future term
#' @export
get_terms_ahead <- function(term, terms_ahead, include_summer = TRUE) {
  if (include_summer) {
    years_ahead <- terms_ahead %/% 3
    for (i in seq_len(terms_ahead %% 3)) {
      term <- get_next_term(term)
    }
  } else {
    years_ahead <- terms_ahead %/% 2
    for (i in seq_len(terms_ahead %% 2)) {
      term <- get_next_term(term)
    }
  }
  return(term + years_ahead * 10)
}

#' Safe Table Function
#' @description Wrapper for base::table() to avoid conflicts
#' @param x Vector to tabulate
#' @return Table object
tab_ifany <- function(x) base::table(x)

# Campus A (Primary Campus) Data Processing ---------------------------------------------------------

#' Process Campus A Net Tuition Revenue Data
#' @description 
#' Loads and processes the primary campus enrollment and financial data, including
#' data cleaning, cohort assignment, and cross-reference matching.
#' 
#' @details
#' This section handles the complex data integration process for the primary campus,
#' including multiple join operations and alternate matching logic for unmatched records.

cat("Processing Campus A data...\n")

# Check for data file existence with informative error messages
data_files_check <- list(
  "Campus A Merged Data" = CAMPUS_A_MERGED_DATA,
  "Campus A Cross-Reference" = CAMPUS_A_XREFERENCE,
  "Campus A Cohorts" = CAMPUS_A_COHORTS,
  "Enrollment Archive" = ENROLLMENT_ARCHIVE
)

missing_files <- data_files_check[!file.exists(unlist(data_files_check))]
if (length(missing_files) > 0) {
  cat("Missing data files:\n")
  for (i in seq_along(missing_files)) {
    cat("- ", names(missing_files)[i], ": ", missing_files[[i]], "\n")
  }
  stop("Please ensure all required data files are available before running the analysis.")
}

# Load Campus A datasets
campus_a_merged_nr <- read_csv(CAMPUS_A_MERGED_DATA, show_col_types = FALSE)
campus_a_xreference <- read_csv(CAMPUS_A_XREFERENCE, show_col_types = FALSE)
cohorts_term <- read_csv(CAMPUS_A_COHORTS, show_col_types = FALSE)
enrollment_archive <- read_csv(ENROLLMENT_ARCHIVE, show_col_types = FALSE)

#' Clean and prepare Campus A data
#' @description Initial data cleaning and column standardization
campus_a_clean <- campus_a_merged_nr %>%
  distinct() %>%
  clean_names() %>%
  # Select non-numeric columns and rate columns
  select(matches("^[^0-9]*$|.*_rate$"))

# Standardize key column names using flexible matching
if ("term_key" %in% names(campus_a_clean)) {
  # Column already exists
} else if (any(grepl("term_key", names(campus_a_clean), ignore.case = TRUE))) {
  # Find and rename term_key column
  term_key_col <- names(campus_a_clean)[grepl("term_key", names(campus_a_clean), ignore.case = TRUE)][1]
  campus_a_clean <- campus_a_clean %>% rename(term_key = !!term_key_col)
}

# Similar approach for student_id
if ("student_id" %in% names(campus_a_clean)) {
  # Column already exists
} else if (any(grepl("id_stu|student_id|people_id", names(campus_a_clean), ignore.case = TRUE))) {
  student_id_col <- names(campus_a_clean)[grepl("id_stu|student_id|people_id", names(campus_a_clean), ignore.case = TRUE)][1]
  campus_a_clean <- campus_a_clean %>% rename(student_id = !!student_id_col)
}

#' Filter out excluded program types and student categories
#' @description Remove non-degree and miscellaneous student types based on business rules
campus_a_filtered <- campus_a_clean %>%
  filter(
    !is.na(student_type) | !is.na(program_active),
    if ("program_active" %in% names(.)) !program_active %in% c("UG.POST", "GRAD.UG", "SNM.MISC", "SNM.FAST") else TRUE,
    if ("enroll_source" %in% names(.)) !enroll_source %in% c("INA") else TRUE
  ) %>%
  distinct()

cat("Campus A records after filtering:", nrow(campus_a_filtered), "\n")

#' Join with historical cohorts and archive data
#' @description Merge with cohort assignments and enrollment history
campus_a_with_cohorts <- campus_a_filtered %>%
  left_join(cohorts_term, by = setNames(names(cohorts_term)[1], "student_id"))

# Process enrollment archive with flexible column handling
enrollment_archive_clean <- enrollment_archive %>%
  clean_names() %>%
  select(1:2)  # Take first two columns (typically term_key and matriculation_source)

campus_a_with_archive <- campus_a_with_cohorts %>%
  left_join(enrollment_archive_clean, by = setNames(names(enrollment_archive_clean)[1], "term_key"))

#' Create enrollment source template for matching
#' @description Standardize enrollment source categories for cross-reference matching
campus_a_with_source <- campus_a_with_archive %>%
  mutate(
    enroll_source_tmpl = case_when(
      !is.na(term) & !is.na(cohort) & term == cohort ~ "ENT",  # Entering students
      TRUE ~ case_when(
        !is.na(enroll_source) & enroll_source == "ENT" ~ "ENT",
        !is.na(enroll_source) & enroll_source %in% c("CON", "RET") ~ "CON/RET",
        TRUE ~ if_else(!is.na(enroll_source), enroll_source, "UNK")
      )
    )
  )

#' Create matching templates for cross-reference
#' @description Generate composite keys for program/degree/enrollment matching
campus_a_with_templates <- campus_a_with_source %>%
  mutate(
    pgm_dgr_enroll_tmpl = paste0(
      if_else(!is.na(student_program), student_program, "UNK"),
      if_else(!is.na(student_type), student_type, "UNK"),
      enroll_source_tmpl
    ),
    alt_pgm_dgr_enroll_tmpl = paste0(
      pgm_dgr_enroll_tmpl,
      if_else(!is.na(program_active), program_active, "")
    )
  )

#' Primary matching with cross-reference table
campus_a_xref_clean <- campus_a_xreference %>% clean_names()
match_key_col <- names(campus_a_xref_clean)[1]
region_col <- if ("region" %in% names(campus_a_xref_clean)) "region" else names(campus_a_xref_clean)[2]

matched_records <- campus_a_with_templates %>%
  left_join(campus_a_xref_clean, by = setNames(match_key_col, "pgm_dgr_enroll_tmpl")) %>%
  filter(!is.na(.data[[region_col]]))

#' Alternate matching for unmatched records
unmatched_records <- campus_a_with_templates %>%
  left_join(campus_a_xref_clean, by = setNames(match_key_col, "pgm_dgr_enroll_tmpl")) %>%
  filter(is.na(.data[[region_col]]))

# Prepare alternate cross-reference
campus_a_xref_alt <- campus_a_xref_clean %>%
  rename(alt_pgm_dgr_enroll_tmpl = !!match_key_col)

# Perform alternate matching
if (nrow(unmatched_records) > 0) {
  alt_matched <- unmatched_records %>%
    select(-all_of(names(campus_a_xref_clean)[-1])) %>%
    left_join(campus_a_xref_alt, by = "alt_pgm_dgr_enroll_tmpl") %>%
    select(-ends_with(".x")) %>%
    rename_with(~ gsub("\\.y$", "", .), ends_with(".y"))
} else {
  alt_matched <- unmatched_records[0, ]  # Empty dataframe with same structure
}

#' Combine all matched records
campus_a_final <- bind_rows(matched_records, alt_matched) %>%
  clean_names() %>%
  # Flexible renaming for student enrollment type
  {if ("student_type_2" %in% names(.)) rename(., stu_enroll_type = student_type_2) else .} %>%
  {if ("stu_enroll_type" %in% names(.)) . else rename(., stu_enroll_type = student_type)} %>%
  filter(!is.na(enrollment_type))

# Calculate term sequences
campus_a_with_terms <- campus_a_final %>%
  mutate(
    term_int = sapply(if ("term" %in% names(.)) term else rep("2024FA", nrow(.)), term_code_to_num),
    cohort_int = sapply(if ("cohort" %in% names(.)) cohort else rep("2024FA", nrow(.)), term_code_to_num)
  ) %>%
  select(-any_of(c("pgm_dgr_enroll_tmpl", "alt_pgm_dgr_enroll_tmpl")))

#' Generate Campus A summary statistics
#' @description Aggregate financial data by cohort and program dimensions
campus_a_summary <- campus_a_with_terms %>%
  group_by(
    across(any_of(c("term", "term_int", "cohort_int", "region", "enrollment_type", 
                    "attendance_type", "stu_enroll_type", "program")))
  ) %>%
  summarise(
    row_count = n(),
    total_tuition_charge = sum(if ("tuition_charge" %in% names(.)) tuition_charge else 0, na.rm = TRUE),
    total_fa_institutional = sum(if ("fa_institutional" %in% names(.)) fa_institutional else 0, na.rm = TRUE),
    total_comp_fee = sum(if ("comp_fee" %in% names(.)) comp_fee else 0, na.rm = TRUE),
    total_ntr = total_tuition_charge - total_fa_institutional + total_comp_fee,
    .groups = "drop"
  )

# Export Campus A results
write_csv(campus_a_summary, file.path(OUTPUT_DIR, paste0("campus_a_summary_", OUTPUT_DATE_SUFFIX, ".csv")))

#' Prepare Campus A retention data
campus_a_retention <- campus_a_with_terms %>%
  mutate(
    session = case_when(
      str_sub(as.character(cohort_int), -1) == "2" ~ "Fall",
      str_sub(as.character(cohort_int), -1) == "3" ~ "Spring",
      str_sub(as.character(cohort_int), -1) == "1" ~ "Summer",
      TRUE ~ "Unknown"
    )
  )

write_csv(campus_a_retention, file.path(OUTPUT_DIR, paste0("campus_a_export_", OUTPUT_DATE_SUFFIX, ".csv")))

cat("Campus A processing complete. Records processed:", nrow(campus_a_retention), "\n")

# Campus B (Business Campus) Data Processing ----------------------------------------------------

#' Process Campus B Multi-Year Data
#' @description 
#' Loads and processes Campus B data from multiple semester files, combining
#' spring and fall terms across multiple years.

cat("Processing Campus B data...\n")

# Campus B file patterns (generalized for different naming conventions)
campus_b_years <- 2018:2025
campus_b_terms <- c("spring", "fall")

#' Load Campus B data files dynamically
#' @description Searches for and loads all available Campus B data files
load_campus_b_files <- function(data_dir, years, terms) {
  files_loaded <- list()
  
  for (term in terms) {
    term_files <- list()
    for (year in years) {
      # Try multiple file naming patterns
      file_patterns <- c(
        file.path(data_dir, paste0("campus_b_", year, "_", term, ".csv")),
        file.path(data_dir, paste0("bcb_", year, "_", term, ".csv")),
        file.path(data_dir, paste0(year, "_", term, ".csv"))
      )
      
      existing_file <- file_patterns[file.exists(file_patterns)][1]
      
      if (!is.na(existing_file)) {
        cat("Loading:", basename(existing_file), "\n")
        term_files[[paste0(year, "_", term)]] <- read_csv(existing_file, show_col_types = FALSE)
      }
    }
    
    if (length(term_files) > 0) {
      files_loaded[[term]] <- bind_rows(term_files, .id = "file_source")
    }
  }
  
  return(files_loaded)
}

# Load Campus B data
if (dir.exists(CAMPUS_B_DATA_DIR)) {
  campus_b_data_list <- load_campus_b_files(CAMPUS_B_DATA_DIR, campus_b_years, campus_b_terms)
  
  if (length(campus_b_data_list) > 0) {
    # Combine all Campus B data
    campus_b_combined <- bind_rows(campus_b_data_list, .id = "term_type") %>%
      distinct() %>%
      clean_names()
    
    cat("Campus B records loaded:", nrow(campus_b_combined), "\n")
    
    # Export raw combined data
    write_csv(campus_b_combined, file.path(OUTPUT_DIR, paste0("campus_b_ntr_data_", OUTPUT_DATE_SUFFIX, ".csv")))
    
    # Clean Campus B data by removing financial detail columns
    financial_detail_cols <- c("spring", "entering_student", "applied_lesson_number", "applied_lesson_charge",
                              "lens_course", "resident_commuter", "food_plan", "meal_charge", "dorm_plan",
                              "housing_charge", "health_charge", "health_waiver", "part_time", "tuition_comp",
                              "net_revenue", "ext_fed_aid", "single_room_charge", "fall")
    
    campus_b_clean <- campus_b_combined %>%
      select(-any_of(financial_detail_cols)) %>%
      distinct()
    
    # Process Campus B with cross-reference matching (if available)
    if (file.exists(CAMPUS_B_XREFERENCE)) {
      campus_b_xreference <- read_csv(CAMPUS_B_XREFERENCE, show_col_types = FALSE) %>% clean_names()
      
      # Create matching template for Campus B
      campus_b_with_template <- campus_b_clean %>%
        mutate(
          pgm_dgr_enroll_tmpl = paste0(
            if_else(!is.na(program), program, "UNK"),
            if_else(!is.na(degree), degree, "UNK"),
            if_else(!is.na(enrollment_source), enrollment_source, "UNK")
          )
        )
      
      # Primary matching
      campus_b_matched <- campus_b_with_template %>%
        left_join(campus_b_xreference, by = "pgm_dgr_enroll_tmpl") %>%
        filter(!is.na(.data[[names(campus_b_xreference)[2]]]))
      
      # Alternate matching for unmatched records
      campus_b_unmatched <- campus_b_with_template %>%
        left_join(campus_b_xreference, by = "pgm_dgr_enroll_tmpl") %>%
        filter(is.na(.data[[names(campus_b_xreference)[2]]]))
      
      if (nrow(campus_b_unmatched) > 0) {
        campus_b_unmatched_alt <- campus_b_unmatched %>%
          mutate(
            pgm_dgr_enroll_tmpl = paste0(
              pgm_dgr_enroll_tmpl,
              if_else(!is.na(curriculum), curriculum, "")
            )
          ) %>%
          left_join(campus_b_xreference, by = "pgm_dgr_enroll_tmpl")
      } else {
        campus_b_unmatched_alt <- campus_b_unmatched[0, ]
      }
      
      # Combine matched records
      campus_b_final <- bind_rows(campus_b_matched, campus_b_unmatched_alt) %>%
        clean_names()
      
    } else {
      cat("Campus B cross-reference file not found. Proceeding without matching.\n")
      campus_b_final <- campus_b_clean
    }
    
  } else {
    cat("No Campus B data files found in:", CAMPUS_B_DATA_DIR, "\n")
    campus_b_final <- data.frame()  # Empty dataframe
  }
} else {
  cat("Campus B data directory not found:", CAMPUS_B_DATA_DIR, "\n")
  campus_b_final <- data.frame()  # Empty dataframe
}

# Integrated Retention Analysis -----------------------------------------------------------------

#' Create Unified Retention Dataset
#' @description 
#' Combines Campus A and Campus B data into a standardized format for
#' comprehensive retention analysis across both campuses.

cat("Creating integrated retention dataset...\n")

#' Standardize Campus A data for retention analysis
if (exists("campus_a_retention") && nrow(campus_a_retention) > 0) {
  campus_a_retention_std <- campus_a_retention %>%
    mutate(
      school = "Campus_A",
      academic_year = as.character(substr(if ("term" %in% names(.)) term else "2024", 1, 4)),
      term_session = case_when(
        str_sub(as.character(term_int), -1) == "2" ~ "Fall",
        str_sub(as.character(term_int), -1) == "3" ~ "Spring",
        str_sub(as.character(term_int), -1) == "1" ~ "Summer",
        TRUE ~ "Unknown"
      ),
      cohort_year = as.character(substr(if ("cohort" %in% names(.)) cohort else "2024", 1, 4))
    ) %>%
    rename(
      student_id = any_of(c("student_id", "id_stu", "people_id")),
      enrollment_status = any_of(c("enrollment_status", "stat_cur_term_active_code")),
      credits = any_of(c("credits", "billing_credits")),
      institutional_grant = any_of(c("institutional_grant", "fa_institutional")),
      program_type = any_of(c("program_type", "program")),
      cohort_session = any_of(c("cohort_session", "session")),
      degree = any_of(c("degree", "program_active"))
    )
} else {
  campus_a_retention_std <- data.frame()
}

#' Standardize Campus B data for retention analysis
if (exists("campus_b_final") && nrow(campus_b_final) > 0) {
  # Flexible column mapping for Campus B
  campus_b_retention_std <- campus_b_final %>%
    mutate(
      school = "Campus_B",
      # Create standardized columns with fallbacks
      term_code = if ("term_code" %in% names(.)) term_code else 
        if ("academic_year" %in% names(.) && "academic_term" %in% names(.)) 
          paste0(academic_year, str_sub(academic_term, 1, 2)) else "2024FA",
      term_int = sapply(term_code, term_code_to_num),
      cohort_int = if ("cohort" %in% names(.)) sapply(cohort, term_code_to_num) else term_int,
      session = case_when(
        str_sub(as.character(cohort_int), -1) == "2" ~ "Fall",
        str_sub(as.character(cohort_int), -1) == "3" ~ "Spring", 
        str_sub(as.character(cohort_int), -1) == "1" ~ "Summer",
        TRUE ~ "Fall"  # Default fallback
      ),
      term_key = paste0(
        if ("people_id" %in% names(.)) people_id else 
          if ("student_id" %in% names(.)) student_id else row_number(), 
        "*", term_code
      ),
      enrollment_status = if ("enrollment_status" %in% names(.)) 
        str_sub(enrollment_status, 1, 1) else "E",
      academic_year = as.character(if ("academic_year" %in% names(.)) academic_year else 
        substr(term_code, 1, 4))
    ) %>%
    # Handle NA values in financial columns
    mutate(
      across(any_of(c("scholarships", "tuition_charge", "comp_charge")), 
             ~ if_else(is.na(.) | . == 0, 0, as.numeric(.)))
    ) %>%
    # Standardize column names
    rename(
      student_id = any_of(c("people_id", "student_id")),
      term = any_of(c("term_code", "term")),
      region = any_of(c("region_xref", "region")),
      enrollment_type = any_of(c("enroll_type_xref", "enrollment_type")),
      attendance_type = any_of(c("attend_type_xref", "attendance_type")),
      stu_enroll_type = any_of(c("student_type_xref", "stu_enroll_type")),
      program_type = any_of(c("program_xref", "program_type")),
      cohort_session = any_of(c("admit_term", "cohort_session")),
      institutional_grant = any_of(c("scholarships", "institutional_grant")),
      comp_fee = any_of(c("comp_charge", "comp_fee")),
      matric_source = any_of(c("enrollment_source", "matric_source")),
      student_program = any_of(c("program", "student_program")),
      type = any_of(c("curriculum", "type")),
      cohort_year = any_of(c("admit_year", "cohort_year"))
    ) %>%
    # Handle non-degree region classification
    mutate(
      region = case_when(
        (is.na(region) | region == 'NA') & 
          !is.na(degree) & degree == "NONDEG" ~ "Non-Degree",
        TRUE ~ region
      )
    ) %>%
    select(-any_of(c("academic_term", "pgm_dgr_enroll_tmpl", "session")))
} else {
  campus_b_retention_std <- data.frame()
}

#' Combine Campus A and Campus B data
if (nrow(campus_a_retention_std) > 0 && nrow(campus_b_retention_std) > 0) {
  retention_fact <- bind_rows(campus_a_retention_std, campus_b_retention_std)
} else if (nrow(campus_a_retention_std) > 0) {
  retention_fact <- campus_a_retention_std
} else if (nrow(campus_b_retention_std) > 0) {
  retention_fact <- campus_b_retention_std
} else {
  retention_fact <- data.frame()
  cat("Warning: No data available for retention analysis.\n")
}

#' Final data cleaning and standardization
if (nrow(retention_fact) > 0) {
  retention_fact_clean <- retention_fact %>%
    # Remove matriculation source (often contains PII)
    select(-any_of("matric_source")) %>%
    # Standardize financial columns
    mutate(
      across(any_of(c("institutional_grant", "comp_fee", "tuition_charge")), 
             ~ case_when(
               is.na(.) | . == "NA" ~ 0L,
               TRUE ~ as.integer(.)
             )),
      # Handle missing cohort information
      cohort = case_when(
        (is.na(cohort) | cohort == "NA") & 
          !is.na(stu_enroll_type) & stu_enroll_type == "Entering" ~ term,
        TRUE ~ cohort
      ),
      # Standardize program names
      student_program = case_when(
        student_program == 'GRAD' ~ 'Graduate',
        student_program == 'UNDG' ~ 'Undergraduate', 
        student_program == 'EXSU' ~ 'Non-Degree',
        TRUE ~ student_program
      )
    ) %>%
    # Rename final columns
    rename(
      cohort_term = any_of(c("cohort", "cohort_term")),
      term_code = any_of(c("term", "term_code"))
    ) %>%
    # Filter to enrolled students only
    filter(enrollment_status == 'E')
  
  cat("Integrated retention dataset created with", nrow(retention_fact_clean), "records.\n")
} else {
  retention_fact_clean <- data.frame()
}

# Data Quality and Export ------------------------------------------------------------------------

#' Create Data Mapping Function for Quality Analysis
#' @description Generates data profiling information for quality assessment
#' @param df Data frame to analyze
#' @return Data frame with variable statistics
map_it <- function(df) {
  if (nrow(df) == 0) {
    return(data.frame(
      variable = character(0),
      class = character(0),
      unique_count = numeric(0),
      na_count = numeric(0)
    ))
  }
  
  df %>%
    summarise(across(everything(), ~ list(
      class = class(.x)[1], 
      unique_count = n_distinct(.x, na.rm = TRUE),
      na_count = sum(is.na(.x))
    ))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "info") %>%
    unnest_wider(info)
}

# Generate data quality reports
if (exists("retention_fact_clean") && nrow(retention_fact_clean) > 0) {
  map_it_retention_fact <- map_it(retention_fact_clean)
  write_csv(map_it_retention_fact, file.path(OUTPUT_DIR, "data_quality_retention_fact.csv"))
}

if (exists("campus_a_retention_std") && nrow(campus_a_retention_std) > 0) {
  map_it_campus_a <- map_it(campus_a_retention_std)
  write_csv(map_it_campus_a, file.path(OUTPUT_DIR, "data_quality_campus_a.csv"))
}

if (exists("campus_b_retention_std") && nrow(campus_b_retention_std) > 0) {
  map_it_campus_b <- map_it(campus_b_retention_std)
  write_csv(map_it_campus_b, file.path(OUTPUT_DIR, "data_quality_campus_b.csv"))
}

# Identify records with missing critical information
if (exists("retention_fact_clean") && nrow(retention_fact_clean) > 0) {
  retention_fact_na <- retention_fact_clean %>%
    filter(
      is.na(cohort_session) |
        is.na(cohort_int) |
        is.na(cohort_term) |
        is.na(attendance_type) |
        is.na(stu_enroll_type) |
        is.na(program_type)
    )
  
  # Export final datasets
  write_csv(retention_fact_clean, file.path(OUTPUT_DIR, "retention_fact_final.csv"))
  write_csv(retention_fact_na, file.path(OUTPUT_DIR, "retention_fact_missing_data.csv"))
  
  cat("Analysis complete!\n")
  cat("Final retention dataset:", nrow(retention_fact_clean), "records\n")
  cat("Records with missing critical data:", nrow(retention_fact_na), "records\n")
  cat("Data quality rate:", round((1 - nrow(retention_fact_na)/nrow(retention_fact_clean)) * 100, 1), "%\n")
} else {
  cat("Analysis complete, but no final dataset was generated.\n")
  cat("Please check data file availability and configuration.\n")
}

cat("All output files saved to:", OUTPUT_DIR, "\n")
