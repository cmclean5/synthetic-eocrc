# Build calibration tables for simulated cohort outputs.
#
# These functions are designed to work with simulation output objects returned by:
# - simulate_cohort_from_spec()
# - simulate_cohort_short_from_spec()
# - simulate_cohort_long_from_spec()
#
# The goal is to provide structured tabular summaries that can be used for:
# - calibration checks
# - regression testing
# - comparison between old and refactored simulator outputs
# - export to CSV for inspection in spreadsheets or reports
#
# Main user-facing functions:
# - calibration_table_overall_summary()
# - calibration_table_alcohol_prevalence_by_year_ses()
# - calibration_table_obesity_prevalence_by_year_sex_ses()
# - calibration_table_case_counts_by_diagnosis_year()
# - calibration_table_stage_distribution_among_cases()
# - calibration_table_baseline_distributions()
# - build_calibration_tables()
#
# Design choices:
# - base R is used for data manipulation
# - functions return plain data frames
# - wrapper function returns a named list of tables
# - optional CSV export is built in
# - overall summaries now include person-time incidence-rate measures for
#   calibration against observed rates
#
# Run:
# source("R/calibration_tables.R")
# tables <- build_calibration_tables(
#   sim_out = out,
#   output_dir = "outputs/calibration_tables"
# )

# Check whether an object is a single non-empty string.
.sim_cal_is_scalar_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

# Check that the simulation output object has the expected structure.
#
# Expected elements are:
# - patient
# - long
.sim_cal_check_sim_out <- function(sim_out) {
  if (!is.list(sim_out)) {
    stop("`sim_out` must be a list.", call. = FALSE)
  }
  
  if (!all(c("patient", "long") %in% names(sim_out))) {
    stop(
      "`sim_out` must contain elements named `patient` and `long`.",
      call. = FALSE
    )
  }
  
  if (!is.data.frame(sim_out$patient)) {
    stop("`sim_out$patient` must be a data frame.", call. = FALSE)
  }
  
  if (!is.data.frame(sim_out$long)) {
    stop("`sim_out$long` must be a data frame.", call. = FALSE)
  }
  
  invisible(TRUE)
}

# Safely compute the mean of a numeric vector.
#
# If there are no non-missing values, return NA_real_.
.sim_cal_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(x, na.rm = TRUE)
}

# Safely compute the sum of a numeric vector.
#
# If there are no non-missing values, return NA_real_.
.sim_cal_sum <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  sum(x, na.rm = TRUE)
}

# Safely compute the proportion of binary values coded as 0/1 or FALSE/TRUE.
#
# If there are no non-missing values, return NA_real_.
.sim_cal_prop_binary <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(as.numeric(x), na.rm = TRUE)
}

# Extract integer years from a numeric calendar-time vector.
#
# By default, floor() is used so values such as 2018.7 are assigned to 2018.
.sim_cal_extract_year <- function(x, year_fun = floor) {
  if (!is.numeric(x)) {
    return(rep(NA_integer_, length(x)))
  }
  
  as.integer(year_fun(x))
}

# Detect the event variable to use in the patient table.
#
# Resolution order:
# 1. explicit event_var argument if supplied
# 2. generic "event" column if present
# 3. disease-specific event column named by a unique event_name value
#
# If no event variable can be detected, return NULL.
.sim_cal_detect_event_var <- function(patient_df, event_var = NULL) {
  if (!is.null(event_var)) {
    if (!(event_var %in% names(patient_df))) {
      stop("Requested event_var '", event_var, "' not found in patient_df.", call. = FALSE)
    }
    
    return(event_var)
  }
  
  if ("event" %in% names(patient_df)) {
    return("event")
  }
  
  if ("event_name" %in% names(patient_df)) {
    event_names <- unique(patient_df$event_name)
    event_names <- event_names[!is.na(event_names)]
    
    if (length(event_names) == 1 && event_names %in% names(patient_df)) {
      return(event_names)
    }
  }
  
  NULL
}

# Build visit-level years from either:
# - cal_time_raw
# - cal_time
#
# If neither exists, return all-NA years.
.sim_cal_get_long_years <- function(long_df, year_fun = floor) {
  if ("cal_time_raw" %in% names(long_df)) {
    return(.sim_cal_extract_year(long_df$cal_time_raw, year_fun = year_fun))
  }
  
  if ("cal_time" %in% names(long_df)) {
    return(.sim_cal_extract_year(long_df$cal_time, year_fun = year_fun))
  }
  
  rep(NA_integer_, nrow(long_df))
}

# Build diagnosis years from either:
# - cal_time_at_diagnosis_raw
# - cal_time_at_diagnosis
#
# If neither exists, return all-NA years.
.sim_cal_get_diagnosis_years <- function(patient_df, year_fun = floor) {
  if ("cal_time_at_diagnosis_raw" %in% names(patient_df)) {
    return(.sim_cal_extract_year(patient_df$cal_time_at_diagnosis_raw, year_fun = year_fun))
  }
  
  if ("cal_time_at_diagnosis" %in% names(patient_df)) {
    return(.sim_cal_extract_year(patient_df$cal_time_at_diagnosis, year_fun = year_fun))
  }
  
  rep(NA_integer_, nrow(patient_df))
}

# Build follow-up durations in years from the patient table.
#
# Resolution order:
# 1. raw age-scale follow-up
# 2. rounded age-scale follow-up
# 3. raw calendar-time follow-up
# 4. rounded calendar-time follow-up
#
# This helper is used for descriptive follow-up summaries such as mean and total
# follow-up years.
.sim_cal_get_followup_years <- function(patient_df) {
  if (all(c("entry_age_raw", "censor_age_raw") %in% names(patient_df))) {
    return(patient_df$censor_age_raw - patient_df$entry_age_raw)
  }
  
  if (all(c("entry_age", "censor_age") %in% names(patient_df))) {
    return(patient_df$censor_age - patient_df$entry_age)
  }
  
  if (all(c("entry_cal_time_raw", "censor_cal_time_raw") %in% names(patient_df))) {
    return(patient_df$censor_cal_time_raw - patient_df$entry_cal_time_raw)
  }
  
  if (all(c("entry_cal_time", "censor_cal_time") %in% names(patient_df))) {
    return(patient_df$censor_cal_time - patient_df$entry_cal_time)
  }
  
  rep(NA_real_, nrow(patient_df))
}

# Build person-time contributions in years from the patient table.
#
# Resolution order:
# 1. raw calendar-time follow-up
# 2. rounded calendar-time follow-up
# 3. fallback to generic follow-up duration
#
# This helper is used specifically when calculating incidence rates per 100,000
# person-years.
.sim_cal_get_person_time_years <- function(patient_df) {
  if (all(c("entry_cal_time_raw", "censor_cal_time_raw") %in% names(patient_df))) {
    return(patient_df$censor_cal_time_raw - patient_df$entry_cal_time_raw)
  }
  
  if (all(c("entry_cal_time", "censor_cal_time") %in% names(patient_df))) {
    return(patient_df$censor_cal_time - patient_df$entry_cal_time)
  }
  
  .sim_cal_get_followup_years(patient_df)
}

# Compute cohort-level rate summaries from the patient table and event vector.
#
# Returned fields include:
# - n_people
# - n_cases_total
# - total_py
# - cum_risk_per_100k
# - rate_per_100k_py
# - se_rate_per_100k_py
# - cl_rate_per_100k_py
# - cu_rate_per_100k_py
.sim_cal_rate_summary <- function(patient_df,
                                  event_values) {
  n_people <- nrow(patient_df)
  
  n_cases_total <- if (length(event_values) == 0 || all(is.na(event_values))) {
    NA_real_
  } else {
    sum(as.numeric(event_values), na.rm = TRUE)
  }
  
  total_py <- .sim_cal_sum(.sim_cal_get_person_time_years(patient_df))
  
  cum_risk_per_100k <- if (is.finite(n_cases_total) && n_people > 0) {
    1e5 * n_cases_total / n_people
  } else {
    NA_real_
  }
  
  rate_per_100k_py <- if (is.finite(n_cases_total) &&
                          is.finite(total_py) &&
                          total_py > 0) {
    1e5 * n_cases_total / total_py
  } else {
    NA_real_
  }
  
  se_rate_per_100k_py <- if (is.finite(n_cases_total) &&
                             is.finite(total_py) &&
                             total_py > 0) {
    1e5 * sqrt(n_cases_total) / total_py
  } else {
    NA_real_
  }
  
  cl_rate_per_100k_py <- if (is.finite(rate_per_100k_py) &&
                             is.finite(se_rate_per_100k_py)) {
    pmax(rate_per_100k_py - (1.96 * se_rate_per_100k_py), 0)
  } else {
    NA_real_
  }
  
  cu_rate_per_100k_py <- if (is.finite(rate_per_100k_py) &&
                             is.finite(se_rate_per_100k_py)) {
    rate_per_100k_py + (1.96 * se_rate_per_100k_py)
  } else {
    NA_real_
  }
  
  list(
    n_people = n_people,
    n_cases_total = n_cases_total,
    total_py = total_py,
    cum_risk_per_100k = cum_risk_per_100k,
    rate_per_100k_py = rate_per_100k_py,
    se_rate_per_100k_py = se_rate_per_100k_py,
    cl_rate_per_100k_py = cl_rate_per_100k_py,
    cu_rate_per_100k_py = cu_rate_per_100k_py
  )
}

# Save a calibration table to CSV if requested.
#
# The output directory is created if needed.
.sim_cal_save_table_if_requested <- function(table_df,
                                             filename = NULL,
                                             output_dir = NULL) {
  if (is.null(output_dir) || is.null(filename)) {
    return(invisible(NULL))
  }
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  utils::write.csv(
    table_df,
    file = file.path(output_dir, filename),
    row.names = FALSE,
    na = ""
  )
  
  invisible(NULL)
}

# Build a one-row overall cohort summary table.
#
# This is useful as a compact top-level calibration table.
#
# Returned columns include:
# - disease
# - event_name
# - memory_model
# - n_patients
# - n_visits
# - n_cases
# - n_deaths
# - case_rate
# - death_rate
# - mean entry age
# - mean censor age
# - mean follow-up
# - total follow-up
# - total person-years
# - cumulative risk per 100,000
# - rate per 100,000 person-years
# - approximate confidence interval for the rate
# - stage completeness among cases
calibration_table_overall_summary <- function(sim_out,
                                              event_var = NULL) {
  .sim_cal_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  long_df <- sim_out$long
  
  detected_event_var <- .sim_cal_detect_event_var(patient_df, event_var = event_var)
  
  event_values <- if (!is.null(detected_event_var)) {
    patient_df[[detected_event_var]]
  } else {
    rep(NA_real_, nrow(patient_df))
  }
  
  death_values <- if ("death" %in% names(patient_df)) {
    patient_df$death
  } else {
    rep(NA_real_, nrow(patient_df))
  }
  
  followup <- .sim_cal_get_followup_years(patient_df)
  rate_summary <- .sim_cal_rate_summary(patient_df, event_values)
  
  disease_value <- if ("disease" %in% names(patient_df)) {
    vals <- unique(patient_df$disease)
    vals <- vals[!is.na(vals)]
    if (length(vals) >= 1) vals[1] else NA_character_
  } else {
    NA_character_
  }
  
  event_name_value <- if ("event_name" %in% names(patient_df)) {
    vals <- unique(patient_df$event_name)
    vals <- vals[!is.na(vals)]
    if (length(vals) >= 1) vals[1] else NA_character_
  } else {
    NA_character_
  }
  
  memory_model_value <- if ("memory_model" %in% names(patient_df)) {
    vals <- unique(patient_df$memory_model)
    vals <- vals[!is.na(vals)]
    if (length(vals) == 1) vals[1] else NA_character_
  } else {
    NA_character_
  }
  
  stage_non_missing_among_cases <- if (!is.null(detected_event_var) &&
                                       "stage" %in% names(patient_df) &&
                                       any(patient_df[[detected_event_var]] == 1, na.rm = TRUE)) {
    mean(
      !is.na(patient_df$stage[patient_df[[detected_event_var]] == 1]),
      na.rm = TRUE
    )
  } else {
    NA_real_
  }
  
  data.frame(
    disease = as.character(disease_value),
    event_name = as.character(event_name_value),
    memory_model = as.character(memory_model_value),
    n_patients = nrow(patient_df),
    n_visits = nrow(long_df),
    n_cases = .sim_cal_sum(event_values),
    n_deaths = .sim_cal_sum(death_values),
    case_rate = .sim_cal_prop_binary(event_values),
    death_rate = .sim_cal_prop_binary(death_values),
    mean_entry_age = if ("entry_age" %in% names(patient_df)) .sim_cal_mean(patient_df$entry_age) else NA_real_,
    mean_censor_age = if ("censor_age" %in% names(patient_df)) .sim_cal_mean(patient_df$censor_age) else NA_real_,
    mean_followup_years = .sim_cal_mean(followup),
    total_followup_years = .sim_cal_sum(followup),
    total_py = rate_summary$total_py,
    cum_risk_per_100k = rate_summary$cum_risk_per_100k,
    rate_per_100k_py = rate_summary$rate_per_100k_py,
    se_rate_per_100k_py = rate_summary$se_rate_per_100k_py,
    cl_rate_per_100k_py = rate_summary$cl_rate_per_100k_py,
    cu_rate_per_100k_py = rate_summary$cu_rate_per_100k_py,
    stage_non_missing_among_cases = stage_non_missing_among_cases,
    stringsAsFactors = FALSE
  )
}

# Build a calibration table of alcohol prevalence by calendar year and SES.
#
# Returned columns:
# - year
# - ses
# - alcohol_state
# - n
# - denom
# - prevalence
#
# This table is constructed from the longitudinal visit table.
calibration_table_alcohol_prevalence_by_year_ses <- function(sim_out,
                                                             states = NULL,
                                                             year_fun = floor) {
  .sim_cal_check_sim_out(sim_out)
  
  long_df <- sim_out$long
  
  required_cols <- c("ses", "alcohol_state")
  missing_cols <- setdiff(required_cols, names(long_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Longitudinal table is missing required columns for alcohol calibration: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  df <- long_df
  df$year <- .sim_cal_get_long_years(df, year_fun = year_fun)
  
  df <- df[!is.na(df$year) & !is.na(df$ses) & !is.na(df$alcohol_state), , drop = FALSE]
  
  if (!is.null(states)) {
    df <- df[df$alcohol_state %in% states, , drop = FALSE]
  }
  
  if (nrow(df) == 0) {
    return(data.frame(
      year = integer(0),
      ses = character(0),
      alcohol_state = character(0),
      n = numeric(0),
      denom = numeric(0),
      prevalence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  count_tab <- as.data.frame(
    table(df$year, df$ses, df$alcohol_state),
    stringsAsFactors = FALSE
  )
  names(count_tab) <- c("year", "ses", "alcohol_state", "n")
  
  count_tab$year <- as.integer(as.character(count_tab$year))
  count_tab$n <- as.numeric(count_tab$n)
  
  denom_df <- stats::aggregate(
    rep(1, nrow(df)) ~ year + ses,
    data = df,
    FUN = sum
  )
  names(denom_df)[names(denom_df) == "rep(1, nrow(df))"] <- "denom"
  
  out <- merge(
    count_tab,
    denom_df,
    by = c("year", "ses"),
    all.x = TRUE,
    all.y = FALSE
  )
  
  out$prevalence <- ifelse(out$denom > 0, out$n / out$denom, NA_real_)
  
  out <- out[order(out$year, out$ses, out$alcohol_state), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Build a calibration table of obesity prevalence by year and sex / SES.
#
# Returned columns:
# - year
# - sex
# - ses
# - n
# - obesity_prevalence
#
# This table is constructed from the longitudinal visit table using the
# obese_indicator column.
calibration_table_obesity_prevalence_by_year_sex_ses <- function(sim_out,
                                                                 year_fun = floor) {
  .sim_cal_check_sim_out(sim_out)
  
  long_df <- sim_out$long
  
  required_cols <- c("sex", "ses", "obese_indicator")
  missing_cols <- setdiff(required_cols, names(long_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Longitudinal table is missing required columns for obesity calibration: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  df <- long_df
  df$year <- .sim_cal_get_long_years(df, year_fun = year_fun)
  
  df <- df[!is.na(df$year) & !is.na(df$sex) & !is.na(df$ses) & !is.na(df$obese_indicator), , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(data.frame(
      year = integer(0),
      sex = character(0),
      ses = character(0),
      n = numeric(0),
      obesity_prevalence = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  mean_df <- stats::aggregate(
    obese_indicator ~ year + sex + ses,
    data = df,
    FUN = mean
  )
  names(mean_df)[names(mean_df) == "obese_indicator"] <- "obesity_prevalence"
  
  n_df <- stats::aggregate(
    obese_indicator ~ year + sex + ses,
    data = df,
    FUN = length
  )
  names(n_df)[names(n_df) == "obese_indicator"] <- "n"
  
  out <- merge(mean_df, n_df, by = c("year", "sex", "ses"), all = TRUE)
  
  out <- out[order(out$year, out$sex, out$ses), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Build a calibration table of case counts by diagnosis year.
#
# Returned columns:
# - diagnosis_year
# - n_cases
#
# This table is constructed from the patient table.
calibration_table_case_counts_by_diagnosis_year <- function(sim_out,
                                                            event_var = NULL,
                                                            year_fun = floor) {
  .sim_cal_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  detected_event_var <- .sim_cal_detect_event_var(patient_df, event_var = event_var)
  
  if (is.null(detected_event_var)) {
    return(data.frame(
      diagnosis_year = integer(0),
      n_cases = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  diagnosis_year <- .sim_cal_get_diagnosis_years(patient_df, year_fun = year_fun)
  
  df <- data.frame(
    event = patient_df[[detected_event_var]],
    diagnosis_year = diagnosis_year,
    stringsAsFactors = FALSE
  )
  
  df <- df[df$event == 1 & !is.na(df$diagnosis_year), , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(data.frame(
      diagnosis_year = integer(0),
      n_cases = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  tab <- as.data.frame(table(df$diagnosis_year), stringsAsFactors = FALSE)
  names(tab) <- c("diagnosis_year", "n_cases")
  tab$diagnosis_year <- as.integer(as.character(tab$diagnosis_year))
  tab$n_cases <- as.numeric(tab$n_cases)
  
  tab <- tab[order(tab$diagnosis_year), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

# Build a calibration table of stage distribution among diagnosed cases.
#
# Returned columns:
# - stage
# - n_cases
# - proportion_among_staged_cases
# - proportion_among_all_cases
#
# This table is constructed from the patient table.
calibration_table_stage_distribution_among_cases <- function(sim_out,
                                                             event_var = NULL) {
  .sim_cal_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  detected_event_var <- .sim_cal_detect_event_var(patient_df, event_var = event_var)
  
  if (is.null(detected_event_var) || !("stage" %in% names(patient_df))) {
    return(data.frame(
      stage = character(0),
      n_cases = numeric(0),
      proportion_among_staged_cases = numeric(0),
      proportion_among_all_cases = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  case_idx <- patient_df[[detected_event_var]] == 1
  n_all_cases <- sum(case_idx, na.rm = TRUE)
  
  df <- patient_df[case_idx & !is.na(patient_df$stage), "stage", drop = FALSE]
  
  if (nrow(df) == 0) {
    return(data.frame(
      stage = character(0),
      n_cases = numeric(0),
      proportion_among_staged_cases = numeric(0),
      proportion_among_all_cases = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  tab <- as.data.frame(table(df$stage), stringsAsFactors = FALSE)
  names(tab) <- c("stage", "n_cases")
  tab$n_cases <- as.numeric(tab$n_cases)
  
  n_staged_cases <- sum(tab$n_cases)
  
  tab$proportion_among_staged_cases <- if (n_staged_cases > 0) {
    tab$n_cases / n_staged_cases
  } else {
    NA_real_
  }
  
  tab$proportion_among_all_cases <- if (n_all_cases > 0) {
    tab$n_cases / n_all_cases
  } else {
    NA_real_
  }
  
  tab <- tab[order(tab$stage), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

# Build a baseline distribution table for key baseline variables.
#
# Returned columns:
# - variable
# - level
# - n
# - proportion
#
# This is useful for quick calibration checks on baseline covariate generation.
calibration_table_baseline_distributions <- function(sim_out,
                                                     variables = c("sex", "education", "ses", "ethnicity", "geography")) {
  .sim_cal_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  out_list <- list()
  k <- 0L
  
  for (nm in variables) {
    if (!(nm %in% names(patient_df))) {
      next
    }
    
    x <- as.character(patient_df[[nm]])
    x[is.na(x)] <- "<NA>"
    
    tab <- table(x, useNA = "no")
    
    k <- k + 1L
    out_list[[k]] <- data.frame(
      variable = nm,
      level = names(tab),
      n = as.numeric(tab),
      proportion = as.numeric(tab) / sum(tab),
      stringsAsFactors = FALSE
    )
  }
  
  if (length(out_list) == 0) {
    return(data.frame(
      variable = character(0),
      level = character(0),
      n = numeric(0),
      proportion = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

# Build the full set of calibration tables.
#
# Returned value:
# - a named list of data frames
#
# If output_dir is supplied, each table is also saved as a CSV file.
build_calibration_tables <- function(sim_out,
                                     event_var = NULL,
                                     year_fun = floor,
                                     output_dir = NULL,
                                     file_prefix = "calibration") {
  .sim_cal_check_sim_out(sim_out)
  
  tables <- list(
    overall_summary = calibration_table_overall_summary(
      sim_out = sim_out,
      event_var = event_var
    ),
    baseline_distributions = calibration_table_baseline_distributions(
      sim_out = sim_out
    ),
    alcohol_prevalence_by_year_ses = calibration_table_alcohol_prevalence_by_year_ses(
      sim_out = sim_out,
      year_fun = year_fun
    ),
    obesity_prevalence_by_year_sex_ses = calibration_table_obesity_prevalence_by_year_sex_ses(
      sim_out = sim_out,
      year_fun = year_fun
    ),
    case_counts_by_diagnosis_year = calibration_table_case_counts_by_diagnosis_year(
      sim_out = sim_out,
      event_var = event_var,
      year_fun = year_fun
    ),
    stage_distribution_among_cases = calibration_table_stage_distribution_among_cases(
      sim_out = sim_out,
      event_var = event_var
    )
  )
  
  if (!is.null(output_dir)) {
    .sim_cal_save_table_if_requested(
      tables$overall_summary,
      filename = paste0(file_prefix, "_overall_summary.csv"),
      output_dir = output_dir
    )
    
    .sim_cal_save_table_if_requested(
      tables$baseline_distributions,
      filename = paste0(file_prefix, "_baseline_distributions.csv"),
      output_dir = output_dir
    )
    
    .sim_cal_save_table_if_requested(
      tables$alcohol_prevalence_by_year_ses,
      filename = paste0(file_prefix, "_alcohol_prevalence_by_year_ses.csv"),
      output_dir = output_dir
    )
    
    .sim_cal_save_table_if_requested(
      tables$obesity_prevalence_by_year_sex_ses,
      filename = paste0(file_prefix, "_obesity_prevalence_by_year_sex_ses.csv"),
      output_dir = output_dir
    )
    
    .sim_cal_save_table_if_requested(
      tables$case_counts_by_diagnosis_year,
      filename = paste0(file_prefix, "_case_counts_by_diagnosis_year.csv"),
      output_dir = output_dir
    )
    
    .sim_cal_save_table_if_requested(
      tables$stage_distribution_among_cases,
      filename = paste0(file_prefix, "_stage_distribution_among_cases.csv"),
      output_dir = output_dir
    )
  }
  
  tables
}
