# Output builders: summaries, calibration tables, and diagnostic plots.
#
# This script consolidates output-related utilities for simulated cohort
# outputs returned by:
# - simulate_person(), simulate_person_short(), simulate_person_long()
# - simulate_cohort(), simulate_cohort_short(), simulate_cohort_long()
#
# The expected simulation output object is a list with elements:
# - patient: data frame with one row per person
# - long:    data frame with one row per visit
#
# Main user-facing functions:
#
# Summaries:
# - summarise_patient_table()
# - summarise_longitudinal_table()
# - summarise_simulation_outputs()
#
# Calibration tables:
# - calibration_table_overall_summary()
# - calibration_table_baseline_distributions()
# - calibration_table_alcohol_prevalence_by_year_ses()
# - calibration_table_obesity_prevalence_by_year_sex_ses()
# - calibration_table_case_counts_by_diagnosis_year()
# - calibration_table_stage_distribution_among_cases()
# - build_calibration_tables()
#
# Diagnostic plots (ggplot2):
# - plot_alcohol_prevalence_by_year_ses()
# - plot_obesity_prevalence_by_year_sex_ses()
# - plot_case_counts_by_diagnosis_year()
# - plot_stage_distribution_among_cases()
# - plot_simulation_diagnostics()
#
# Expected shared package utilities:
# - from utils_math.R:
#   - is_scalar_number()
#   - is_scalar_string()
#   - list_string_or_default()
#   - list_numeric_or_default()

# Check basic structure of a simulation output object.
.output_check_sim_out <- function(sim_out) {
  if (!is.list(sim_out)) {
    stop("`sim_out` must be a list.", call. = FALSE)
  }
  
  if (!all(c("patient", "long") %in% names(sim_out))) {
    stop("`sim_out` must contain elements named `patient` and `long`.", call. = FALSE)
  }
  
  if (!is.data.frame(sim_out$patient)) {
    stop("`sim_out$patient` must be a data frame.", call. = FALSE)
  }
  
  if (!is.data.frame(sim_out$long)) {
    stop("`sim_out$long` must be a data frame.", call. = FALSE)
  }
  
  invisible(TRUE)
}

# Extract integer years from a numeric calendar-time vector.
#
# By default, floor() is used so values such as 2018.7 are assigned to 2018.
.output_extract_year <- function(x, year_fun = floor) {
  if (!is.numeric(x)) {
    return(rep(NA_integer_, length(x)))
  }
  
  as.integer(year_fun(x))
}

# Build visit-level years from either:
# - cal_time_raw
# - cal_time
#
# If neither exists, return all-NA years.
.output_get_long_years <- function(long_df, year_fun = floor) {
  if ("cal_time_raw" %in% names(long_df)) {
    return(.output_extract_year(long_df$cal_time_raw, year_fun = year_fun))
  }
  
  if ("cal_time" %in% names(long_df)) {
    return(.output_extract_year(long_df$cal_time, year_fun = year_fun))
  }
  
  rep(NA_integer_, nrow(long_df))
}

# Build diagnosis years from either:
# - cal_time_at_diagnosis_raw
# - cal_time_at_diagnosis
#
# If neither exists, return all-NA years.
.output_get_diagnosis_years <- function(patient_df, year_fun = floor) {
  if ("cal_time_at_diagnosis_raw" %in% names(patient_df)) {
    return(.output_extract_year(patient_df$cal_time_at_diagnosis_raw, year_fun = year_fun))
  }
  
  if ("cal_time_at_diagnosis" %in% names(patient_df)) {
    return(.output_extract_year(patient_df$cal_time_at_diagnosis, year_fun = year_fun))
  }
  
  rep(NA_integer_, nrow(patient_df))
}

# Safely compute the mean of a numeric vector.
#
# If the vector has no non-missing values, return NA_real_.
.output_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(x, na.rm = TRUE)
}

# Safely compute the sum of a numeric vector.
#
# If the vector has no non-missing values, return NA_real_.
.output_sum <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  sum(x, na.rm = TRUE)
}

# Safely compute the proportion of TRUE-like values in a binary vector.
#
# This function expects vectors coded as:
# - 0 / 1
# - TRUE / FALSE
#
# If the vector has no non-missing values, return NA_real_.
.output_prop_binary <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(as.numeric(x), na.rm = TRUE)
}

# Create a frequency table for a categorical variable.
#
# Returned columns:
# - variable
# - level
# - n
# - prop
#
# Missing values are labelled as "<NA>".
.output_tabulate_categorical <- function(x, variable_name) {
  if (length(x) == 0) {
    return(data.frame(
      variable = character(0),
      level = character(0),
      n = numeric(0),
      prop = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- "<NA>"
  
  tab <- table(x_chr, useNA = "no")
  
  out <- data.frame(
    variable = variable_name,
    level = names(tab),
    n = as.numeric(tab),
    prop = as.numeric(tab) / sum(tab),
    stringsAsFactors = FALSE
  )
  
  out <- out[order(-out$n, out$level), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Count observations by year from a calendar-time vector.
#
# Returned columns:
# - year
# - n
.output_count_by_year <- function(cal_time, year_fun = floor) {
  years <- .output_extract_year(cal_time, year_fun = year_fun)
  years <- years[!is.na(years)]
  
  if (length(years) == 0) {
    return(data.frame(
      year = integer(0),
      n = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  tab <- table(years)
  
  out <- data.frame(
    year = as.integer(names(tab)),
    n = as.numeric(tab),
    stringsAsFactors = FALSE
  )
  
  out <- out[order(out$year), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Summarise the mean of a numeric value by year.
#
# Returned columns:
# - year
# - n
# - mean
.output_mean_by_year <- function(years, values) {
  ok <- !is.na(years) & !is.na(values)
  
  if (!any(ok)) {
    return(data.frame(
      year = integer(0),
      n = numeric(0),
      mean = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  years <- years[ok]
  values <- values[ok]
  
  split_vals <- split(values, years)
  
  out <- data.frame(
    year = as.integer(names(split_vals)),
    n = vapply(split_vals, length, numeric(1)),
    mean = vapply(split_vals, mean, numeric(1)),
    stringsAsFactors = FALSE
  )
  
  out <- out[order(out$year), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Summarise a categorical variable by year.
#
# Returned columns:
# - variable
# - year
# - level
# - n
# - prop
.output_categorical_by_year <- function(years, values, variable_name) {
  ok <- !is.na(years) & !is.na(values)
  
  if (!any(ok)) {
    return(data.frame(
      variable = character(0),
      year = integer(0),
      level = character(0),
      n = numeric(0),
      prop = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  df <- data.frame(
    year = years[ok],
    value = as.character(values[ok]),
    stringsAsFactors = FALSE
  )
  
  df$value[is.na(df$value)] <- "<NA>"
  
  tab <- as.data.frame(table(df$year, df$value), stringsAsFactors = FALSE)
  names(tab) <- c("year", "level", "n")
  
  tab$year <- as.integer(as.character(tab$year))
  tab$n <- as.numeric(tab$n)
  
  totals <- tapply(tab$n, tab$year, sum)
  tab$prop <- tab$n / totals[as.character(tab$year)]
  tab$variable <- variable_name
  
  tab <- tab[, c("variable", "year", "level", "n", "prop"), drop = FALSE]
  tab <- tab[order(tab$year, -tab$n, tab$level), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

# Detect the event variable to use in patient-level summaries.
#
# Resolution order:
# 1. explicit event_var argument if supplied
# 2. generic "event" column if present
# 3. disease-specific event column named by a unique event_name value
#
# If no event variable can be detected, return NULL.
.detect_event_var <- function(patient_df, event_var = NULL) {
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

#Safely compute the sum of a numeric vector.
#
# If there are no non-missing values, return NA_real_.
.sim_cal_sum <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  sum(x, na.rm = TRUE)
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


# Summarise the patient-level output table.
summarise_patient_table <- function(patient_df,
                                    event_var = NULL,
                                    year_fun = floor) {
  if (!is.data.frame(patient_df)) {
    stop("`patient_df` must be a data frame.", call. = FALSE)
  }
  
  detected_event_var <- .detect_event_var(patient_df, event_var = event_var)
  
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
  
  severe_values <- if ("severe_case" %in% names(patient_df)) {
    patient_df$severe_case
  } else {
    rep(NA_real_, nrow(patient_df))
  }
  
  followup <- if (all(c("entry_age_raw", "censor_age_raw") %in% names(patient_df))) {
    patient_df$censor_age_raw - patient_df$entry_age_raw
  } else if (all(c("entry_age", "censor_age") %in% names(patient_df))) {
    patient_df$censor_age - patient_df$entry_age
  } else {
    rep(NA_real_, nrow(patient_df))
  }
  
  overview <- data.frame(
    n_patients = nrow(patient_df),
    n_cases = .output_sum(event_values),
    n_deaths = .output_sum(death_values),
    case_rate = .output_prop_binary(event_values),
    death_rate = .output_prop_binary(death_values),
    severe_case_rate_among_cases = if (all(is.na(severe_values)) || all(is.na(event_values))) {
      NA_real_
    } else {
      mean(severe_values[event_values == 1], na.rm = TRUE)
    },
    mean_entry_age = if ("entry_age" %in% names(patient_df)) .output_mean(patient_df$entry_age) else NA_real_,
    mean_censor_age = if ("censor_age" %in% names(patient_df)) .output_mean(patient_df$censor_age) else NA_real_,
    mean_followup_years = .output_mean(followup),
    total_followup_years = .output_sum(followup),
    stringsAsFactors = FALSE
  )
  
  baseline_variables <- c("sex", "education", "ses", "ethnicity", "geography")
  baseline_distributions <- list()
  
  for (nm in baseline_variables) {
    if (nm %in% names(patient_df)) {
      baseline_distributions[[nm]] <- .output_tabulate_categorical(
        patient_df[[nm]],
        variable_name = nm
      )
    }
  }
  
  stage_distribution <- if ("stage" %in% names(patient_df)) {
    case_idx <- if (!is.null(detected_event_var)) {
      patient_df[[detected_event_var]] == 1
    } else {
      !is.na(patient_df$stage)
    }
    
    .output_tabulate_categorical(
      patient_df$stage[case_idx],
      variable_name = "stage"
    )
  } else {
    .output_tabulate_categorical(character(0), "stage")
  }
  
  diagnosis_year_summary <- if ("cal_time_at_diagnosis_raw" %in% names(patient_df)) {
    .output_count_by_year(patient_df$cal_time_at_diagnosis_raw, year_fun = year_fun)
  } else if ("cal_time_at_diagnosis" %in% names(patient_df)) {
    .output_count_by_year(patient_df$cal_time_at_diagnosis, year_fun = year_fun)
  } else {
    data.frame(year = integer(0), n = numeric(0), stringsAsFactors = FALSE)
  }
  
  death_year_summary <- if ("cal_time_at_death_raw" %in% names(patient_df)) {
    .output_count_by_year(patient_df$cal_time_at_death_raw, year_fun = year_fun)
  } else if ("cal_time_at_death" %in% names(patient_df)) {
    .output_count_by_year(patient_df$cal_time_at_death, year_fun = year_fun)
  } else {
    data.frame(year = integer(0), n = numeric(0), stringsAsFactors = FALSE)
  }
  
  list(
    event_var = detected_event_var,
    overview = overview,
    baseline_distributions = baseline_distributions,
    stage_distribution = stage_distribution,
    diagnosis_year_summary = diagnosis_year_summary,
    death_year_summary = death_year_summary
  )
}

# Summarise the longitudinal visit table.
summarise_longitudinal_table <- function(long_df,
                                         year_fun = floor) {
  if (!is.data.frame(long_df)) {
    stop("`long_df` must be a data frame.", call. = FALSE)
  }
  
  visit_years <- .output_get_long_years(long_df, year_fun = year_fun)
  
  mean_visits_per_id <- if ("id" %in% names(long_df) && nrow(long_df) > 0) {
    counts <- table(long_df$id)
    mean(as.numeric(counts))
  } else {
    NA_real_
  }
  
  overview <- data.frame(
    n_rows = nrow(long_df),
    n_ids = if ("id" %in% names(long_df)) length(unique(long_df$id)) else NA_real_,
    mean_visits_per_id = mean_visits_per_id,
    mean_age = if ("age" %in% names(long_df)) .output_mean(long_df$age) else NA_real_,
    stringsAsFactors = FALSE
  )
  
  visit_year_summary <- if ("cal_time_raw" %in% names(long_df)) {
    .output_count_by_year(long_df$cal_time_raw, year_fun = year_fun)
  } else if ("cal_time" %in% names(long_df)) {
    .output_count_by_year(long_df$cal_time, year_fun = year_fun)
  } else {
    data.frame(year = integer(0), n = numeric(0), stringsAsFactors = FALSE)
  }
  
  alcohol_state_by_year <- if ("alcohol_state" %in% names(long_df)) {
    .output_categorical_by_year(
      years = visit_years,
      values = long_df$alcohol_state,
      variable_name = "alcohol_state"
    )
  } else {
    .output_categorical_by_year(integer(0), character(0), "alcohol_state")
  }
  
  obesity_by_year <- if ("obese_indicator" %in% names(long_df)) {
    tmp <- .output_mean_by_year(
      years = visit_years,
      values = long_df$obese_indicator
    )
    names(tmp) <- c("year", "n", "mean_obese_indicator")
    tmp
  } else {
    data.frame(year = integer(0), n = numeric(0), mean_obese_indicator = numeric(0), stringsAsFactors = FALSE)
  }
  
  measurement_availability_by_year <- data.frame(
    year = integer(0),
    n = numeric(0),
    prop_visceral_observed = numeric(0),
    prop_insulin_observed = numeric(0),
    mean_visceral_observed = numeric(0),
    mean_insulin_observed = numeric(0),
    stringsAsFactors = FALSE
  )
  
  if (length(visit_years) > 0 && any(!is.na(visit_years))) {
    valid_years <- sort(unique(visit_years[!is.na(visit_years)]))
    
    rows <- lapply(valid_years, function(y) {
      idx <- visit_years == y
      
      visceral_vals <- if ("visceral_adipose" %in% names(long_df)) long_df$visceral_adipose[idx] else rep(NA_real_, sum(idx))
      insulin_vals <- if ("fasting_insulin" %in% names(long_df)) long_df$fasting_insulin[idx] else rep(NA_real_, sum(idx))
      
      data.frame(
        year = as.integer(y),
        n = sum(idx),
        prop_visceral_observed = mean(!is.na(visceral_vals)),
        prop_insulin_observed = mean(!is.na(insulin_vals)),
        mean_visceral_observed = .output_mean(visceral_vals),
        mean_insulin_observed = .output_mean(insulin_vals),
        stringsAsFactors = FALSE
      )
    })
    
    measurement_availability_by_year <- do.call(rbind, rows)
    rownames(measurement_availability_by_year) <- NULL
  }
  
  list(
    overview = overview,
    visit_year_summary = visit_year_summary,
    alcohol_state_by_year = alcohol_state_by_year,
    obesity_by_year = obesity_by_year,
    measurement_availability_by_year = measurement_availability_by_year
  )
}

# Summarise the full simulation output object.
summarise_simulation_outputs <- function(sim_out,
                                         event_var = NULL,
                                         year_fun = floor) {
  .output_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  long_df <- sim_out$long
  
  patient_summary <- summarise_patient_table(
    patient_df = patient_df,
    event_var = event_var,
    year_fun = year_fun
  )
  
  long_summary <- summarise_longitudinal_table(
    long_df = long_df,
    year_fun = year_fun
  )
  
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
  
  overview <- data.frame(
    disease = as.character(disease_value),
    event_name = as.character(event_name_value),
    memory_model = as.character(memory_model_value),
    n_patients = patient_summary$overview$n_patients,
    n_visits = long_summary$overview$n_rows,
    n_cases = patient_summary$overview$n_cases,
    n_deaths = patient_summary$overview$n_deaths,
    case_rate = patient_summary$overview$case_rate,
    death_rate = patient_summary$overview$death_rate,
    mean_followup_years = patient_summary$overview$mean_followup_years,
    mean_visits_per_id = long_summary$overview$mean_visits_per_id,
    stringsAsFactors = FALSE
  )
  
  list(
    overview = overview,
    patient = patient_summary,
    long = long_summary
  )
}

# Save a calibration table to CSV if requested.
#
# The output directory is created if needed.
.save_table_if_requested <- function(table_df,
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
calibration_table_overall_summary <- function(sim_out,
                                              event_var = NULL) {
  .output_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  long_df    <- sim_out$long
  
  detected_event_var <- .detect_event_var(patient_df, event_var = event_var)
  
  event_values <- if (!is.null(detected_event_var)) patient_df[[detected_event_var]] else rep(NA_real_, nrow(patient_df))
  death_values <- if ("death" %in% names(patient_df)) patient_df$death else rep(NA_real_, nrow(patient_df))
  
  rate_summary <- .sim_cal_rate_summary(patient_df, event_values)
  
  followup <- if (all(c("entry_age_raw", "censor_age_raw") %in% names(patient_df))) {
    patient_df$censor_age_raw - patient_df$entry_age_raw
  } else if (all(c("entry_age", "censor_age") %in% names(patient_df))) {
    patient_df$censor_age - patient_df$entry_age
  } else {
    rep(NA_real_, nrow(patient_df))
  }
  
  stage_non_missing_among_cases <- if (!is.null(detected_event_var) &&
                                       "stage" %in% names(patient_df) &&
                                       any(patient_df[[detected_event_var]] == 1, na.rm = TRUE)) {
    mean(!is.na(patient_df$stage[patient_df[[detected_event_var]] == 1]), na.rm = TRUE)
  } else {
    NA_real_
  }
  
  data.frame(
    disease = if ("disease" %in% names(patient_df)) as.character(unique(patient_df$disease)[1]) else NA_character_,
    event_name = if ("event_name" %in% names(patient_df)) as.character(unique(patient_df$event_name)[1]) else NA_character_,
    memory_model = if ("memory_model" %in% names(patient_df)) as.character(unique(patient_df$memory_model)[1]) else NA_character_,
    n_patients = nrow(patient_df),
    n_visits = nrow(long_df),
    n_cases = .output_sum(event_values),
    n_deaths = .output_sum(death_values),
    case_rate = .output_prop_binary(event_values),
    death_rate = .output_prop_binary(death_values),
    mean_entry_age = if ("entry_age" %in% names(patient_df)) .output_mean(patient_df$entry_age) else NA_real_,
    mean_censor_age = if ("censor_age" %in% names(patient_df)) .output_mean(patient_df$censor_age) else NA_real_,
    mean_followup_years = .output_mean(followup),
    total_followup_years = .output_sum(followup),
    rate_per_100k_py = rate_summary$rate_per_100k_py,
    se_rate_per_100k_py = rate_summary$se_rate_per_100k_py,
    cl_rate_per_100k_py = rate_summary$cl_rate_per_100k_py,
    cu_rate_per_100k_py = rate_summary$cu_rate_per_100k_py,
    stage_non_missing_among_cases = stage_non_missing_among_cases,
    stringsAsFactors = FALSE
  )
}

# Build a baseline distribution table for key baseline variables.
calibration_table_baseline_distributions <- function(sim_out,
                                                     variables = c("sex", "education", "ses", "ethnicity", "geography")) {
  .output_check_sim_out(sim_out)
  
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

# Build a calibration table of alcohol prevalence by calendar year and SES.
calibration_table_alcohol_prevalence_by_year_ses <- function(sim_out,
                                                             states = NULL,
                                                             year_fun = floor) {
  .output_check_sim_out(sim_out)
  
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
  df$year <- .output_get_long_years(df, year_fun = year_fun)
  
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
  
  out <- merge(count_tab, denom_df, by = c("year", "ses"), all.x = TRUE)
  out$prevalence <- ifelse(out$denom > 0, out$n / out$denom, NA_real_)
  
  out <- out[order(out$year, out$ses, out$alcohol_state), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Build a calibration table of obesity prevalence by year and sex / SES.
calibration_table_obesity_prevalence_by_year_sex_ses <- function(sim_out,
                                                                 year_fun = floor) {
  .output_check_sim_out(sim_out)
  
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
  df$year <- .output_get_long_years(df, year_fun = year_fun)
  
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
calibration_table_case_counts_by_diagnosis_year <- function(sim_out,
                                                            event_var = NULL,
                                                            year_fun = floor) {
  .output_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  detected_event_var <- .detect_event_var(patient_df, event_var = event_var)
  
  if (is.null(detected_event_var)) {
    return(data.frame(diagnosis_year = integer(0), n_cases = numeric(0), stringsAsFactors = FALSE))
  }
  
  diagnosis_year <- .output_get_diagnosis_years(patient_df, year_fun = year_fun)
  
  df <- data.frame(
    event = patient_df[[detected_event_var]],
    diagnosis_year = diagnosis_year,
    stringsAsFactors = FALSE
  )
  
  df <- df[df$event == 1 & !is.na(df$diagnosis_year), , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(data.frame(diagnosis_year = integer(0), n_cases = numeric(0), stringsAsFactors = FALSE))
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
calibration_table_stage_distribution_among_cases <- function(sim_out,
                                                             event_var = NULL) {
  .output_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  detected_event_var <- .detect_event_var(patient_df, event_var = event_var)
  
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
  
  tab$proportion_among_staged_cases <- if (n_staged_cases > 0) tab$n_cases / n_staged_cases else NA_real_
  tab$proportion_among_all_cases <- if (n_all_cases > 0) tab$n_cases / n_all_cases else NA_real_
  
  tab <- tab[order(tab$stage), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

# Build the full set of calibration tables.
#
# If output_dir is supplied, each table is also saved as CSV.
build_calibration_tables <- function(sim_out,
                                     event_var = NULL,
                                     year_fun = floor,
                                     output_dir = NULL,
                                     file_prefix = "calibration") {
  .output_check_sim_out(sim_out)
  
  tables <- list(
    overall_summary = calibration_table_overall_summary(sim_out, event_var = event_var),
    baseline_distributions = calibration_table_baseline_distributions(sim_out),
    alcohol_prevalence_by_year_ses = calibration_table_alcohol_prevalence_by_year_ses(sim_out, year_fun = year_fun),
    obesity_prevalence_by_year_sex_ses = calibration_table_obesity_prevalence_by_year_sex_ses(sim_out, year_fun = year_fun),
    case_counts_by_diagnosis_year = calibration_table_case_counts_by_diagnosis_year(sim_out, event_var = event_var, year_fun = year_fun),
    stage_distribution_among_cases = calibration_table_stage_distribution_among_cases(sim_out, event_var = event_var)
  )
  
  if (!is.null(output_dir)) {
    .save_table_if_requested(tables$overall_summary, paste0(file_prefix, "_overall_summary.csv"), output_dir)
    .save_table_if_requested(tables$baseline_distributions, paste0(file_prefix, "_baseline_distributions.csv"), output_dir)
    .save_table_if_requested(tables$alcohol_prevalence_by_year_ses, paste0(file_prefix, "_alcohol_prevalence_by_year_ses.csv"), output_dir)
    .save_table_if_requested(tables$obesity_prevalence_by_year_sex_ses, paste0(file_prefix, "_obesity_prevalence_by_year_sex_ses.csv"), output_dir)
    .save_table_if_requested(tables$case_counts_by_diagnosis_year, paste0(file_prefix, "_case_counts_by_diagnosis_year.csv"), output_dir)
    .save_table_if_requested(tables$stage_distribution_among_cases, paste0(file_prefix, "_stage_distribution_among_cases.csv"), output_dir)
  }
  
  tables
}

# Require ggplot2 for plotting functions.
.output_require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for diagnostic plots.", call. = FALSE)
  }
  
  invisible(TRUE)
}

# Return a placeholder ggplot object when there is no data available for a plot.
.output_plot_empty <- function(title, message) {
  .output_require_ggplot2()
  
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, size = 5) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}

# Save a ggplot object if an output directory has been requested.
.output_save_plot_if_requested <- function(plot_obj,
                                           filename = NULL,
                                           output_dir = NULL,
                                           width = 8,
                                           height = 5,
                                           dpi = 300) {
  if (is.null(output_dir) || is.null(filename)) {
    return(invisible(NULL))
  }
  
  .output_require_ggplot2()
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  ggplot2::ggsave(
    filename = file.path(output_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
  
  invisible(NULL)
}

# Plot alcohol prevalence by calendar year and SES.
plot_alcohol_prevalence_by_year_ses <- function(sim_out,
                                                states = NULL,
                                                year_fun = floor) {
  .output_require_ggplot2()
  .output_check_sim_out(sim_out)
  
  long_df <- sim_out$long
  
  required_cols <- c("ses", "alcohol_state")
  missing_cols <- setdiff(required_cols, names(long_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Longitudinal table is missing required columns for alcohol plotting: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  df <- long_df
  df$year <- .output_get_long_years(df, year_fun = year_fun)
  
  df <- df[!is.na(df$year) & !is.na(df$ses) & !is.na(df$alcohol_state), , drop = FALSE]
  
  if (!is.null(states)) {
    df <- df[df$alcohol_state %in% states, , drop = FALSE]
  }
  
  if (nrow(df) == 0) {
    return(.output_plot_empty(
      title = "Alcohol prevalence by calendar year and SES",
      message = "No alcohol-state data available for plotting."
    ))
  }
  
  tab <- as.data.frame(table(df$year, df$ses, df$alcohol_state), stringsAsFactors = FALSE)
  names(tab) <- c("year", "ses", "alcohol_state", "n")
  tab$year <- as.integer(as.character(tab$year))
  tab$n <- as.numeric(tab$n)
  
  totals <- ave(tab$n, tab$year, tab$ses, FUN = sum)
  tab$prop <- ifelse(totals > 0, tab$n / totals, NA_real_)
  
  ggplot2::ggplot(tab, ggplot2::aes(x = year, y = prop, colour = alcohol_state, group = alcohol_state)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::facet_wrap(~ ses, nrow = 1) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(
      title = "Alcohol prevalence by calendar year and SES",
      x = "Calendar year",
      y = "Prevalence",
      colour = "Alcohol state"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   legend.position = "bottom")
}

# Plot obesity prevalence by year and sex / SES.
plot_obesity_prevalence_by_year_sex_ses <- function(sim_out,
                                                    year_fun = floor) {
  .output_require_ggplot2()
  .output_check_sim_out(sim_out)
  
  long_df <- sim_out$long
  
  required_cols <- c("sex", "ses", "obese_indicator")
  missing_cols <- setdiff(required_cols, names(long_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Longitudinal table is missing required columns for obesity plotting: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  df <- long_df
  df$year <- .output_get_long_years(df, year_fun = year_fun)
  
  df <- df[!is.na(df$year) & !is.na(df$sex) & !is.na(df$ses) & !is.na(df$obese_indicator), , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(.output_plot_empty(
      title = "Obesity prevalence by year and sex / SES",
      message = "No obesity-indicator data available for plotting."
    ))
  }
  
  mean_df <- stats::aggregate(obese_indicator ~ year + sex + ses, data = df, FUN = mean)
  
  ggplot2::ggplot(mean_df, ggplot2::aes(x = year, y = obese_indicator, colour = sex, group = sex)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::facet_wrap(~ ses, nrow = 1) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(
      title = "Obesity prevalence by year and sex / SES",
      x = "Calendar year",
      y = "Obesity prevalence",
      colour = "Sex"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   legend.position = "bottom")
}

# Plot case counts by diagnosis year.
plot_case_counts_by_diagnosis_year <- function(sim_out,
                                               event_var = NULL,
                                               year_fun = floor) {
  .output_require_ggplot2()
  .output_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  detected_event_var <- .detect_event_var(patient_df, event_var = event_var)
  
  if (is.null(detected_event_var)) {
    return(.output_plot_empty(
      title = "Case counts by diagnosis year",
      message = "No event variable could be detected."
    ))
  }
  
  diagnosis_year <- .output_get_diagnosis_years(patient_df, year_fun = year_fun)
  
  df <- data.frame(
    event = patient_df[[detected_event_var]],
    diagnosis_year = diagnosis_year,
    stringsAsFactors = FALSE
  )
  
  df <- df[df$event == 1 & !is.na(df$diagnosis_year), , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(.output_plot_empty(
      title = "Case counts by diagnosis year",
      message = "No diagnosed cases available for plotting."
    ))
  }
  
  tab <- as.data.frame(table(df$diagnosis_year), stringsAsFactors = FALSE)
  names(tab) <- c("diagnosis_year", "n")
  tab$diagnosis_year <- as.integer(as.character(tab$diagnosis_year))
  tab$n <- as.numeric(tab$n)
  
  ggplot2::ggplot(tab, ggplot2::aes(x = diagnosis_year, y = n)) +
    ggplot2::geom_col(fill = "#2C7FB8") +
    ggplot2::labs(
      title = "Case counts by diagnosis year",
      x = "Diagnosis year",
      y = "Number of cases"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

# Plot stage distribution among diagnosed cases.
plot_stage_distribution_among_cases <- function(sim_out,
                                                event_var = NULL,
                                                normalise = TRUE) {
  .output_require_ggplot2()
  .output_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  detected_event_var <- .detect_event_var(patient_df, event_var = event_var)
  
  if (is.null(detected_event_var)) {
    return(.output_plot_empty(
      title = "Stage distribution among cases",
      message = "No event variable could be detected."
    ))
  }
  
  if (!("stage" %in% names(patient_df))) {
    return(.output_plot_empty(
      title = "Stage distribution among cases",
      message = "No stage variable found in patient-level output."
    ))
  }
  
  df <- patient_df[
    patient_df[[detected_event_var]] == 1 & !is.na(patient_df$stage),
    "stage",
    drop = FALSE
  ]
  
  if (nrow(df) == 0) {
    return(.output_plot_empty(
      title = "Stage distribution among cases",
      message = "No staged cases available for plotting."
    ))
  }
  
  tab <- as.data.frame(table(df$stage), stringsAsFactors = FALSE)
  names(tab) <- c("stage", "n")
  tab$n <- as.numeric(tab$n)
  tab$prop <- tab$n / sum(tab$n)
  
  if (isTRUE(normalise)) {
    ggplot2::ggplot(tab, ggplot2::aes(x = stage, y = prop, fill = stage)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = paste0("n=", n)), vjust = -0.3, size = 3.5) +
      ggplot2::scale_y_continuous(
        limits = c(0, min(1, max(tab$prop) * 1.15)),
        labels = function(x) paste0(round(100 * x), "%")
      ) +
      ggplot2::labs(
        title = "Stage distribution among cases",
        x = "Stage",
        y = "Proportion of cases"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  } else {
    ggplot2::ggplot(tab, ggplot2::aes(x = stage, y = n, fill = stage)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.3, size = 3.5) +
      ggplot2::labs(
        title = "Stage distribution among cases",
        x = "Stage",
        y = "Number of cases"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }
}

# Generate the standard set of simulation diagnostic plots.
#
# Returned value:
# - a named list of ggplot objects
#
# If output_dir is supplied, all plots are also saved as PNG files.
plot_simulation_diagnostics <- function(sim_out,
                                        event_var = NULL,
                                        year_fun = floor,
                                        output_dir = NULL,
                                        file_prefix = "simulation_diagnostics",
                                        width = 8,
                                        height = 5,
                                        dpi = 300) {
  .output_require_ggplot2()
  .output_check_sim_out(sim_out)
  
  plots <- list(
    alcohol_prevalence_by_year_ses = plot_alcohol_prevalence_by_year_ses(sim_out, year_fun = year_fun),
    obesity_prevalence_by_year_sex_ses = plot_obesity_prevalence_by_year_sex_ses(sim_out, year_fun = year_fun),
    case_counts_by_diagnosis_year = plot_case_counts_by_diagnosis_year(sim_out, event_var = event_var, year_fun = year_fun),
    stage_distribution_among_cases = plot_stage_distribution_among_cases(sim_out, event_var = event_var, normalise = TRUE)
  )
  
  if (!is.null(output_dir)) {
    .output_save_plot_if_requested(plots$alcohol_prevalence_by_year_ses,
                                   paste0(file_prefix, "_alcohol_prevalence_by_year_ses.png"),
                                   output_dir, width, height, dpi)
    .output_save_plot_if_requested(plots$obesity_prevalence_by_year_sex_ses,
                                   paste0(file_prefix, "_obesity_prevalence_by_year_sex_ses.png"),
                                   output_dir, width, height, dpi)
    .output_save_plot_if_requested(plots$case_counts_by_diagnosis_year,
                                   paste0(file_prefix, "_case_counts_by_diagnosis_year.png"),
                                   output_dir, width, height, dpi)
    .output_save_plot_if_requested(plots$stage_distribution_among_cases,
                                   paste0(file_prefix, "_stage_distribution_among_cases.png"),
                                   output_dir, width, height, dpi)
  }
  
  plots
}
