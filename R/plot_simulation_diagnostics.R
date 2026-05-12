# Plot diagnostic summaries for simulated cohort outputs.
#
# These functions are designed to work with simulation output objects returned by:
# - simulate_cohort_from_spec()
# - simulate_cohort_short_from_spec()
# - simulate_cohort_long_from_spec()
#
# The script focuses on a small set of practical validation plots:
# - alcohol prevalence by calendar year and SES
# - obesity prevalence by year and sex / SES
# - case counts by diagnosis year
# - stage distribution among cases
#
# Main user-facing functions:
# - plot_alcohol_prevalence_by_year_ses()
# - plot_obesity_prevalence_by_year_sex_ses()
# - plot_case_counts_by_diagnosis_year()
# - plot_stage_distribution_among_cases()
# - plot_simulation_diagnostics()
#
# Design choices:
# - ggplot2 is used for plotting
# - base R is used for data manipulation to keep dependencies light
# - functions return ggplot objects rather than printing automatically
# - if a plot has no data, a valid placeholder ggplot is returned instead of erroring

# Run:
# source('plot_simulation_diagnostics.R')
# plots <- plot_simulation_diagnostics(
#   sim_out = out,
#   output_dir = "outputs/diagnostic_plots"
# )

# Check whether an object is a single non-empty string.
.sim_plot_is_scalar_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

# Require ggplot2 and stop with a clear error if it is not installed.
.sim_plot_require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for simulation diagnostic plots.",
      call. = FALSE
    )
  }
}

# Check that the simulation output has the expected structure.
#
# Expected elements are:
# - patient
# - long
.sim_plot_check_sim_out <- function(sim_out) {
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

# Extract integer years from a numeric calendar-time vector.
#
# By default, floor() is used so that values such as 2018.7 are assigned to 2018.
.sim_plot_extract_year <- function(x, year_fun = floor) {
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
.sim_plot_detect_event_var <- function(patient_df, event_var = NULL) {
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

# Return a placeholder ggplot object when there is no data available for a plot.
#
# This keeps the plotting pipeline robust and lets wrapper functions return
# a full set of plot objects even when, for example, there are no diagnosed cases.
.sim_plot_empty <- function(title, message) {
  .sim_plot_require_ggplot2()
  
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, size = 5) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

# Save a ggplot object if an output directory has been requested.
#
# The directory is created if it does not already exist.
.sim_plot_save_if_requested <- function(plot_obj,
                                        filename = NULL,
                                        output_dir = NULL,
                                        width = 8,
                                        height = 5,
                                        dpi = 300) {
  if (is.null(output_dir) || is.null(filename)) {
    return(invisible(NULL))
  }
  
  .sim_plot_require_ggplot2()
  
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

# Build a year variable from either:
# - cal_time_raw
# - cal_time
#
# If neither exists, return all-NA years.
.sim_plot_get_long_years <- function(long_df, year_fun = floor) {
  if ("cal_time_raw" %in% names(long_df)) {
    return(.sim_plot_extract_year(long_df$cal_time_raw, year_fun = year_fun))
  }
  
  if ("cal_time" %in% names(long_df)) {
    return(.sim_plot_extract_year(long_df$cal_time, year_fun = year_fun))
  }
  
  rep(NA_integer_, nrow(long_df))
}

# Build diagnosis years from either:
# - cal_time_at_diagnosis_raw
# - cal_time_at_diagnosis
#
# If neither exists, return all-NA years.
.sim_plot_get_diagnosis_years <- function(patient_df, year_fun = floor) {
  if ("cal_time_at_diagnosis_raw" %in% names(patient_df)) {
    return(.sim_plot_extract_year(patient_df$cal_time_at_diagnosis_raw, year_fun = year_fun))
  }
  
  if ("cal_time_at_diagnosis" %in% names(patient_df)) {
    return(.sim_plot_extract_year(patient_df$cal_time_at_diagnosis, year_fun = year_fun))
  }
  
  rep(NA_integer_, nrow(patient_df))
}

# Plot alcohol prevalence by calendar year and SES.
#
# This uses the visit-level long table and calculates, for each year and SES:
# - the proportion in each alcohol state
#
# Plot design:
# - x-axis: calendar year
# - y-axis: prevalence
# - colour: alcohol state
# - facets: SES
plot_alcohol_prevalence_by_year_ses <- function(sim_out,
                                                states = NULL,
                                                year_fun = floor) {
  .sim_plot_require_ggplot2()
  .sim_plot_check_sim_out(sim_out)
  
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
  df$year <- .sim_plot_get_long_years(df, year_fun = year_fun)
  
  df <- df[!is.na(df$year) & !is.na(df$ses) & !is.na(df$alcohol_state), , drop = FALSE]
  
  if (!is.null(states)) {
    df <- df[df$alcohol_state %in% states, , drop = FALSE]
  }
  
  if (nrow(df) == 0) {
    return(.sim_plot_empty(
      title = "Alcohol prevalence by calendar year and SES",
      message = "No alcohol-state data available for plotting."
    ))
  }
  
  tab <- as.data.frame(
    table(df$year, df$ses, df$alcohol_state),
    stringsAsFactors = FALSE
  )
  names(tab) <- c("year", "ses", "alcohol_state", "n")
  
  tab$year <- as.integer(as.character(tab$year))
  tab$n <- as.numeric(tab$n)
  
  totals <- ave(tab$n, tab$year, tab$ses, FUN = sum)
  tab$prop <- ifelse(totals > 0, tab$n / totals, NA_real_)
  
  tab <- tab[tab$n > 0 | tab$prop > 0, , drop = FALSE]
  
  ggplot2::ggplot(
    tab,
    ggplot2::aes(x = year, y = prop, colour = alcohol_state, group = alcohol_state)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::facet_wrap(~ ses, nrow = 1) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = function(x) paste0(round(100 * x), "%")
    ) +
    ggplot2::labs(
      title = "Alcohol prevalence by calendar year and SES",
      x = "Calendar year",
      y = "Prevalence",
      colour = "Alcohol state"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Plot obesity prevalence by year and sex / SES.
#
# This uses the visit-level long table and calculates the mean obese indicator
# within each year, sex, and SES group.
#
# Plot design:
# - x-axis: calendar year
# - y-axis: obesity prevalence
# - colour: sex
# - facets: SES
plot_obesity_prevalence_by_year_sex_ses <- function(sim_out,
                                                    year_fun = floor) {
  .sim_plot_require_ggplot2()
  .sim_plot_check_sim_out(sim_out)
  
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
  df$year <- .sim_plot_get_long_years(df, year_fun = year_fun)
  
  df <- df[!is.na(df$year) & !is.na(df$sex) & !is.na(df$ses) & !is.na(df$obese_indicator), , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(.sim_plot_empty(
      title = "Obesity prevalence by year and sex / SES",
      message = "No obesity-indicator data available for plotting."
    ))
  }
  
  mean_df <- stats::aggregate(
    obese_indicator ~ year + sex + ses,
    data = df,
    FUN = mean
  )
  
  n_df <- stats::aggregate(
    obese_indicator ~ year + sex + ses,
    data = df,
    FUN = length
  )
  names(n_df)[names(n_df) == "obese_indicator"] <- "n"
  
  plot_df <- merge(mean_df, n_df, by = c("year", "sex", "ses"), all = TRUE)
  rownames(plot_df) <- NULL
  
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = year, y = obese_indicator, colour = sex, group = sex)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::facet_wrap(~ ses, nrow = 1) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = function(x) paste0(round(100 * x), "%")
    ) +
    ggplot2::labs(
      title = "Obesity prevalence by year and sex / SES",
      x = "Calendar year",
      y = "Obesity prevalence",
      colour = "Sex"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Plot case counts by diagnosis year.
#
# This uses the patient-level table and counts diagnosed cases by year of diagnosis.
#
# Plot design:
# - x-axis: diagnosis year
# - y-axis: number of diagnosed cases
# - geometry: bars
plot_case_counts_by_diagnosis_year <- function(sim_out,
                                               event_var = NULL,
                                               year_fun = floor) {
  .sim_plot_require_ggplot2()
  .sim_plot_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  detected_event_var <- .sim_plot_detect_event_var(patient_df, event_var = event_var)
  
  if (is.null(detected_event_var)) {
    return(.sim_plot_empty(
      title = "Case counts by diagnosis year",
      message = "No event variable could be detected."
    ))
  }
  
  diagnosis_year <- .sim_plot_get_diagnosis_years(patient_df, year_fun = year_fun)
  
  df <- data.frame(
    event = patient_df[[detected_event_var]],
    diagnosis_year = diagnosis_year,
    stringsAsFactors = FALSE
  )
  
  df <- df[df$event == 1 & !is.na(df$diagnosis_year), , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(.sim_plot_empty(
      title = "Case counts by diagnosis year",
      message = "No diagnosed cases available for plotting."
    ))
  }
  
  tab <- as.data.frame(table(df$diagnosis_year), stringsAsFactors = FALSE)
  names(tab) <- c("diagnosis_year", "n")
  tab$diagnosis_year <- as.integer(as.character(tab$diagnosis_year))
  tab$n <- as.numeric(tab$n)
  
  ggplot2::ggplot(
    tab,
    ggplot2::aes(x = diagnosis_year, y = n)
  ) +
    ggplot2::geom_col(fill = "#2C7FB8") +
    ggplot2::labs(
      title = "Case counts by diagnosis year",
      x = "Diagnosis year",
      y = "Number of cases"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )
}

# Plot stage distribution among diagnosed cases.
#
# This uses the patient-level table and calculates the distribution of stage
# among cases with non-missing stage information.
#
# Plot design:
# - x-axis: stage
# - y-axis: either proportion or count
# - geometry: bars
#
# Arguments:
# - normalise:
#   if TRUE, plot proportions
#   if FALSE, plot raw counts
plot_stage_distribution_among_cases <- function(sim_out,
                                                event_var = NULL,
                                                normalise = TRUE) {
  .sim_plot_require_ggplot2()
  .sim_plot_check_sim_out(sim_out)
  
  patient_df <- sim_out$patient
  detected_event_var <- .sim_plot_detect_event_var(patient_df, event_var = event_var)
  
  if (is.null(detected_event_var)) {
    return(.sim_plot_empty(
      title = "Stage distribution among cases",
      message = "No event variable could be detected."
    ))
  }
  
  if (!("stage" %in% names(patient_df))) {
    return(.sim_plot_empty(
      title = "Stage distribution among cases",
      message = "No stage variable found in patient-level output."
    ))
  }
  
  df <- patient_df[
    patient_df[[detected_event_var]] == 1 & !is.na(patient_df$stage),
    c(detected_event_var, "stage"),
    drop = FALSE
  ]
  
  if (nrow(df) == 0) {
    return(.sim_plot_empty(
      title = "Stage distribution among cases",
      message = "No staged cases available for plotting."
    ))
  }
  
  tab <- as.data.frame(table(df$stage), stringsAsFactors = FALSE)
  names(tab) <- c("stage", "n")
  tab$n <- as.numeric(tab$n)
  tab$prop <- tab$n / sum(tab$n)
  
  if (isTRUE(normalise)) {
    ggplot2::ggplot(
      tab,
      ggplot2::aes(x = stage, y = prop, fill = stage)
    ) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0("n=", n)),
        vjust = -0.3,
        size = 3.5
      ) +
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
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold")
      )
  } else {
    ggplot2::ggplot(
      tab,
      ggplot2::aes(x = stage, y = n, fill = stage)
    ) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(label = n),
        vjust = -0.3,
        size = 3.5
      ) +
      ggplot2::labs(
        title = "Stage distribution among cases",
        x = "Stage",
        y = "Number of cases"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold")
      )
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
  .sim_plot_require_ggplot2()
  .sim_plot_check_sim_out(sim_out)
  
  plots <- list(
    alcohol_prevalence_by_year_ses = plot_alcohol_prevalence_by_year_ses(
      sim_out = sim_out,
      year_fun = year_fun
    ),
    obesity_prevalence_by_year_sex_ses = plot_obesity_prevalence_by_year_sex_ses(
      sim_out = sim_out,
      year_fun = year_fun
    ),
    case_counts_by_diagnosis_year = plot_case_counts_by_diagnosis_year(
      sim_out = sim_out,
      event_var = event_var,
      year_fun = year_fun
    ),
    stage_distribution_among_cases = plot_stage_distribution_among_cases(
      sim_out = sim_out,
      event_var = event_var,
      normalise = TRUE
    )
  )
  
  # Save plots if requested.
  if (!is.null(output_dir)) {
    .sim_plot_save_if_requested(
      plots$alcohol_prevalence_by_year_ses,
      filename = paste0(file_prefix, "_alcohol_prevalence_by_year_ses.png"),
      output_dir = output_dir,
      width = width,
      height = height,
      dpi = dpi
    )
    
    .sim_plot_save_if_requested(
      plots$obesity_prevalence_by_year_sex_ses,
      filename = paste0(file_prefix, "_obesity_prevalence_by_year_sex_ses.png"),
      output_dir = output_dir,
      width = width,
      height = height,
      dpi = dpi
    )
    
    .sim_plot_save_if_requested(
      plots$case_counts_by_diagnosis_year,
      filename = paste0(file_prefix, "_case_counts_by_diagnosis_year.png"),
      output_dir = output_dir,
      width = width,
      height = height,
      dpi = dpi
    )
    
    .sim_plot_save_if_requested(
      plots$stage_distribution_among_cases,
      filename = paste0(file_prefix, "_stage_distribution_among_cases.png"),
      output_dir = output_dir,
      width = width,
      height = height,
      dpi = dpi
    )
  }
  
  plots
}
