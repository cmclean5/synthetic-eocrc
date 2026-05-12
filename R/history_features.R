# Build and update rolling history features for simulated persons.
#
# This script contains helper functions for managing longitudinal exposure
# history and deriving recent or cumulative summaries from that history.
#
# Main user-facing functions:
# - initialize_history_state()
# - append_history_state()
# - compute_history_features()
# - history_window_mean()
# - history_window_prop_ge()
# - history_last_value()
#
# Current bridge-model use:
# - recent and cumulative alcohol score means
# - recent hazardous-drinking proportion
# - recent and cumulative visceral-adiposity means
# - recent and cumulative log-insulin means
# - recent high-insulin proportion
#
# Design choices:
# - history is stored as a simple named list of vectors
# - empty histories are allowed and handled safely
# - recent-window summaries fall back to user-supplied defaults when no
#   observations are available
# - insulin history is stored on the natural scale and log-transformed only
#   when needed for derived features
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()

# Validate that a history state object has the expected structure.
#
# Expected fields are:
# - score_hist
# - units_hist
# - visceral_hist
# - insulin_hist
# - time_hist
#
# Each should be a numeric vector, and all vectors must have the same length.
.validate_history_state <- function(history) {
  if (!is.list(history)) {
    stop("`history` must be a list.", call. = FALSE)
  }
  
  required_names <- c(
    "score_hist",
    "units_hist",
    "visceral_hist",
    "insulin_hist",
    "time_hist"
  )
  
  missing_names <- setdiff(required_names, names(history))
  
  if (length(missing_names) > 0) {
    stop(
      "History state is missing required elements: ",
      paste(missing_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  lengths <- vapply(history[required_names], length, integer(1))
  
  if (length(unique(lengths)) != 1) {
    stop(
      "All history vectors must have the same length.",
      call. = FALSE
    )
  }
  
  numeric_ok <- vapply(
    history[required_names],
    is.numeric,
    logical(1)
  )
  
  if (!all(numeric_ok)) {
    stop("All history elements must be numeric vectors.", call. = FALSE)
  }
  
  invisible(TRUE)
}

# Initialise an empty history state.
#
# The returned object can be passed directly to:
# - append_history_state()
# - compute_history_features()
#
# All vectors are initially empty, so recent and cumulative feature functions
# will use their fallback values until observations are appended.
initialize_history_state <- function() {
  history <- list(
    score_hist = numeric(0),
    units_hist = numeric(0),
    visceral_hist = numeric(0),
    insulin_hist = numeric(0),
    time_hist = numeric(0)
  )
  
  .validate_history_state(history)
  history
}

# Return the values in either:
# - the full history
# - a recent historical window ending just before current_time
#
# Arguments:
# - x:
#   numeric vector of measurements
# - times:
#   numeric vector of measurement times
# - current_time:
#   current calendar time
# - window_years:
#   if NULL, use all history
#   otherwise use values satisfying:
#   current_time - window_years <= time < current_time
#
# If inputs are invalid or empty, an empty numeric vector is returned.
.history_window_values <- function(x,
                                   times,
                                   current_time,
                                   window_years = NULL) {
  if (!is.numeric(x) || !is.numeric(times) || length(x) != length(times)) {
    return(numeric(0))
  }
  
  if (!is_scalar_number(current_time)) {
    return(numeric(0))
  }
  
  if (length(x) == 0) {
    return(numeric(0))
  }
  
  if (is.null(window_years)) {
    return(x)
  }
  
  if (!is_scalar_number(window_years) || window_years < 0) {
    return(numeric(0))
  }
  
  idx <- times >= (current_time - window_years) & times < current_time
  
  x[idx]
}

# Compute the mean of values over either:
# - all history
# - a recent time window
#
# If no non-missing values are available, return `fallback`.
history_window_mean <- function(x,
                                times,
                                current_time,
                                window_years = NULL,
                                fallback = 0) {
  vals <- .history_window_values(
    x = x,
    times = times,
    current_time = current_time,
    window_years = window_years
  )
  
  if (length(vals) == 0 || all(is.na(vals))) {
    return(fallback)
  }
  
  mean(vals, na.rm = TRUE)
}

# Compute the proportion of values greater than or equal to a threshold over
# either:
# - all history
# - a recent time window
#
# If no non-missing values are available, return `fallback`.
history_window_prop_ge <- function(x,
                                   times,
                                   current_time,
                                   threshold,
                                   window_years = NULL,
                                   fallback = 0) {
  vals <- .history_window_values(
    x = x,
    times = times,
    current_time = current_time,
    window_years = window_years
  )
  
  if (length(vals) == 0 || all(is.na(vals))) {
    return(fallback)
  }
  
  mean(vals >= threshold, na.rm = TRUE)
}

# Return the last non-missing value in a vector.
#
# If no non-missing value exists, return `fallback`.
history_last_value <- function(x, fallback = NA_real_) {
  if (!is.numeric(x) || length(x) == 0 || all(is.na(x))) {
    return(fallback)
  }
  
  idx <- which(!is.na(x))
  
  if (length(idx) == 0) {
    return(fallback)
  }
  
  x[max(idx)]
}

# Append one time-point of latent or exposure history.
#
# Arguments:
# - history:
#   existing history state from initialize_history_state()
# - cal_time:
#   time of the new observation
# - alcohol_score:
#   optional alcohol score
# - alcohol_units:
#   optional alcohol units
# - visceral_true:
#   optional true adiposity value
# - insulin_true:
#   optional true insulin value on the natural scale
#
# Design choice:
# - if a supplied value is NULL, NA_real_ is appended for that field
# - this keeps all history vectors aligned to the same time index
append_history_state <- function(history,
                                 cal_time,
                                 alcohol_score = NULL,
                                 alcohol_units = NULL,
                                 visceral_true = NULL,
                                 insulin_true = NULL) {
  .validate_history_state(history)
  
  if (!is_scalar_number(cal_time)) {
    stop("`cal_time` must be a numeric scalar.", call. = FALSE)
  }
  
  if (!is.null(alcohol_score) && !is_scalar_number(alcohol_score)) {
    stop("`alcohol_score` must be NULL or a numeric scalar.", call. = FALSE)
  }
  
  if (!is.null(alcohol_units) && !is_scalar_number(alcohol_units)) {
    stop("`alcohol_units` must be NULL or a numeric scalar.", call. = FALSE)
  }
  
  if (!is.null(visceral_true) && !is_scalar_number(visceral_true)) {
    stop("`visceral_true` must be NULL or a numeric scalar.", call. = FALSE)
  }
  
  if (!is.null(insulin_true) && !is_scalar_number(insulin_true)) {
    stop("`insulin_true` must be NULL or a numeric scalar.", call. = FALSE)
  }
  
  history$score_hist <- c(
    history$score_hist,
    if (is.null(alcohol_score)) NA_real_ else as.numeric(alcohol_score)
  )
  
  history$units_hist <- c(
    history$units_hist,
    if (is.null(alcohol_units)) NA_real_ else as.numeric(alcohol_units)
  )
  
  history$visceral_hist <- c(
    history$visceral_hist,
    if (is.null(visceral_true)) NA_real_ else as.numeric(visceral_true)
  )
  
  history$insulin_hist <- c(
    history$insulin_hist,
    if (is.null(insulin_true)) NA_real_ else as.numeric(insulin_true)
  )
  
  history$time_hist <- c(
    history$time_hist,
    as.numeric(cal_time)
  )
  
  .validate_history_state(history)
  history
}

# Compute the full set of current history-derived features used by the current
# simulator.
#
# Arguments:
# - history:
#   history state object
# - current_time:
#   current calendar time at which features are needed
# - recent_window_years:
#   width of the recent-history window
# - high_log_insulin_threshold:
#   threshold on the log scale used to define high recent insulin
# - fallback_visceral:
#   fallback value if no visceral history is available
# - fallback_insulin:
#   fallback value on the natural scale if no insulin history is available
#
# Returned fields:
# - recent_mean_score
# - cum_mean_score
# - haz_recent_prop
# - recent_visceral
# - cum_visceral
# - recent_log_insulin
# - cum_log_insulin
# - high_insulin_recent_prop
#
# Current bridge-model conventions:
# - hazardous alcohol score threshold is 2
# - insulin is log-transformed as log(insulin + 1e-8)
compute_history_features <- function(history,
                                     current_time,
                                     recent_window_years = 2,
                                     high_log_insulin_threshold = NULL,
                                     fallback_visceral = NA_real_,
                                     fallback_insulin = NA_real_) {
  .validate_history_state(history)
  
  if (!is_scalar_number(current_time)) {
    stop("`current_time` must be a numeric scalar.", call. = FALSE)
  }
  
  if (!is_scalar_number(recent_window_years) || recent_window_years < 0) {
    stop("`recent_window_years` must be a non-negative numeric scalar.", call. = FALSE)
  }
  
  if (!is.null(high_log_insulin_threshold) &&
      !is_scalar_number(high_log_insulin_threshold)) {
    stop(
      "`high_log_insulin_threshold` must be NULL or a numeric scalar.",
      call. = FALSE
    )
  }
  
  if (!is_scalar_number(fallback_insulin) && !is.na(fallback_insulin)) {
    stop(
      "`fallback_insulin` must be a numeric scalar or NA_real_.",
      call. = FALSE
    )
  }
  
  if (!is_scalar_number(fallback_visceral) && !is.na(fallback_visceral)) {
    stop(
      "`fallback_visceral` must be a numeric scalar or NA_real_.",
      call. = FALSE
    )
  }
  
  # Compute alcohol-history summaries.
  recent_mean_score <- history_window_mean(
    x = history$score_hist,
    times = history$time_hist,
    current_time = current_time,
    window_years = recent_window_years,
    fallback = 0
  )
  
  cum_mean_score <- history_window_mean(
    x = history$score_hist,
    times = history$time_hist,
    current_time = current_time,
    window_years = NULL,
    fallback = 0
  )
  
  haz_recent_prop <- history_window_prop_ge(
    x = history$score_hist,
    times = history$time_hist,
    current_time = current_time,
    threshold = 2,
    window_years = recent_window_years,
    fallback = 0
  )
  
  # Compute adiposity-history summaries.
  recent_visceral <- history_window_mean(
    x = history$visceral_hist,
    times = history$time_hist,
    current_time = current_time,
    window_years = recent_window_years,
    fallback = fallback_visceral
  )
  
  cum_visceral <- history_window_mean(
    x = history$visceral_hist,
    times = history$time_hist,
    current_time = current_time,
    window_years = NULL,
    fallback = fallback_visceral
  )
  
  # Transform insulin history to the log scale in a numerically safe way.
  #
  # A small positive offset is added to avoid issues if insulin values are zero
  # or if placeholder zeroes appear during development.
  log_insulin_hist <- log(history$insulin_hist + 1e-8)
  
  recent_log_insulin <- history_window_mean(
    x = log_insulin_hist,
    times = history$time_hist,
    current_time = current_time,
    window_years = recent_window_years,
    fallback = if (is.na(fallback_insulin)) NA_real_ else log(fallback_insulin)
  )
  
  cum_log_insulin <- history_window_mean(
    x = log_insulin_hist,
    times = history$time_hist,
    current_time = current_time,
    window_years = NULL,
    fallback = if (is.na(fallback_insulin)) NA_real_ else log(fallback_insulin)
  )
  
  high_insulin_recent_prop <- if (is.null(high_log_insulin_threshold)) {
    NA_real_
  } else {
    history_window_prop_ge(
      x = log_insulin_hist,
      times = history$time_hist,
      current_time = current_time,
      threshold = high_log_insulin_threshold,
      window_years = recent_window_years,
      fallback = 0
    )
  }
  
  list(
    recent_mean_score = recent_mean_score,
    cum_mean_score = cum_mean_score,
    haz_recent_prop = haz_recent_prop,
    recent_visceral = recent_visceral,
    cum_visceral = cum_visceral,
    recent_log_insulin = recent_log_insulin,
    cum_log_insulin = cum_log_insulin,
    high_insulin_recent_prop = high_insulin_recent_prop
  )
}
