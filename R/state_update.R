# Update the within-visit person state for the tumour simulator.
#
# This script coordinates the main non-disease state changes that happen at a
# visit. In the current bridge-model architecture, this means:
# - derive recent and cumulative history features
# - sample alcohol exposure
# - update adiposity and latent metabolic state
# - update insulin
# - generate observed measurements
# - optionally append current values to the stored longitudinal history
#
# Main user-facing functions:
# - compute_state_history_features()
# - update_person_state()
# - update_person_state_short()
# - update_person_state_long()
#
# Important design notes:
# - this module updates the person's current biological and behavioural state,
#   but it does not:
#   - advance age or calendar time
#   - increment visit counters
#   - simulate disease events
#   - simulate mortality events
# - those tasks belong in simulate_person.R
#
# Returned structure from update_person_state():
# - state:
#   updated person state to carry forward
# - history:
#   updated history object
# - history_features:
#   history-derived summaries used for the current visit update
# - current:
#   current visit-level true and observed variables needed by downstream code
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()
#   - list_numeric_or_default()
# - from history_features.R:
#   - initialize_history_state()
#   - compute_history_features()
#   - append_history_state()
# - from exposure_alcohol.R:
#   - sample_alcohol_exposure()
# - from exposure_adiposity.R:
#   - update_adiposity_exposure()
# - from exposure_insulin.R:
#   - update_insulin_exposure()
#   - get_insulin_high_log_threshold()
# - from observation_models.R:
#   - build_observation_context()
#   - generate_current_observations()

# Validate that the state object contains the minimum required structure.
#
# The state object is expected to come from initialize_person_state().
.state_update_validate_state <- function(state) {
  if (!is.list(state)) {
    stop("`state` must be a list.", call. = FALSE)
  }
  
  required_fields <- c(
    "disease",
    "sex",
    "education",
    "ses",
    "ethnicity",
    "geography",
    "age",
    "cal_time",
    "visit",
    "latent_metabolic",
    "latent_crc",
    "prev_alcohol_score",
    "prev_visceral",
    "prev_insulin",
    "family_history",
    "genetic"
  )
  
  missing_fields <- required_fields[!required_fields %in% names(state)]
  
  if (length(missing_fields) > 0) {
    stop(
      "State object is missing required fields: ",
      paste(missing_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!is.list(state$family_history)) {
    stop("`state$family_history` must be a list.", call. = FALSE)
  }
  
  if (!is.list(state$genetic)) {
    stop("`state$genetic` must be a list.", call. = FALSE)
  }
  
  invisible(TRUE)
}

# Return a valid history object.
#
# If `history` is NULL, create an empty history state. Otherwise return the
# supplied history unchanged.
.state_update_get_history <- function(history = NULL) {
  if (is.null(history)) {
    return(initialize_history_state())
  }
  
  history
}

# Compute the history-derived summaries needed for the current visit update.
#
# This is a thin orchestration wrapper around compute_history_features() that
# supplies the bridge-model defaults:
# - fallback visceral value comes from state$prev_visceral
# - fallback insulin value comes from state$prev_insulin
# - the high-insulin threshold comes from the config
compute_state_history_features <- function(spec,
                                           state,
                                           history = NULL,
                                           recent_window_years = 2) {
  .state_update_validate_state(state)
  
  history <- .state_update_get_history(history)
  
  if (!is_scalar_number(recent_window_years) || recent_window_years < 0) {
    stop("`recent_window_years` must be a non-negative numeric scalar.", call. = FALSE)
  }
  
  compute_history_features(
    history = history,
    current_time = state$cal_time,
    recent_window_years = recent_window_years,
    high_log_insulin_threshold = get_insulin_high_log_threshold(spec),
    fallback_visceral = state$prev_visceral,
    fallback_insulin = state$prev_insulin
  )
}

# Build the standard observation context for the current person state.
#
# This is separated into a helper so the logic stays in one place and can later
# be extended without changing the main update function.
.state_update_build_observation_context <- function(state) {
  fh_diabetes <- list_numeric_or_default(
    x = state$family_history,
    name = "fh_diabetes",
    fallback = 0
  )
  
  build_observation_context(
    sex = state$sex,
    ses = state$ses,
    education = state$education,
    ethnicity = state$ethnicity,
    geography = state$geography,
    fh_diabetes = fh_diabetes
  )
}

# Build a compact list of current visit values from component module outputs.
#
# The goal is to provide simulate_person.R with a single object containing the
# current visit's relevant true and observed variables.
.state_update_build_current_output <- function(memory_model,
                                               history_features,
                                               alcohol_out,
                                               adiposity_out,
                                               insulin_out,
                                               observation_out) {
  probs <- alcohol_out$probs %||% numeric(0)
  
  prob_non_drinker <- if ("non_drinker" %in% names(probs)) probs["non_drinker"] else NA_real_
  prob_moderate <- if ("moderate_drinker" %in% names(probs)) probs["moderate_drinker"] else NA_real_
  prob_hazardous <- if ("hazardous_drinker" %in% names(probs)) probs["hazardous_drinker"] else NA_real_
  
  list(
    memory_model = memory_model,
    alcohol_state = alcohol_out$state,
    alcohol_score = alcohol_out$score,
    alcohol_units = alcohol_out$units,
    alcohol_probs = probs,
    prob_non_drinker = prob_non_drinker,
    prob_moderate = prob_moderate,
    prob_hazardous = prob_hazardous,
    p_obesity_target = adiposity_out$p_obesity_target,
    target_mean_visceral = adiposity_out$target_mean_visceral,
    mean_visceral = adiposity_out$mean_visceral,
    visceral_true = adiposity_out$visceral_true,
    obese_indicator = adiposity_out$obese_indicator,
    mean_log_insulin = insulin_out$mean_log_insulin,
    log_insulin_true = insulin_out$log_insulin_true,
    insulin_true = insulin_out$insulin_true,
    high_log_threshold = insulin_out$high_log_threshold,
    high_insulin_indicator = insulin_out$high_insulin_indicator,
    visceral_obs = observation_out$visceral_obs,
    visceral_observed_indicator = observation_out$visceral_observed_indicator,
    p_measure_visceral = observation_out$p_measure_visceral,
    insulin_obs = observation_out$insulin_obs,
    insulin_observed_indicator = observation_out$insulin_observed_indicator,
    p_measure_insulin = observation_out$p_measure_insulin,
    recent_mean_score = list_numeric_or_default(history_features, "recent_mean_score", NA_real_),
    cum_mean_score = list_numeric_or_default(history_features, "cum_mean_score", NA_real_),
    haz_recent_prop = list_numeric_or_default(history_features, "haz_recent_prop", NA_real_),
    recent_visceral = list_numeric_or_default(history_features, "recent_visceral", NA_real_),
    cum_visceral = list_numeric_or_default(history_features, "cum_visceral", NA_real_),
    recent_log_insulin = list_numeric_or_default(history_features, "recent_log_insulin", NA_real_),
    cum_log_insulin = list_numeric_or_default(history_features, "cum_log_insulin", NA_real_),
    high_insulin_recent_prop = list_numeric_or_default(history_features, "high_insulin_recent_prop", NA_real_)
  )
}

# Update the person state for one visit.
#
# Arguments:
# - spec:
#   full validated simulation specification
# - state:
#   person state object from initialize_person_state()
# - history:
#   optional history state; if NULL, an empty history state is created
# - memory_model:
#   either "short" or "long"
# - recent_window_years:
#   width of the recent-history window used for long-memory summaries
# - append_history:
#   whether to append the current visit's true values to the history after the
#   update is complete
#
# Default append-history behaviour:
# - if append_history is NULL, it defaults to TRUE for the long-memory model
# - and FALSE for the short-memory model
#
# Returned fields:
# - state
# - history
# - history_features
# - alcohol
# - adiposity
# - insulin
# - observations
# - current
update_person_state <- function(spec,
                                state,
                                history = NULL,
                                memory_model = c("short", "long"),
                                recent_window_years = 2,
                                append_history = NULL) {
  .state_update_validate_state(state)
  
  memory_model <- match.arg(memory_model)
  history <- .state_update_get_history(history)
  
  if (is.null(append_history)) {
    append_history <- identical(memory_model, "long")
  }
  
  if (!is.logical(append_history) || length(append_history) != 1 || is.na(append_history)) {
    stop("`append_history` must be NULL or a single TRUE/FALSE value.", call. = FALSE)
  }
  
  # Compute history features using only information available before the current
  # visit update.
  #
  # This preserves the logic of the current bridge simulator, where the current
  # visit's values do not influence the history summaries used for that same
  # visit's biological transition models.
  history_features <- compute_state_history_features(
    spec = spec,
    state = state,
    history = history,
    recent_window_years = recent_window_years
  )
  
  # Sample current alcohol exposure.
  alcohol_out <- sample_alcohol_exposure(
    spec = spec,
    memory_model = memory_model,
    age = state$age,
    sex = state$sex,
    ses = state$ses,
    education = state$education,
    ethnicity = state$ethnicity,
    geography = state$geography,
    cal_time = state$cal_time,
    latent_metabolic = state$latent_metabolic,
    prev_score = state$prev_alcohol_score,
    history_features = history_features,
    disease = state$disease
  )
  
  # Update adiposity and latent metabolic state.
  adiposity_out <- update_adiposity_exposure(
    spec = spec,
    memory_model = memory_model,
    cal_time = state$cal_time,
    age = state$age,
    sex = state$sex,
    ses = state$ses,
    education = state$education,
    ethnicity = state$ethnicity,
    geography = state$geography,
    latent_metabolic = state$latent_metabolic,
    alcohol_score = alcohol_out$score,
    fh_diabetes = list_numeric_or_default(state$family_history, "fh_diabetes", 0),
    prev_visceral = state$prev_visceral,
    history_features = history_features,
    disease = state$disease
  )
  
  # Update insulin using the newly updated latent metabolic state and the
  # current true adiposity value.
  insulin_out <- update_insulin_exposure(
    spec = spec,
    memory_model = memory_model,
    prev_insulin = state$prev_insulin,
    visceral_true = adiposity_out$visceral_true,
    latent_metabolic = adiposity_out$latent_metabolic,
    alcohol_score = alcohol_out$score,
    fh_diabetes = list_numeric_or_default(state$family_history, "fh_diabetes", 0),
    history_features = history_features
  )
  
  # Generate observed measurements from the current true values.
  observation_context <- .state_update_build_observation_context(state)
  
  observation_out <- generate_current_observations(
    spec = spec,
    visceral_true = adiposity_out$visceral_true,
    insulin_true = insulin_out$insulin_true,
    context = observation_context
  )
  
  # Build a compact current-visit output object for downstream simulation and
  # output-building code.
  current_out <- .state_update_build_current_output(
    memory_model = memory_model,
    history_features = history_features,
    alcohol_out = alcohol_out,
    adiposity_out = adiposity_out,
    insulin_out = insulin_out,
    observation_out = observation_out
  )
  
  # Update the persistent state carried into the next visit.
  #
  # Age, calendar time, and visit index are intentionally not advanced here.
  state$latent_metabolic <- adiposity_out$latent_metabolic
  state$prev_alcohol_score <- alcohol_out$score
  state$prev_visceral <- adiposity_out$visceral_true
  state$prev_log_insulin <- insulin_out$log_insulin_true
  state$prev_insulin <- insulin_out$insulin_true
  state$current <- current_out
  
  # Optionally append the current visit's true values to the history store so
  # they can influence later visits.
  if (isTRUE(append_history)) {
    history <- append_history_state(
      history = history,
      cal_time = state$cal_time,
      alcohol_score = alcohol_out$score,
      alcohol_units = alcohol_out$units,
      visceral_true = adiposity_out$visceral_true,
      insulin_true = insulin_out$insulin_true
    )
  }
  
  list(
    state = state,
    history = history,
    history_features = history_features,
    alcohol = alcohol_out,
    adiposity = adiposity_out,
    insulin = insulin_out,
    observations = observation_out,
    current = current_out
  )
}

# Convenience wrapper for short-memory state updates.
update_person_state_short <- function(spec,
                                      state,
                                      history = NULL,
                                      recent_window_years = 2,
                                      append_history = NULL) {
  update_person_state(
    spec = spec,
    state = state,
    history = history,
    memory_model = "short",
    recent_window_years = recent_window_years,
    append_history = append_history
  )
}

# Convenience wrapper for long-memory state updates.
update_person_state_long <- function(spec,
                                     state,
                                     history = NULL,
                                     recent_window_years = 2,
                                     append_history = NULL) {
  update_person_state(
    spec = spec,
    state = state,
    history = history,
    memory_model = "long",
    recent_window_years = recent_window_years,
    append_history = append_history
  )
}
