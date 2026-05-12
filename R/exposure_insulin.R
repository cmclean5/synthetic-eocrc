# Insulin exposure module for the tumour simulator.
#
# This script contains insulin-specific logic used by the simulator, including:
# - initial insulin-state generation
# - short-memory insulin updates
# - long-memory insulin updates
# - high-insulin threshold helpers
#
# Main user-facing functions:
# - get_insulin_high_log_threshold()
# - get_insulin_variable_name()
# - initialize_insulin_state()
# - update_insulin_exposure_short()
# - update_insulin_exposure_long()
# - update_insulin_exposure()
#
# Current design notes:
# - this module is compatible with the current colorectal bridge model
# - insulin is represented internally on both:
#   - the log scale
#   - the natural scale
# - the visceral-adiposity centring reference is now read from the adiposity
#   config rather than being hard-coded in this module
# - history-based long-memory summaries are passed in through history_features
# - observed measurement generation is intentionally not handled here and is
#   intended to live in observation_models.R later
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()
#   - is_scalar_string()
#   - list_numeric_or_default()
# - from utils_sampling.R:
#   - rnorm_trunc()
# - from config_accessors.R:
#   - get_adiposity_reference_value()
#   - get_insulin_spec()

# Return the configured high-insulin threshold on the log scale.
#
# This threshold is used in the current bridge model when computing recent
# proportions of high insulin values from history.
get_insulin_high_log_threshold <- function(spec) {
  insulin_spec <- get_insulin_spec(spec)
  threshold <- insulin_spec$high_log_threshold
  
  if (!is_scalar_number(threshold)) {
    stop("Insulin high_log_threshold must be a numeric scalar.", call. = FALSE)
  }
  
  threshold
}

# Return the configured insulin variable name.
#
# This is mainly a convenience helper for later modular output building.
get_insulin_variable_name <- function(spec) {
  insulin_spec <- get_insulin_spec(spec)
  variable_name <- insulin_spec$variable %||% NA_character_
  
  if (!is_scalar_string(variable_name)) {
    stop("Insulin variable name must be a non-empty string.", call. = FALSE)
  }
  
  variable_name
}

# Initialise the baseline insulin state.
#
# The current bridge-model baseline insulin initialisation depends on:
# - current visceral adiposity
# - latent metabolic state
#
# The initial-state specification is taken from:
# - spec$exposures$insulin$initial_state
#
# Returned fields:
# - prev_log_insulin
# - prev_insulin
# - high_log_threshold
# - high_insulin_indicator
initialize_insulin_state <- function(spec,
                                     prev_visceral,
                                     latent_metabolic = 0) {
  insulin_spec <- get_insulin_spec(spec)
  init <- insulin_spec$initial_state
  
  if (!is.list(init)) {
    stop("Insulin initial-state specification is missing.", call. = FALSE)
  }
  
  if (!is_scalar_number(prev_visceral)) {
    stop("`prev_visceral` must be a numeric scalar.", call. = FALSE)
  }
  
  required_fields <- c(
    "base_mean_log",
    "visceral_coef",
    "latent_metabolic_coef",
    "sd",
    "min_log",
    "max_log"
  )
  
  required_ok <- vapply(init[required_fields], is_scalar_number, logical(1))
  
  if (!all(required_ok)) {
    stop(
      "Insulin initial-state specification requires numeric values for: ",
      paste(required_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (init$sd <= 0) {
    stop("Insulin initial-state sd must be > 0.", call. = FALSE)
  }
  
  if (init$max_log <= init$min_log) {
    stop("Insulin initial-state max_log must be greater than min_log.", call. = FALSE)
  }
  
  visceral_ref <- get_adiposity_reference_value(spec)
  
  prev_log_insulin <- rnorm_trunc(
    n = 1,
    mean = init$base_mean_log +
      init$visceral_coef * (prev_visceral - visceral_ref) +
      init$latent_metabolic_coef * latent_metabolic,
    sd = init$sd,
    min = init$min_log,
    max = init$max_log
  )
  
  prev_insulin <- exp(prev_log_insulin)
  high_log_threshold <- get_insulin_high_log_threshold(spec)
  high_insulin_indicator <- as.integer(prev_log_insulin >= high_log_threshold)
  
  list(
    prev_log_insulin = prev_log_insulin,
    prev_insulin = prev_insulin,
    high_log_threshold = high_log_threshold,
    high_insulin_indicator = high_insulin_indicator
  )
}

# Update insulin under the short-memory model.
#
# Current bridge-model behaviour:
# - autoregressive persistence on the log scale
# - current visceral effect
# - current alcohol-score effect
# - latent metabolic effect
# - family-history-of-diabetes effect
# - truncated-normal noise
#
# Returned fields:
# - mean_log_insulin
# - log_insulin_true
# - insulin_true
# - high_log_threshold
# - high_insulin_indicator
update_insulin_exposure_short <- function(spec,
                                          prev_insulin,
                                          visceral_true,
                                          latent_metabolic,
                                          alcohol_score,
                                          fh_diabetes = 0) {
  insulin_spec <- get_insulin_spec(spec)
  dyn <- insulin_spec$dynamics$short
  
  if (!is.list(dyn)) {
    stop("Short-memory insulin dynamics are missing.", call. = FALSE)
  }
  
  required_fields <- c(
    "base_mean_log",
    "prev_log_coef",
    "visceral_coef",
    "alcohol_score_coef",
    "latent_metabolic_coef",
    "fh_diabetes_coef",
    "sd",
    "min_log",
    "max_log"
  )
  
  required_ok <- vapply(dyn[required_fields], is_scalar_number, logical(1))
  
  if (!all(required_ok)) {
    stop(
      "Short-memory insulin dynamics require numeric values for: ",
      paste(required_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!all(vapply(c(prev_insulin, visceral_true, latent_metabolic, alcohol_score, fh_diabetes),
                  is_scalar_number,
                  logical(1)))) {
    stop(
      "Short-memory insulin update requires numeric prev_insulin, visceral_true, ",
      "latent_metabolic, alcohol_score, and fh_diabetes.",
      call. = FALSE
    )
  }
  
  if (prev_insulin <= 0) {
    stop("`prev_insulin` must be > 0.", call. = FALSE)
  }
  
  if (dyn$sd <= 0) {
    stop("Short-memory insulin sd must be > 0.", call. = FALSE)
  }
  
  if (dyn$max_log <= dyn$min_log) {
    stop("Short-memory insulin max_log must be greater than min_log.", call. = FALSE)
  }
  
  visceral_ref <- get_adiposity_reference_value(spec)
  
  mean_log_insulin <- dyn$base_mean_log +
    dyn$prev_log_coef * (log(prev_insulin) - dyn$base_mean_log) +
    dyn$visceral_coef * (visceral_true - visceral_ref) +
    dyn$alcohol_score_coef * alcohol_score +
    dyn$latent_metabolic_coef * latent_metabolic +
    dyn$fh_diabetes_coef * fh_diabetes
  
  log_insulin_true <- rnorm_trunc(
    n = 1,
    mean = mean_log_insulin,
    sd = dyn$sd,
    min = dyn$min_log,
    max = dyn$max_log
  )
  
  insulin_true <- exp(log_insulin_true)
  high_log_threshold <- get_insulin_high_log_threshold(spec)
  high_insulin_indicator <- as.integer(log_insulin_true >= high_log_threshold)
  
  list(
    mean_log_insulin = mean_log_insulin,
    log_insulin_true = log_insulin_true,
    insulin_true = insulin_true,
    high_log_threshold = high_log_threshold,
    high_insulin_indicator = high_insulin_indicator
  )
}

# Update insulin under the long-memory model.
#
# Current bridge-model behaviour:
# - autoregressive persistence on the log scale
# - current visceral effect
# - recent visceral effect
# - cumulative visceral effect
# - current alcohol-score effect
# - recent alcohol-history effect
# - latent metabolic effect
# - family-history-of-diabetes effect
# - truncated-normal noise
#
# The history_features list is expected to contain, where available:
# - recent_visceral
# - cum_visceral
# - recent_mean_score
#
# Returned fields:
# - mean_log_insulin
# - log_insulin_true
# - insulin_true
# - high_log_threshold
# - high_insulin_indicator
# - recent_visceral
# - cum_visceral
# - recent_mean_score
update_insulin_exposure_long <- function(spec,
                                         prev_insulin,
                                         visceral_true,
                                         latent_metabolic,
                                         alcohol_score,
                                         fh_diabetes = 0,
                                         history_features = NULL) {
  insulin_spec <- get_insulin_spec(spec)
  dyn <- insulin_spec$dynamics$long
  
  if (!is.list(dyn)) {
    stop("Long-memory insulin dynamics are missing.", call. = FALSE)
  }
  
  required_fields <- c(
    "base_mean_log",
    "prev_log_coef",
    "visceral_coef",
    "recent_visceral_coef",
    "cum_visceral_coef",
    "alcohol_score_coef",
    "recent_mean_score_coef",
    "latent_metabolic_coef",
    "fh_diabetes_coef",
    "sd",
    "min_log",
    "max_log"
  )
  
  required_ok <- vapply(dyn[required_fields], is_scalar_number, logical(1))
  
  if (!all(required_ok)) {
    stop(
      "Long-memory insulin dynamics require numeric values for: ",
      paste(required_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (!all(vapply(c(prev_insulin, visceral_true, latent_metabolic, alcohol_score, fh_diabetes),
                  is_scalar_number,
                  logical(1)))) {
    stop(
      "Long-memory insulin update requires numeric prev_insulin, visceral_true, ",
      "latent_metabolic, alcohol_score, and fh_diabetes.",
      call. = FALSE
    )
  }
  
  if (prev_insulin <= 0) {
    stop("`prev_insulin` must be > 0.", call. = FALSE)
  }
  
  if (dyn$sd <= 0) {
    stop("Long-memory insulin sd must be > 0.", call. = FALSE)
  }
  
  if (dyn$max_log <= dyn$min_log) {
    stop("Long-memory insulin max_log must be greater than min_log.", call. = FALSE)
  }
  
  recent_visceral <- list_numeric_or_default(
    x = history_features,
    name = "recent_visceral",
    fallback = visceral_true
  )
  
  cum_visceral <- list_numeric_or_default(
    x = history_features,
    name = "cum_visceral",
    fallback = visceral_true
  )
  
  recent_mean_score <- list_numeric_or_default(
    x = history_features,
    name = "recent_mean_score",
    fallback = 0
  )
  
  visceral_ref <- get_adiposity_reference_value(spec)
  
  mean_log_insulin <- dyn$base_mean_log +
    dyn$prev_log_coef * (log(prev_insulin) - dyn$base_mean_log) +
    dyn$visceral_coef * (visceral_true - visceral_ref) +
    dyn$recent_visceral_coef * (recent_visceral - visceral_ref) +
    dyn$cum_visceral_coef * (cum_visceral - visceral_ref) +
    dyn$alcohol_score_coef * alcohol_score +
    dyn$recent_mean_score_coef * recent_mean_score +
    dyn$latent_metabolic_coef * latent_metabolic +
    dyn$fh_diabetes_coef * fh_diabetes
  
  log_insulin_true <- rnorm_trunc(
    n = 1,
    mean = mean_log_insulin,
    sd = dyn$sd,
    min = dyn$min_log,
    max = dyn$max_log
  )
  
  insulin_true <- exp(log_insulin_true)
  high_log_threshold <- get_insulin_high_log_threshold(spec)
  high_insulin_indicator <- as.integer(log_insulin_true >= high_log_threshold)
  
  list(
    mean_log_insulin = mean_log_insulin,
    log_insulin_true = log_insulin_true,
    insulin_true = insulin_true,
    high_log_threshold = high_log_threshold,
    high_insulin_indicator = high_insulin_indicator,
    recent_visceral = recent_visceral,
    cum_visceral = cum_visceral,
    recent_mean_score = recent_mean_score
  )
}

# Update insulin under the requested memory model.
#
# This is the main wrapper function used by the simulator.
#
# For memory_model = "short", the key inputs are:
# - prev_insulin
# - current visceral_true
# - latent_metabolic
# - current alcohol_score
#
# For memory_model = "long", the key additional input is:
# - history_features
#
# Returned fields:
# - mean_log_insulin
# - log_insulin_true
# - insulin_true
# - high_log_threshold
# - high_insulin_indicator
#
# and, for the long-memory model, also:
# - recent_visceral
# - cum_visceral
# - recent_mean_score
update_insulin_exposure <- function(spec,
                                    memory_model = c("short", "long"),
                                    prev_insulin,
                                    visceral_true,
                                    latent_metabolic,
                                    alcohol_score,
                                    fh_diabetes = 0,
                                    history_features = NULL) {
  memory_model <- match.arg(memory_model)
  
  if (identical(memory_model, "short")) {
    return(
      update_insulin_exposure_short(
        spec = spec,
        prev_insulin = prev_insulin,
        visceral_true = visceral_true,
        latent_metabolic = latent_metabolic,
        alcohol_score = alcohol_score,
        fh_diabetes = fh_diabetes
      )
    )
  }
  
  update_insulin_exposure_long(
    spec = spec,
    prev_insulin = prev_insulin,
    visceral_true = visceral_true,
    latent_metabolic = latent_metabolic,
    alcohol_score = alcohol_score,
    fh_diabetes = fh_diabetes,
    history_features = history_features
  )
}
