# Observation and measurement models for the tumour simulator.
#
# This script contains logic for converting true latent or exposure values into
# observed values subject to:
# - measurement probabilities
# - potential missingness
# - optional rounding for recorded measurements
#
# Main user-facing functions:
# - build_observation_context()
# - compute_observation_probability()
# - compute_adiposity_measurement_probability()
# - compute_insulin_measurement_probability()
# - generate_observed_value()
# - generate_observed_adiposity()
# - generate_observed_insulin()
# - generate_current_observations()
#
# Current design notes:
# - this module is compatible with the current colorectal bridge model
# - measurement probabilities are currently modelled using Bernoulli logit models
# - true values are not altered apart from optional rounding when observed
# - if a measurement is not observed, NA_real_ is returned
#
# Future design direction:
# - this module can later be extended to include:
#   - measurement error
#   - interval censoring
#   - lower limits of detection
#   - assay-specific observation models
#   - outcome-dependent observation processes
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()
#   - linear_predictor()
#   - invlogit()
# - from config_accessors.R:
#   - get_adiposity_spec()
#   - get_insulin_spec()

# Build the standard observation-model context used by the current simulator.
#
# This context currently includes:
# - sex
# - ses
# - education
# - ethnicity
# - geography
# - fh_diabetes
#
# Additional fields can be added later without changing calling code elsewhere.
build_observation_context <- function(sex = NULL,
                                      ses = NULL,
                                      education = NULL,
                                      ethnicity = NULL,
                                      geography = NULL,
                                      fh_diabetes = NULL) {
  list(
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    fh_diabetes = fh_diabetes
  )
}

# Return the adiposity observation-model specification block.
.get_adiposity_observation_model_spec <- function(spec) {
  adiposity_spec <- get_adiposity_spec(spec)
  model_spec <- adiposity_spec$observation_model
  
  if (!is.list(model_spec)) {
    stop(
      "Adiposity observation model specification is missing.",
      call. = FALSE
    )
  }
  
  model_spec
}

# Return the insulin observation-model specification block.
.get_insulin_observation_model_spec <- function(spec) {
  insulin_spec <- get_insulin_spec(spec)
  model_spec <- insulin_spec$observation_model
  
  if (!is.list(model_spec)) {
    stop(
      "Insulin observation model specification is missing.",
      call. = FALSE
    )
  }
  
  model_spec
}

# Compute an observation probability from a Bernoulli logit model specification.
#
# The model specification is expected to contain:
# - intercept
# - coefficients
#
# Returned value:
# - scalar probability in [0, 1]
compute_observation_probability <- function(model_spec,
                                            context = list()) {
  if (!is.list(model_spec)) {
    stop("Observation model specification must be a list.", call. = FALSE)
  }
  
  lp <- linear_predictor(
    intercept = model_spec$intercept %||% 0,
    coefficients = model_spec$coefficients %||% list(),
    context = context
  )
  
  p <- invlogit(lp)
  pmin(pmax(p, 0), 1)
}

# Compute the probability that adiposity is measured at the current visit.
#
# The current bridge model uses the adiposity observation model configured in:
# - spec$exposures$adiposity$observation_model
compute_adiposity_measurement_probability <- function(spec,
                                                      sex = NULL,
                                                      ses = NULL,
                                                      education = NULL,
                                                      ethnicity = NULL,
                                                      geography = NULL,
                                                      fh_diabetes = NULL,
                                                      context = NULL) {
  model_spec <- .get_adiposity_observation_model_spec(spec)
  
  if (is.null(context)) {
    context <- build_observation_context(
      sex = sex,
      ses = ses,
      education = education,
      ethnicity = ethnicity,
      geography = geography,
      fh_diabetes = fh_diabetes
    )
  }
  
  compute_observation_probability(
    model_spec = model_spec,
    context = context
  )
}

# Compute the probability that insulin is measured at the current visit.
#
# The current bridge model uses the insulin observation model configured in:
# - spec$exposures$insulin$observation_model
compute_insulin_measurement_probability <- function(spec,
                                                    sex = NULL,
                                                    ses = NULL,
                                                    education = NULL,
                                                    ethnicity = NULL,
                                                    geography = NULL,
                                                    fh_diabetes = NULL,
                                                    context = NULL) {
  model_spec <- .get_insulin_observation_model_spec(spec)
  
  if (is.null(context)) {
    context <- build_observation_context(
      sex = sex,
      ses = ses,
      education = education,
      ethnicity = ethnicity,
      geography = geography,
      fh_diabetes = fh_diabetes
    )
  }
  
  compute_observation_probability(
    model_spec = model_spec,
    context = context
  )
}

# Generate an observed measurement from a true value and observation probability.
#
# Behaviour:
# - sample a Bernoulli indicator with probability p_observe
# - if observed, return the true value with optional rounding
# - if not observed, return NA_real_
#
# Arguments:
# - true_value:
#   scalar numeric true value
# - p_observe:
#   observation probability in [0, 1]
# - digits:
#   optional number of decimal places to round to when observed
#
# Returned fields:
# - observed
# - observed_indicator
# - p_observe
generate_observed_value <- function(true_value,
                                    p_observe,
                                    digits = NULL) {
  if (!is_scalar_number(true_value)) {
    stop("`true_value` must be a numeric scalar.", call. = FALSE)
  }
  
  if (!is_scalar_number(p_observe)) {
    stop("`p_observe` must be a numeric scalar.", call. = FALSE)
  }
  
  if (p_observe < 0 || p_observe > 1) {
    stop("`p_observe` must lie in [0, 1].", call. = FALSE)
  }
  
  if (!is.null(digits) && (!is_scalar_number(digits) || digits < 0)) {
    stop("`digits` must be NULL or a non-negative numeric scalar.", call. = FALSE)
  }
  
  observed_indicator <- rbinom(1, 1, p_observe)
  
  observed_value <- if (observed_indicator == 1) {
    if (is.null(digits)) {
      true_value
    } else {
      round(true_value, digits = digits)
    }
  } else {
    NA_real_
  }
  
  list(
    observed = observed_value,
    observed_indicator = observed_indicator,
    p_observe = p_observe
  )
}

# Generate an observed adiposity value from the current adiposity true value.
#
# Returned fields:
# - observed
# - observed_indicator
# - p_observe
generate_observed_adiposity <- function(spec,
                                        visceral_true,
                                        sex = NULL,
                                        ses = NULL,
                                        education = NULL,
                                        ethnicity = NULL,
                                        geography = NULL,
                                        fh_diabetes = NULL,
                                        digits = 2,
                                        context = NULL) {
  if (is.null(context)) {
    context <- build_observation_context(
      sex = sex,
      ses = ses,
      education = education,
      ethnicity = ethnicity,
      geography = geography,
      fh_diabetes = fh_diabetes
    )
  }
  
  p_measure <- compute_adiposity_measurement_probability(
    spec = spec,
    context = context
  )
  
  generate_observed_value(
    true_value = visceral_true,
    p_observe = p_measure,
    digits = digits
  )
}

# Generate an observed insulin value from the current insulin true value.
#
# Returned fields:
# - observed
# - observed_indicator
# - p_observe
generate_observed_insulin <- function(spec,
                                      insulin_true,
                                      sex = NULL,
                                      ses = NULL,
                                      education = NULL,
                                      ethnicity = NULL,
                                      geography = NULL,
                                      fh_diabetes = NULL,
                                      digits = 2,
                                      context = NULL) {
  if (is.null(context)) {
    context <- build_observation_context(
      sex = sex,
      ses = ses,
      education = education,
      ethnicity = ethnicity,
      geography = geography,
      fh_diabetes = fh_diabetes
    )
  }
  
  p_measure <- compute_insulin_measurement_probability(
    spec = spec,
    context = context
  )
  
  generate_observed_value(
    true_value = insulin_true,
    p_observe = p_measure,
    digits = digits
  )
}

# Generate the current set of observed metabolic measurements.
#
# This is a convenience wrapper used by the simulator to obtain both:
# - adiposity observation
# - insulin observation
#
# Returned fields:
# - visceral_obs
# - visceral_observed_indicator
# - p_measure_visceral
# - insulin_obs
# - insulin_observed_indicator
# - p_measure_insulin
generate_current_observations <- function(spec,
                                          visceral_true,
                                          insulin_true,
                                          sex = NULL,
                                          ses = NULL,
                                          education = NULL,
                                          ethnicity = NULL,
                                          geography = NULL,
                                          fh_diabetes = NULL,
                                          adiposity_digits = 2,
                                          insulin_digits = 2,
                                          context = NULL) {
  if (is.null(context)) {
    context <- build_observation_context(
      sex = sex,
      ses = ses,
      education = education,
      ethnicity = ethnicity,
      geography = geography,
      fh_diabetes = fh_diabetes
    )
  }
  
  adiposity_obs <- generate_observed_adiposity(
    spec = spec,
    visceral_true = visceral_true,
    context = context,
    digits = adiposity_digits
  )
  
  insulin_obs <- generate_observed_insulin(
    spec = spec,
    insulin_true = insulin_true,
    context = context,
    digits = insulin_digits
  )
  
  list(
    visceral_obs = adiposity_obs$observed,
    visceral_observed_indicator = adiposity_obs$observed_indicator,
    p_measure_visceral = adiposity_obs$p_observe,
    insulin_obs = insulin_obs$observed,
    insulin_observed_indicator = insulin_obs$observed_indicator,
    p_measure_insulin = insulin_obs$p_observe
  )
}
