# Alcohol exposure module for the tumour simulator.
#
# This script contains alcohol-specific logic used by the simulator, including:
# - alcohol state metadata helpers
# - rule-based target probability resolution
# - alcohol-units sampling
# - short-memory alcohol-state transitions
# - long-memory alcohol-state transitions
#
# Main user-facing functions:
# - get_alcohol_states()
# - get_alcohol_score_map()
# - alcohol_state_to_score()
# - resolve_alcohol_target_probs()
# - sample_alcohol_units()
# - sample_alcohol_exposure_short()
# - sample_alcohol_exposure_long()
# - sample_alcohol_exposure()
#
# Current design notes:
# - this module is compatible with the current colorectal bridge model
# - it uses resolve_rule() for population target probabilities
# - it accepts externally computed history summaries for the long-memory model
# - it returns a compact list containing:
#   - state
#   - score
#   - units
#   - probs
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()
#   - is_scalar_string()
#   - as_character_vector()
#   - as_named_numeric()
#   - softmax()
#   - list_numeric_or_default()
# - from utils_sampling.R:
#   - normalize_named_probs()
#   - sample_from_named_probs()
#   - rnorm_trunc()
# - from config_accessors.R:
#   - get_alcohol_spec()
# - from rule_resolver.R:
#   - resolve_rule()

# Return the configured alcohol states from the spec.
#
# The returned order is the canonical state order used throughout this module.
get_alcohol_states <- function(spec) {
  alcohol_spec <- get_alcohol_spec(spec)
  states <- as_character_vector(alcohol_spec$states)
  
  if (is.null(states) || length(states) == 0) {
    stop("Alcohol states are not defined in the spec.", call. = FALSE)
  }
  
  if (anyDuplicated(states)) {
    stop("Alcohol states must be unique.", call. = FALSE)
  }
  
  states
}

# Return the alcohol score map as a named numeric vector.
#
# The names must correspond exactly to the configured alcohol states.
get_alcohol_score_map <- function(spec) {
  alcohol_spec <- get_alcohol_spec(spec)
  score_map <- as_named_numeric(alcohol_spec$score_map)
  
  if (is.null(score_map) || length(score_map) == 0) {
    stop("Alcohol score map is not defined in the spec.", call. = FALSE)
  }
  
  states <- get_alcohol_states(spec)
  
  if (!setequal(names(score_map), states) || length(score_map) != length(states)) {
    stop(
      "Alcohol score map names must match the configured alcohol states exactly.",
      call. = FALSE
    )
  }
  
  score_map[states]
}

# Convert an alcohol state label to its numeric score.
alcohol_state_to_score <- function(spec, state) {
  if (!is_scalar_string(state)) {
    stop("`state` must be a single non-empty string.", call. = FALSE)
  }
  
  score_map <- get_alcohol_score_map(spec)
  
  if (!(state %in% names(score_map))) {
    stop("Unknown alcohol state '", state, "'.", call. = FALSE)
  }
  
  unname(score_map[[state]])
}

# Safely extract and order a named probability vector for alcohol states.
#
# The resulting vector is reordered to the configured alcohol-state order and
# normalised so that probabilities sum to 1.
.exposure_alcohol_order_probs <- function(prob_map,
                                          states,
                                          context = "alcohol probabilities") {
  probs <- as_named_numeric(prob_map)
  
  if (is.null(probs) || length(probs) == 0) {
    stop(context, " must be a non-empty named numeric object.", call. = FALSE)
  }
  
  if (!all(states %in% names(probs))) {
    stop(
      context, " do not match the configured alcohol states.",
      call. = FALSE
    )
  }
  
  probs <- probs[states]
  
  normalize_named_probs(
    prob_map = probs,
    allow_zero_sum = FALSE,
    context = context
  )
}

# Resolve the target population alcohol-state probabilities from rules.
#
# This function uses the rule resolver to obtain the baseline alcohol-state
# probabilities for the requested subgroup and calendar time.
#
# The returned vector is reordered to the configured alcohol-state order and
# normalised.
resolve_alcohol_target_probs <- function(spec,
                                         cal_time,
                                         age = NULL,
                                         sex = NULL,
                                         ses = NULL,
                                         education = NULL,
                                         ethnicity = NULL,
                                         geography = NULL,
                                         disease = NULL) {
  if (!is_scalar_number(cal_time)) {
    stop("`cal_time` must be a numeric scalar.", call. = FALSE)
  }
  
  states <- get_alcohol_states(spec)
  
  probs <- resolve_rule(
    spec = spec,
    target = "alcohol.state_probs",
    year = cal_time,
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    disease = disease
  )
  
  .exposure_alcohol_order_probs(
    prob_map = probs,
    states = states,
    context = "Resolved alcohol target probabilities"
  )
}

# Sample alcohol units for a given alcohol state.
#
# Supported unit-model distributions are:
# - point_mass
# - truncated_normal
#
# For the long-memory model, optional extra mean terms can depend on:
# - recent_mean_score
# - haz_recent_prop
sample_alcohol_units <- function(spec,
                                 state,
                                 memory_model = c("short", "long"),
                                 recent_mean_score = 0,
                                 haz_recent_prop = 0) {
  memory_model <- match.arg(memory_model)
  
  if (!is_scalar_string(state)) {
    stop("`state` must be a single non-empty string.", call. = FALSE)
  }
  
  alcohol_spec <- get_alcohol_spec(spec)
  units_model <- alcohol_spec$units_model[[state]]
  
  if (!is.list(units_model)) {
    stop("No alcohol units model found for state '", state, "'.", call. = FALSE)
  }
  
  dist <- units_model$distribution %||% NA_character_
  
  if (identical(dist, "point_mass")) {
    value <- units_model$value
    
    if (!is_scalar_number(value)) {
      stop("Point-mass alcohol units model requires numeric `value`.", call. = FALSE)
    }
    
    return(as.numeric(value))
  }
  
  if (identical(dist, "truncated_normal")) {
    mean_value <- units_model$mean
    sd_value <- units_model$sd
    min_value <- units_model$min
    max_value <- units_model$max
    
    if (!all(vapply(c(mean_value, sd_value, min_value, max_value),
                    is_scalar_number,
                    logical(1)))) {
      stop(
        "Truncated-normal alcohol units model requires numeric mean, sd, min, and max.",
        call. = FALSE
      )
    }
    
    if (identical(memory_model, "long")) {
      mean_value <- mean_value +
        (units_model$long_recent_mean_coef %||% 0) * recent_mean_score +
        (units_model$long_hazard_recent_prop_coef %||% 0) * haz_recent_prop
    }
    
    return(round(
      rnorm_trunc(
        n = 1,
        mean = mean_value,
        sd = sd_value,
        min = min_value,
        max = max_value
      ),
      1
    ))
  }
  
  stop(
    "Unsupported alcohol units distribution for state '", state, "': ", dist,
    call. = FALSE
  )
}

# Sample an alcohol exposure state under the short-memory model.
#
# Current bridge-model behaviour:
# - begin from the rule-resolved calendar-time target probabilities
# - add persistence from the previous alcohol score
# - allow the hazardous state to depend weakly on latent metabolic state
#
# Returned fields:
# - state
# - score
# - units
# - probs
sample_alcohol_exposure_short <- function(spec,
                                          age,
                                          sex,
                                          ses,
                                          education = NULL,
                                          ethnicity = NULL,
                                          geography = NULL,
                                          cal_time,
                                          latent_metabolic = 0,
                                          prev_score = 0,
                                          disease = NULL) {
  if (!is_scalar_number(cal_time)) {
    stop("`cal_time` must be a numeric scalar.", call. = FALSE)
  }
  
  if (!is_scalar_number(latent_metabolic)) {
    stop("`latent_metabolic` must be a numeric scalar.", call. = FALSE)
  }
  
  if (!is_scalar_number(prev_score)) {
    stop("`prev_score` must be a numeric scalar.", call. = FALSE)
  }
  
  states <- get_alcohol_states(spec)
  score_map <- get_alcohol_score_map(spec)
  alcohol_spec <- get_alcohol_spec(spec)
  
  base_p <- resolve_alcohol_target_probs(
    spec = spec,
    cal_time = cal_time,
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    disease = disease
  )
  
  short_spec <- alcohol_spec$memory_models$short
  
  if (!is.list(short_spec)) {
    stop("Short-memory alcohol model specification is missing.", call. = FALSE)
  }
  
  persistence_map <- as_named_numeric(short_spec$persistence_logits)
  
  if (is.null(persistence_map)) {
    stop("Short-memory alcohol model requires `persistence_logits`.", call. = FALSE)
  }
  
  lp <- log(base_p)
  
  for (st in states) {
    state_score <- unname(score_map[[st]])
    lp[st] <- lp[st] + (persistence_map[[st]] %||% 0) * as.numeric(prev_score == state_score)
  }
  
  if ("hazardous_drinker" %in% states) {
    lp["hazardous_drinker"] <- lp["hazardous_drinker"] +
      (short_spec$latent_metabolic_hazardous_coef %||% 0) * latent_metabolic
  }
  
  p <- softmax(lp)
  names(p) <- states
  
  p <- normalize_named_probs(
    prob_map = p,
    allow_zero_sum = FALSE,
    context = "Short-memory alcohol-state probabilities"
  )
  
  state <- sample_from_named_probs(
    prob_map = p,
    context = "Short-memory alcohol-state probabilities"
  )
  
  units <- sample_alcohol_units(
    spec = spec,
    state = state,
    memory_model = "short"
  )
  
  list(
    state = state,
    score = alcohol_state_to_score(spec, state),
    units = units,
    probs = p
  )
}

# Sample an alcohol exposure state under the long-memory model.
#
# Current bridge-model behaviour:
# - begin from the rule-resolved calendar-time target probabilities
# - add dependence on recent alcohol history
# - allow hazardous drinking to depend on recent hazardous proportion and
#   latent metabolic state
#
# The history_features list is expected to contain, where available:
# - recent_mean_score
# - cum_mean_score
# - haz_recent_prop
#
# Returned fields:
# - state
# - score
# - units
# - probs
# - recent_mean_score
# - cum_mean_score
# - haz_recent_prop
sample_alcohol_exposure_long <- function(spec,
                                         age,
                                         sex,
                                         ses,
                                         education = NULL,
                                         ethnicity = NULL,
                                         geography = NULL,
                                         cal_time,
                                         latent_metabolic = 0,
                                         history_features = NULL,
                                         disease = NULL) {
  if (!is_scalar_number(cal_time)) {
    stop("`cal_time` must be a numeric scalar.", call. = FALSE)
  }
  
  if (!is_scalar_number(latent_metabolic)) {
    stop("`latent_metabolic` must be a numeric scalar.", call. = FALSE)
  }
  
  states <- get_alcohol_states(spec)
  alcohol_spec <- get_alcohol_spec(spec)
  
  base_p <- resolve_alcohol_target_probs(
    spec = spec,
    cal_time = cal_time,
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    disease = disease
  )
  
  long_spec <- alcohol_spec$memory_models$long
  
  if (!is.list(long_spec)) {
    stop("Long-memory alcohol model specification is missing.", call. = FALSE)
  }
  
  recent_mean_score <- list_numeric_or_default(
    x = history_features,
    name = "recent_mean_score",
    fallback = 0
  )
  
  cum_mean_score <- list_numeric_or_default(
    x = history_features,
    name = "cum_mean_score",
    fallback = 0
  )
  
  haz_recent_prop <- list_numeric_or_default(
    x = history_features,
    name = "haz_recent_prop",
    fallback = 0
  )
  
  # The current bridge model does not use cum_mean_score directly in the alcohol
  # transition itself, but it is extracted here so the long-memory interface is
  # complete and easy to extend later.
  lp <- log(base_p)
  
  if ("non_drinker" %in% states) {
    lp["non_drinker"] <- lp["non_drinker"] +
      (long_spec$non_if_recent_mean_lt_0_25 %||% 0) * as.numeric(recent_mean_score < 0.25)
  }
  
  if ("moderate_drinker" %in% states) {
    lp["moderate_drinker"] <- lp["moderate_drinker"] +
      (long_spec$moderate_if_recent_mean_between_0_75_and_1_5 %||% 0) *
      as.numeric(recent_mean_score >= 0.75 && recent_mean_score < 1.5)
  }
  
  if ("hazardous_drinker" %in% states) {
    lp["hazardous_drinker"] <- lp["hazardous_drinker"] +
      (long_spec$hazardous_recent_mean_coef %||% 0) * recent_mean_score +
      (long_spec$hazardous_recent_prop_coef %||% 0) * haz_recent_prop +
      (long_spec$latent_metabolic_hazardous_coef %||% 0) * latent_metabolic
  }
  
  p <- softmax(lp)
  names(p) <- states
  
  p <- normalize_named_probs(
    prob_map = p,
    allow_zero_sum = FALSE,
    context = "Long-memory alcohol-state probabilities"
  )
  
  state <- sample_from_named_probs(
    prob_map = p,
    context = "Long-memory alcohol-state probabilities"
  )
  
  units <- sample_alcohol_units(
    spec = spec,
    state = state,
    memory_model = "long",
    recent_mean_score = recent_mean_score,
    haz_recent_prop = haz_recent_prop
  )
  
  list(
    state = state,
    score = alcohol_state_to_score(spec, state),
    units = units,
    probs = p,
    recent_mean_score = recent_mean_score,
    cum_mean_score = cum_mean_score,
    haz_recent_prop = haz_recent_prop
  )
}

# Sample an alcohol exposure state under the requested memory model.
#
# This is the main wrapper function used by the simulator.
#
# For memory_model = "short", the key extra input is:
# - prev_score
#
# For memory_model = "long", the key extra input is:
# - history_features
#
# Returned fields:
# - state
# - score
# - units
# - probs
#
# and, for the long-memory model, also:
# - recent_mean_score
# - cum_mean_score
# - haz_recent_prop
sample_alcohol_exposure <- function(spec,
                                    memory_model = c("short", "long"),
                                    age,
                                    sex,
                                    ses,
                                    education = NULL,
                                    ethnicity = NULL,
                                    geography = NULL,
                                    cal_time,
                                    latent_metabolic = 0,
                                    prev_score = 0,
                                    history_features = NULL,
                                    disease = NULL) {
  memory_model <- match.arg(memory_model)
  
  if (identical(memory_model, "short")) {
    return(
      sample_alcohol_exposure_short(
        spec = spec,
        age = age,
        sex = sex,
        ses = ses,
        education = education,
        ethnicity = ethnicity,
        geography = geography,
        cal_time = cal_time,
        latent_metabolic = latent_metabolic,
        prev_score = prev_score,
        disease = disease
      )
    )
  }
  
  sample_alcohol_exposure_long(
    spec = spec,
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    cal_time = cal_time,
    latent_metabolic = latent_metabolic,
    history_features = history_features,
    disease = disease
  )
}
