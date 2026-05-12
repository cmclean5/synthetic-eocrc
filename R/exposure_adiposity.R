# Adiposity exposure module for the tumour simulator.
#
# This script contains adiposity-specific logic used by the simulator,
# including:
# - rule-based obesity-target resolution
# - conversion from obesity probability target to continuous visceral target mean
# - initial visceral-adiposity state generation
# - latent-metabolic-state updates linked to adiposity dynamics
# - short-memory adiposity state transitions
# - long-memory adiposity state transitions
#
# Main user-facing functions:
# - resolve_obesity_target_base()
# - resolve_obesity_target()
# - get_target_visceral_mean()
# - initialize_adiposity_state()
# - update_latent_metabolic_short()
# - update_latent_metabolic_long()
# - sample_visceral_state_short()
# - sample_visceral_state_long()
# - update_adiposity_exposure()
# - update_adiposity_exposure_short()
# - update_adiposity_exposure_long()
#
# Current design notes:
# - this module is compatible with the current colorectal bridge model
# - it uses resolve_rule() for obesity probability targets
# - latent-age centring and obesity-probability clamping bounds are now read
#   from config rather than being hard-coded
# - it updates the latent metabolic state because, in the current model,
#   adiposity dynamics and latent-metabolic dynamics are closely coupled
# - observed measurement generation is intentionally not handled here and is
#   intended to live in observation_models.R later
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()
#   - invlogit()
#   - list_numeric_or_default()
# - from utils_sampling.R:
#   - rnorm_trunc()
# - from config_accessors.R:
#   - get_adiposity_spec()
#   - get_adiposity_latent_age_ref()
#   - get_adiposity_target_probability_bounds()
# - from rule_resolver.R:
#   - resolve_rule()

# Resolve the baseline obesity probability target from rules.
#
# This is the rule-based target before the configured age effect is applied.
#
# The relevant rule target is currently:
# - "adiposity.obesity_probability.base"
resolve_obesity_target_base <- function(spec,
                                        cal_time,
                                        sex,
                                        ses,
                                        education = NULL,
                                        ethnicity = NULL,
                                        geography = NULL,
                                        disease = NULL) {
  if (!is_scalar_number(cal_time)) {
    stop("`cal_time` must be a numeric scalar.", call. = FALSE)
  }
  
  resolve_rule(
    spec = spec,
    target = "adiposity.obesity_probability.base",
    year = cal_time,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    disease = disease
  )
}

# Resolve the full obesity probability target after applying the configured age
# effect.
#
# The age effect is currently taken from:
# - spec$exposures$adiposity$age_effect$age_ref
# - spec$exposures$adiposity$age_effect$logit_slope_per_10y
#
# The final probability is then clamped to the configured bounds from:
# - spec$exposures$adiposity$target_probability_bounds$min
# - spec$exposures$adiposity$target_probability_bounds$max
resolve_obesity_target <- function(spec,
                                   cal_time,
                                   age,
                                   sex,
                                   ses,
                                   education = NULL,
                                   ethnicity = NULL,
                                   geography = NULL,
                                   disease = NULL) {
  adiposity_spec <- get_adiposity_spec(spec)
  
  if (!is_scalar_number(age)) {
    stop("`age` must be a numeric scalar.", call. = FALSE)
  }
  
  base_p <- resolve_obesity_target_base(
    spec = spec,
    cal_time = cal_time,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    disease = disease
  )
  
  if (!is_scalar_number(base_p)) {
    stop("Resolved baseline obesity target must be a numeric scalar.", call. = FALSE)
  }
  
  age_effect <- adiposity_spec$age_effect
  age_ref <- age_effect$age_ref
  age_logit_slope_per_10y <- age_effect$logit_slope_per_10y
  
  if (!all(vapply(c(age_ref, age_logit_slope_per_10y), is_scalar_number, logical(1)))) {
    stop("Adiposity age effect requires numeric age_ref and logit_slope_per_10y.", call. = FALSE)
  }
  
  bounds <- get_adiposity_target_probability_bounds(spec)
  
  lp <- qlogis(base_p) + age_logit_slope_per_10y * ((age - age_ref) / 10)
  p <- invlogit(lp)
  
  pmin(pmax(p, bounds$min), bounds$max)
}

# Convert an obesity probability target into a target mean for the continuous
# visceral-adiposity variable.
#
# The current mapping assumes a normal distribution and uses:
# - a threshold corresponding to obesity
# - a configured standard deviation
#
# This reproduces the current bridge-model logic.
get_target_visceral_mean <- function(spec, obesity_prob) {
  adiposity_spec <- get_adiposity_spec(spec)
  target_mapping <- adiposity_spec$target_mapping
  
  if (!is_scalar_number(obesity_prob)) {
    stop("`obesity_prob` must be a numeric scalar.", call. = FALSE)
  }
  
  threshold <- target_mapping$threshold
  sd_value <- target_mapping$sd
  
  if (!all(vapply(c(threshold, sd_value), is_scalar_number, logical(1)))) {
    stop("Adiposity target mapping requires numeric threshold and sd.", call. = FALSE)
  }
  
  threshold + sd_value * qnorm(obesity_prob)
}

# Initialise the baseline adiposity state.
#
# This resolves the obesity target at the person's entry time and age, then
# draws the initial true visceral-adiposity value from a truncated normal
# distribution.
#
# Returned fields:
# - p_obesity_target_0
# - target_mean_visceral_0
# - prev_visceral
initialize_adiposity_state <- function(spec,
                                       cal_time,
                                       age,
                                       sex,
                                       ses,
                                       education = NULL,
                                       ethnicity = NULL,
                                       geography = NULL,
                                       latent_metabolic = 0,
                                       disease = NULL) {
  adiposity_spec <- get_adiposity_spec(spec)
  
  p_obesity_target_0 <- resolve_obesity_target(
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
  
  target_mean_visceral_0 <- get_target_visceral_mean(
    spec = spec,
    obesity_prob = p_obesity_target_0
  )
  
  adiposity_init <- adiposity_spec$initial_state
  bounds <- adiposity_spec$baseline_distribution
  visceral_sd <- adiposity_spec$visceral_sd
  
  if (!all(vapply(c(bounds$min, bounds$max, visceral_sd), is_scalar_number, logical(1)))) {
    stop(
      "Adiposity initialisation requires numeric visceral_sd and baseline bounds.",
      call. = FALSE
    )
  }
  
  prev_visceral <- rnorm_trunc(
    n = 1,
    mean = target_mean_visceral_0 +
      (adiposity_init$latent_metabolic_coef %||% 0) * latent_metabolic,
    sd = visceral_sd,
    min = bounds$min,
    max = bounds$max
  )
  
  list(
    p_obesity_target_0 = p_obesity_target_0,
    target_mean_visceral_0 = target_mean_visceral_0,
    prev_visceral = prev_visceral
  )
}

# Update the latent metabolic state under the short-memory adiposity model.
#
# Current bridge-model behaviour:
# - autoregressive persistence
# - age effect
# - current alcohol-score effect
# - family-history-of-diabetes effect
# - Gaussian innovation noise
update_latent_metabolic_short <- function(spec,
                                          age,
                                          latent_metabolic,
                                          alcohol_score,
                                          fh_diabetes = 0) {
  adiposity_spec <- get_adiposity_spec(spec)
  dyn <- adiposity_spec$dynamics$short
  
  if (!is.list(dyn)) {
    stop("Short-memory adiposity dynamics are missing.", call. = FALSE)
  }
  
  latent_age_ref <- get_adiposity_latent_age_ref(spec)
  latent_noise_sd <- dyn$latent_noise_sd
  
  if (!is_scalar_number(latent_noise_sd) || latent_noise_sd <= 0) {
    stop(
      "Short-memory adiposity dynamics require numeric latent_noise_sd > 0.",
      call. = FALSE
    )
  }
  
  (dyn$latent_ar %||% 0) * latent_metabolic +
    (dyn$latent_age_coef %||% 0) * (age - latent_age_ref) +
    (dyn$latent_alcohol_score_coef %||% 0) * alcohol_score +
    (dyn$latent_fh_diabetes_coef %||% 0) * fh_diabetes +
    rnorm(1, mean = 0, sd = latent_noise_sd)
}

# Update the latent metabolic state under the long-memory adiposity model.
#
# Current bridge-model behaviour:
# - autoregressive persistence
# - age effect
# - recent alcohol-score history effect
# - cumulative alcohol-score history effect
# - family-history-of-diabetes effect
# - Gaussian innovation noise
update_latent_metabolic_long <- function(spec,
                                         age,
                                         latent_metabolic,
                                         fh_diabetes = 0,
                                         history_features = NULL) {
  adiposity_spec <- get_adiposity_spec(spec)
  dyn <- adiposity_spec$dynamics$long
  
  if (!is.list(dyn)) {
    stop("Long-memory adiposity dynamics are missing.", call. = FALSE)
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
  
  latent_age_ref <- get_adiposity_latent_age_ref(spec)
  latent_noise_sd <- dyn$latent_noise_sd
  
  if (!is_scalar_number(latent_noise_sd) || latent_noise_sd <= 0) {
    stop(
      "Long-memory adiposity dynamics require numeric latent_noise_sd > 0.",
      call. = FALSE
    )
  }
  
  (dyn$latent_ar %||% 0) * latent_metabolic +
    (dyn$latent_age_coef %||% 0) * (age - latent_age_ref) +
    (dyn$latent_recent_mean_score_coef %||% 0) * recent_mean_score +
    (dyn$latent_cum_mean_score_coef %||% 0) * cum_mean_score +
    (dyn$latent_fh_diabetes_coef %||% 0) * fh_diabetes +
    rnorm(1, mean = 0, sd = latent_noise_sd)
}

# Sample the true visceral-adiposity value under the short-memory model.
#
# Returned fields:
# - p_obesity_target
# - target_mean_visceral
# - mean_visceral
# - visceral_true
# - obese_indicator
# - latent_metabolic
sample_visceral_state_short <- function(spec,
                                        cal_time,
                                        age,
                                        sex,
                                        ses,
                                        education = NULL,
                                        ethnicity = NULL,
                                        geography = NULL,
                                        latent_metabolic,
                                        alcohol_score,
                                        fh_diabetes = 0,
                                        prev_visceral,
                                        disease = NULL) {
  adiposity_spec <- get_adiposity_spec(spec)
  dyn <- adiposity_spec$dynamics$short
  
  if (!is.list(dyn)) {
    stop("Short-memory adiposity dynamics are missing.", call. = FALSE)
  }
  
  latent_metabolic_new <- update_latent_metabolic_short(
    spec = spec,
    age = age,
    latent_metabolic = latent_metabolic,
    alcohol_score = alcohol_score,
    fh_diabetes = fh_diabetes
  )
  
  p_obesity_target <- resolve_obesity_target(
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
  
  target_mean_visceral <- get_target_visceral_mean(
    spec = spec,
    obesity_prob = p_obesity_target
  )
  
  mean_visceral <- target_mean_visceral +
    (dyn$mean_prev_visceral_coef %||% 0) * (prev_visceral - target_mean_visceral) +
    (dyn$mean_alcohol_score_coef %||% 0) * alcohol_score +
    (dyn$mean_latent_metabolic_coef %||% 0) * latent_metabolic_new
  
  visceral_true <- rnorm_trunc(
    n = 1,
    mean = mean_visceral,
    sd = adiposity_spec$visceral_sd,
    min = adiposity_spec$baseline_distribution$min,
    max = adiposity_spec$baseline_distribution$max
  )
  
  obese_indicator <- as.integer(visceral_true >= adiposity_spec$obesity_threshold)
  
  list(
    latent_metabolic = latent_metabolic_new,
    p_obesity_target = p_obesity_target,
    target_mean_visceral = target_mean_visceral,
    mean_visceral = mean_visceral,
    visceral_true = visceral_true,
    obese_indicator = obese_indicator
  )
}

# Sample the true visceral-adiposity value under the long-memory model.
#
# Returned fields:
# - p_obesity_target
# - target_mean_visceral
# - mean_visceral
# - visceral_true
# - obese_indicator
# - latent_metabolic
# - recent_mean_score
# - cum_mean_score
# - recent_visceral
sample_visceral_state_long <- function(spec,
                                       cal_time,
                                       age,
                                       sex,
                                       ses,
                                       education = NULL,
                                       ethnicity = NULL,
                                       geography = NULL,
                                       latent_metabolic,
                                       alcohol_score,
                                       fh_diabetes = 0,
                                       prev_visceral,
                                       history_features = NULL,
                                       disease = NULL) {
  adiposity_spec <- get_adiposity_spec(spec)
  dyn <- adiposity_spec$dynamics$long
  
  if (!is.list(dyn)) {
    stop("Long-memory adiposity dynamics are missing.", call. = FALSE)
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
  
  recent_visceral <- list_numeric_or_default(
    x = history_features,
    name = "recent_visceral",
    fallback = prev_visceral
  )
  
  latent_metabolic_new <- update_latent_metabolic_long(
    spec = spec,
    age = age,
    latent_metabolic = latent_metabolic,
    fh_diabetes = fh_diabetes,
    history_features = history_features
  )
  
  p_obesity_target <- resolve_obesity_target(
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
  
  target_mean_visceral <- get_target_visceral_mean(
    spec = spec,
    obesity_prob = p_obesity_target
  )
  
  mean_visceral <- target_mean_visceral +
    (dyn$mean_prev_visceral_coef %||% 0) * (prev_visceral - target_mean_visceral) +
    (dyn$mean_recent_visceral_coef %||% 0) * (recent_visceral - target_mean_visceral) +
    (dyn$mean_alcohol_score_coef %||% 0) * alcohol_score +
    (dyn$mean_recent_mean_score_coef %||% 0) * recent_mean_score +
    (dyn$mean_latent_metabolic_coef %||% 0) * latent_metabolic_new
  
  visceral_true <- rnorm_trunc(
    n = 1,
    mean = mean_visceral,
    sd = adiposity_spec$visceral_sd,
    min = adiposity_spec$baseline_distribution$min,
    max = adiposity_spec$baseline_distribution$max
  )
  
  obese_indicator <- as.integer(visceral_true >= adiposity_spec$obesity_threshold)
  
  list(
    latent_metabolic = latent_metabolic_new,
    p_obesity_target = p_obesity_target,
    target_mean_visceral = target_mean_visceral,
    mean_visceral = mean_visceral,
    visceral_true = visceral_true,
    obese_indicator = obese_indicator,
    recent_mean_score = recent_mean_score,
    cum_mean_score = cum_mean_score,
    recent_visceral = recent_visceral
  )
}

# Update the adiposity exposure under the requested memory model.
#
# This is the main wrapper function used by the simulator.
#
# For memory_model = "short", the key inputs are:
# - current alcohol_score
# - previous visceral value
#
# For memory_model = "long", the key additional input is:
# - history_features
#
# Returned fields:
# - latent_metabolic
# - p_obesity_target
# - target_mean_visceral
# - mean_visceral
# - visceral_true
# - obese_indicator
#
# and, for the long-memory model, also:
# - recent_mean_score
# - cum_mean_score
# - recent_visceral
update_adiposity_exposure <- function(spec,
                                      memory_model = c("short", "long"),
                                      cal_time,
                                      age,
                                      sex,
                                      ses,
                                      education = NULL,
                                      ethnicity = NULL,
                                      geography = NULL,
                                      latent_metabolic,
                                      alcohol_score,
                                      fh_diabetes = 0,
                                      prev_visceral,
                                      history_features = NULL,
                                      disease = NULL) {
  memory_model <- match.arg(memory_model)
  
  if (identical(memory_model, "short")) {
    return(
      sample_visceral_state_short(
        spec = spec,
        cal_time = cal_time,
        age = age,
        sex = sex,
        ses = ses,
        education = education,
        ethnicity = ethnicity,
        geography = geography,
        latent_metabolic = latent_metabolic,
        alcohol_score = alcohol_score,
        fh_diabetes = fh_diabetes,
        prev_visceral = prev_visceral,
        disease = disease
      )
    )
  }
  
  sample_visceral_state_long(
    spec = spec,
    cal_time = cal_time,
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    latent_metabolic = latent_metabolic,
    alcohol_score = alcohol_score,
    fh_diabetes = fh_diabetes,
    prev_visceral = prev_visceral,
    history_features = history_features,
    disease = disease
  )
}

# Convenience wrapper for the short-memory adiposity update.
update_adiposity_exposure_short <- function(spec,
                                            cal_time,
                                            age,
                                            sex,
                                            ses,
                                            education = NULL,
                                            ethnicity = NULL,
                                            geography = NULL,
                                            latent_metabolic,
                                            alcohol_score,
                                            fh_diabetes = 0,
                                            prev_visceral,
                                            disease = NULL) {
  update_adiposity_exposure(
    spec = spec,
    memory_model = "short",
    cal_time = cal_time,
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    latent_metabolic = latent_metabolic,
    alcohol_score = alcohol_score,
    fh_diabetes = fh_diabetes,
    prev_visceral = prev_visceral,
    disease = disease
  )
}

# Convenience wrapper for the long-memory adiposity update.
update_adiposity_exposure_long <- function(spec,
                                           cal_time,
                                           age,
                                           sex,
                                           ses,
                                           education = NULL,
                                           ethnicity = NULL,
                                           geography = NULL,
                                           latent_metabolic,
                                           alcohol_score,
                                           fh_diabetes = 0,
                                           prev_visceral,
                                           history_features = NULL,
                                           disease = NULL) {
  update_adiposity_exposure(
    spec = spec,
    memory_model = "long",
    cal_time = cal_time,
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    latent_metabolic = latent_metabolic,
    alcohol_score = alcohol_score,
    fh_diabetes = fh_diabetes,
    prev_visceral = prev_visceral,
    history_features = history_features,
    disease = disease
  )
}
