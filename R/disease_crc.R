# Colorectal cancer disease module for the tumour simulator.
#
# This script contains the current CRC-specific disease logic used by the
# simulator, including:
# - extraction of the active CRC disease specification
# - rule-based calendar-time trend resolution
# - construction of incidence-model covariate contexts
# - calculation of CRC log-hazards and hazards
# - sampling of CRC event times from exponential waiting-time models
# - construction of stage-model covariate contexts
# - sampling of diagnosis stage at the time of CRC diagnosis
#
# Main user-facing functions:
# - get_crc_disease_name()
# - get_crc_disease_spec()
# - get_crc_event_name()
# - resolve_crc_time_trend()
# - build_crc_incidence_context()
# - compute_crc_log_hazard()
# - compute_crc_hazard()
# - sample_crc_event_time()
# - build_crc_stage_context()
# - sample_crc_stage()
#
# Current design notes:
# - this module is compatible with the current colorectal bridge model
# - CRC incidence uses a log-linear hazard model
# - calendar-time trend contributions are resolved through resolve_rule()
# - the visceral-adiposity centring reference is now read from the adiposity
#   config rather than being hard-coded in this module
# - stage at diagnosis is modelled as:
#   - advanced versus early via a logit model
#   - then stage sampled from either early-stage or advanced-stage probabilities
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()
#   - is_scalar_string()
#   - linear_predictor()
#   - invlogit()
# - from utils_sampling.R:
#   - sample_from_named_probs()
#   - sample_exponential_wait_time()
# - from config_accessors.R:
#   - get_adiposity_reference_value()
#   - get_disease_names()
#   - get_enabled_disease_names()
#   - get_disease_spec()
# - from rule_resolver.R:
#   - resolve_rule()

# Check whether a disease name looks CRC-like.
#
# This allows the CRC module to work with names such as:
# - "crc"
# - "eo_crc"
# - "colorectal_crc"
# while still rejecting clearly non-CRC disease modules.
.disease_crc_is_crc_like_name <- function(disease_name) {
  is_scalar_string(disease_name) &&
    grepl("crc", disease_name, ignore.case = TRUE)
}

# Return the active CRC disease name.
#
# Selection logic:
# - if `disease` is supplied:
#   - it must be enabled
#   - and must look CRC-like
# - otherwise:
#   - prefer an enabled disease named exactly "crc"
#   - otherwise use the first enabled disease whose name looks CRC-like
get_crc_disease_name <- function(spec, disease = NULL) {
  if (!is.null(disease)) {
    disease_spec <- get_disease_spec(
      spec = spec,
      disease = disease,
      must_be_enabled = TRUE
    )
    
    if (!is.list(disease_spec)) {
      stop("CRC disease specification must be a list.", call. = FALSE)
    }
    
    if (!.disease_crc_is_crc_like_name(disease)) {
      stop(
        "Disease '", disease, "' does not appear to be a CRC disease.",
        call. = FALSE
      )
    }
    
    return(disease)
  }
  
  enabled_names <- get_enabled_disease_names(spec)
  
  if ("crc" %in% enabled_names) {
    return("crc")
  }
  
  crc_like <- enabled_names[grepl("crc", enabled_names, ignore.case = TRUE)]
  
  if (length(crc_like) > 0) {
    return(crc_like[1])
  }
  
  stop(
    "No enabled CRC disease specification found in `spec$diseases`.",
    call. = FALSE
  )
}

# Return the active CRC disease specification block.
get_crc_disease_spec <- function(spec, disease = NULL) {
  disease_name <- get_crc_disease_name(spec, disease = disease)
  
  disease_spec <- get_disease_spec(
    spec = spec,
    disease = disease_name,
    must_be_enabled = TRUE
  )
  
  if (!is.list(disease_spec)) {
    stop("CRC disease specification must be a list.", call. = FALSE)
  }
  
  disease_spec
}

# Return the configured event name for the active CRC disease.
get_crc_event_name <- function(spec, disease = NULL) {
  disease_name <- get_crc_disease_name(spec, disease = disease)
  disease_spec <- get_crc_disease_spec(spec, disease = disease_name)
  
  event_name <- disease_spec$event_name %||% disease_name
  
  if (!is_scalar_string(event_name)) {
    stop("CRC event name must be a non-empty string.", call. = FALSE)
  }
  
  event_name
}

# Resolve the CRC calendar-time trend contribution at the requested calendar
# time.
#
# In the current model, this is typically an APC-style log offset obtained via:
# - disease_spec$incidence_model$time_trend_target
resolve_crc_time_trend <- function(spec,
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
  
  disease_name <- get_crc_disease_name(spec, disease = disease)
  disease_spec <- get_crc_disease_spec(spec, disease = disease_name)
  incidence_model <- disease_spec$incidence_model
  
  if (!is.list(incidence_model)) {
    stop("CRC incidence model specification is missing.", call. = FALSE)
  }
  
  target_name <- incidence_model$time_trend_target
  
  if (!is_scalar_string(target_name)) {
    stop("CRC incidence model must define a non-empty time_trend_target.", call. = FALSE)
  }
  
  resolve_rule(
    spec = spec,
    target = target_name,
    year = cal_time,
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    disease = disease_name
  )
}

# Build the covariate context for the CRC incidence model.
#
# This function mirrors the current bridge-model context construction and keeps
# the feature names aligned with the current colorectal config.
#
# The `memory_model` argument controls which history-dependent terms are active.
#
# Returned fields include:
# - age_per_year
# - sex
# - ses
# - education
# - ethnicity
# - geography
# - fh_crc
# - fh_diabetes
# - genetic_crc_predisposition
# - alcohol_score_short
# - alcohol_score_long
# - recent_mean_score
# - haz_recent_prop
# - visceral_current_per_unit
# - visceral_recent_per_unit
# - visceral_cum_per_unit
# - log_insulin_current
# - recent_log_insulin
# - high_insulin_recent_prop
# - latent_crc
build_crc_incidence_context <- function(spec,
                                        memory_model = c("short", "long"),
                                        age,
                                        sex,
                                        ses,
                                        education = NULL,
                                        ethnicity = NULL,
                                        geography = NULL,
                                        alcohol_score,
                                        recent_mean_score = 0,
                                        haz_recent_prop = 0,
                                        visceral_true,
                                        recent_visceral = 0,
                                        cum_visceral = 0,
                                        insulin_true,
                                        recent_log_insulin = 0,
                                        high_insulin_recent_prop = 0,
                                        fh_crc = 0,
                                        fh_diabetes = 0,
                                        genetic_crc_predisposition = 0,
                                        latent_crc = 0,
                                        disease = NULL) {
  memory_model <- match.arg(memory_model)
  
  disease_spec <- get_crc_disease_spec(spec, disease = disease)
  incidence_model <- disease_spec$incidence_model
  
  if (!is.list(incidence_model)) {
    stop("CRC incidence model specification is missing.", call. = FALSE)
  }
  
  age_ref <- incidence_model$age_ref
  
  if (!is_scalar_number(age_ref)) {
    stop("CRC incidence model age_ref must be a numeric scalar.", call. = FALSE)
  }
  
  if (!all(vapply(c(age, alcohol_score, visceral_true, insulin_true,
                    fh_crc, fh_diabetes, genetic_crc_predisposition, latent_crc),
                  is_scalar_number,
                  logical(1)))) {
    stop(
      "CRC incidence context requires numeric values for age, alcohol_score, ",
      "visceral_true, insulin_true, fh_crc, fh_diabetes, ",
      "genetic_crc_predisposition, and latent_crc.",
      call. = FALSE
    )
  }
  
  if (insulin_true <= 0) {
    stop("`insulin_true` must be > 0.", call. = FALSE)
  }
  
  visceral_ref <- get_adiposity_reference_value(spec)
  
  list(
    age_per_year = age - age_ref,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    fh_crc = fh_crc,
    fh_diabetes = fh_diabetes,
    genetic_crc_predisposition = genetic_crc_predisposition,
    alcohol_score_short = if (identical(memory_model, "short")) alcohol_score else 0,
    alcohol_score_long = if (identical(memory_model, "long")) alcohol_score else 0,
    recent_mean_score = if (identical(memory_model, "long")) recent_mean_score else 0,
    haz_recent_prop = if (identical(memory_model, "long")) haz_recent_prop else 0,
    visceral_current_per_unit = visceral_true - visceral_ref,
    visceral_recent_per_unit = if (identical(memory_model, "long")) {
      recent_visceral - visceral_ref
    } else {
      0
    },
    visceral_cum_per_unit = if (identical(memory_model, "long")) {
      cum_visceral - visceral_ref
    } else {
      0
    },
    log_insulin_current = log(insulin_true),
    recent_log_insulin = if (identical(memory_model, "long")) recent_log_insulin else 0,
    high_insulin_recent_prop = if (identical(memory_model, "long")) high_insulin_recent_prop else 0,
    latent_crc = latent_crc
  )
}

# Compute the CRC log-hazard at the requested calendar time.
#
# The current model combines:
# - the incidence-model intercept
# - the resolved calendar-time trend contribution
# - the linear predictor from build_crc_incidence_context()
compute_crc_log_hazard <- function(spec,
                                   cal_time,
                                   memory_model = c("short", "long"),
                                   age,
                                   sex,
                                   ses,
                                   education = NULL,
                                   ethnicity = NULL,
                                   geography = NULL,
                                   alcohol_score,
                                   recent_mean_score = 0,
                                   haz_recent_prop = 0,
                                   visceral_true,
                                   recent_visceral = 0,
                                   cum_visceral = 0,
                                   insulin_true,
                                   recent_log_insulin = 0,
                                   high_insulin_recent_prop = 0,
                                   fh_crc = 0,
                                   fh_diabetes = 0,
                                   genetic_crc_predisposition = 0,
                                   latent_crc = 0,
                                   disease = NULL) {
  memory_model <- match.arg(memory_model)
  
  disease_name <- get_crc_disease_name(spec, disease = disease)
  disease_spec <- get_crc_disease_spec(spec, disease = disease_name)
  incidence_model <- disease_spec$incidence_model
  
  if (!is.list(incidence_model)) {
    stop("CRC incidence model specification is missing.", call. = FALSE)
  }
  
  intercept <- incidence_model$intercept %||% 0
  
  if (!is_scalar_number(intercept)) {
    stop("CRC incidence model intercept must be numeric.", call. = FALSE)
  }
  
  time_trend <- resolve_crc_time_trend(
    spec = spec,
    cal_time = cal_time,
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    disease = disease_name
  )
  
  if (!is_scalar_number(time_trend)) {
    stop("Resolved CRC time trend must be a numeric scalar.", call. = FALSE)
  }
  
  context <- build_crc_incidence_context(
    spec = spec,
    memory_model = memory_model,
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    alcohol_score = alcohol_score,
    recent_mean_score = recent_mean_score,
    haz_recent_prop = haz_recent_prop,
    visceral_true = visceral_true,
    recent_visceral = recent_visceral,
    cum_visceral = cum_visceral,
    insulin_true = insulin_true,
    recent_log_insulin = recent_log_insulin,
    high_insulin_recent_prop = high_insulin_recent_prop,
    fh_crc = fh_crc,
    fh_diabetes = fh_diabetes,
    genetic_crc_predisposition = genetic_crc_predisposition,
    latent_crc = latent_crc,
    disease = disease_name
  )
  
  intercept +
    time_trend +
    linear_predictor(
      intercept = 0,
      coefficients = incidence_model$coefficients %||% list(),
      context = context
    )
}

# Compute the CRC hazard at the requested calendar time.
compute_crc_hazard <- function(spec,
                               cal_time,
                               memory_model = c("short", "long"),
                               age,
                               sex,
                               ses,
                               education = NULL,
                               ethnicity = NULL,
                               geography = NULL,
                               alcohol_score,
                               recent_mean_score = 0,
                               haz_recent_prop = 0,
                               visceral_true,
                               recent_visceral = 0,
                               cum_visceral = 0,
                               insulin_true,
                               recent_log_insulin = 0,
                               high_insulin_recent_prop = 0,
                               fh_crc = 0,
                               fh_diabetes = 0,
                               genetic_crc_predisposition = 0,
                               latent_crc = 0,
                               disease = NULL) {
  exp(
    compute_crc_log_hazard(
      spec = spec,
      cal_time = cal_time,
      memory_model = memory_model,
      age = age,
      sex = sex,
      ses = ses,
      education = education,
      ethnicity = ethnicity,
      geography = geography,
      alcohol_score = alcohol_score,
      recent_mean_score = recent_mean_score,
      haz_recent_prop = haz_recent_prop,
      visceral_true = visceral_true,
      recent_visceral = recent_visceral,
      cum_visceral = cum_visceral,
      insulin_true = insulin_true,
      recent_log_insulin = recent_log_insulin,
      high_insulin_recent_prop = high_insulin_recent_prop,
      fh_crc = fh_crc,
      fh_diabetes = fh_diabetes,
      genetic_crc_predisposition = genetic_crc_predisposition,
      latent_crc = latent_crc,
      disease = disease
    )
  )
}

# Sample a CRC event time from an exponential waiting-time distribution.
#
# This is a convenience helper for the current simulator, where interval-level
# hazards are converted into event waiting times inside the person-level loop.
sample_crc_event_time <- function(h_crc) {
  sample_exponential_wait_time(h_crc)
}

# Build the covariate context for the CRC stage-at-diagnosis model.
#
# The current stage model uses:
# - SES
# - latent CRC score
# - current visceral adiposity
# - current log insulin
# - cumulative visceral adiposity in the long-memory model
# - recent log insulin in the long-memory model
# - interval gap
build_crc_stage_context <- function(spec,
                                    memory_model = c("short", "long"),
                                    ses,
                                    latent_crc,
                                    visceral_true,
                                    insulin_true,
                                    gap,
                                    cum_visceral = 0,
                                    recent_log_insulin = 0) {
  memory_model <- match.arg(memory_model)
  
  if (!all(vapply(c(latent_crc, visceral_true, insulin_true, gap),
                  is_scalar_number,
                  logical(1)))) {
    stop(
      "CRC stage context requires numeric latent_crc, visceral_true, insulin_true, and gap.",
      call. = FALSE
    )
  }
  
  if (insulin_true <= 0) {
    stop("`insulin_true` must be > 0.", call. = FALSE)
  }
  
  visceral_ref <- get_adiposity_reference_value(spec)
  
  list(
    ses = ses,
    latent_crc = latent_crc,
    visceral_current_per_unit = visceral_true - visceral_ref,
    log_insulin_current = log(insulin_true),
    cum_visceral_per_unit = if (identical(memory_model, "long")) {
      cum_visceral - visceral_ref
    } else {
      0
    },
    recent_log_insulin = if (identical(memory_model, "long")) recent_log_insulin else 0,
    interval_gap = gap
  )
}

# Sample CRC stage at diagnosis.
#
# Current bridge-model behaviour:
# 1. compute advanced-versus-early probability from a logit model
# 2. sample advanced status
# 3. if advanced:
#      sample from advanced_stage_probs
#    else:
#      sample from early_stage_probs
#
# Returned fields:
# - stage
# - advanced
# - p_advanced
sample_crc_stage <- function(spec,
                             memory_model = c("short", "long"),
                             ses,
                             latent_crc,
                             visceral_true,
                             insulin_true,
                             gap,
                             cum_visceral = 0,
                             recent_log_insulin = 0,
                             disease = NULL) {
  memory_model <- match.arg(memory_model)
  
  disease_spec <- get_crc_disease_spec(spec, disease = disease)
  stage_model <- disease_spec$stage_model
  
  if (!is.list(stage_model)) {
    stop("CRC stage model specification is missing.", call. = FALSE)
  }
  
  context <- build_crc_stage_context(
    spec = spec,
    memory_model = memory_model,
    ses = ses,
    latent_crc = latent_crc,
    visceral_true = visceral_true,
    insulin_true = insulin_true,
    gap = gap,
    cum_visceral = cum_visceral,
    recent_log_insulin = recent_log_insulin
  )
  
  p_advanced <- invlogit(
    linear_predictor(
      intercept = stage_model$advanced_logit_intercept %||% 0,
      coefficients = stage_model$advanced_logit_coefficients %||% list(),
      context = context
    )
  )
  
  advanced <- rbinom(1, 1, p_advanced)
  
  if (advanced == 1) {
    stage <- sample_from_named_probs(
      prob_map = stage_model$advanced_stage_probs,
      context = "CRC advanced-stage probabilities"
    )
  } else {
    stage <- sample_from_named_probs(
      prob_map = stage_model$early_stage_probs,
      context = "CRC early-stage probabilities"
    )
  }
  
  list(
    stage = stage,
    advanced = advanced,
    p_advanced = p_advanced
  )
}
