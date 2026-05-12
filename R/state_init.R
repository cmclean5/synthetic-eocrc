# Initialise one simulated person state from the JSON specification.
#
# This script contains the baseline person-generation logic used by the
# simulator. It is the state-initialisation module in the agreed project
# structure and is responsible for:
# - selecting the active disease and mortality model
# - drawing baseline demographics
# - drawing socioeconomic status
# - drawing ethnicity and geography
# - generating family-history variables
# - generating inherited predisposition variables
# - generating latent traits
# - initialising adiposity and insulin state through the exposure modules
#
# Main user-facing function:
# - initialize_person_state()
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
# - from config_accessors.R:
#   - get_study_spec()
#   - get_population_spec()
#   - get_latent_traits_spec()
#   - get_disease_name()
#   - get_disease_spec()
#   - get_mortality_name()
#   - get_mortality_spec()
# - from exposure modules:
#   - initialize_adiposity_state()
#   - initialize_insulin_state()
#
# Package design note:
# - this file assumes these helpers are available in the package namespace
# - it does not perform per-file existence checks

# Draw a baseline entry age from the configured entry-age distribution.
#
# The current bridge model supports a truncated increasing exponential
# distribution, matching the existing colorectal setup.
.state_init_sample_entry_age <- function(spec) {
  population <- get_population_spec(spec)
  entry_age <- population$entry_age
  
  distribution <- entry_age$distribution %||% NA_character_
  age_min <- entry_age$min
  age_max <- entry_age$max
  rate <- entry_age$rate
  
  if (!identical(distribution, "truncated_exponential")) {
    stop(
      "Unsupported population.entry_age.distribution: ", distribution,
      call. = FALSE
    )
  }
  
  if (!all(vapply(c(age_min, age_max, rate), is_scalar_number, logical(1)))) {
    stop("Entry-age distribution requires numeric min, max, and rate.", call. = FALSE)
  }
  
  if (rate <= 0) {
    stop("Entry-age rate must be > 0.", call. = FALSE)
  }
  
  width <- age_max - age_min
  u <- runif(1)
  
  age_min + log(1 + u * (exp(rate * width) - 1)) / rate
}

# Draw a baseline attribute from an attribute specification.
#
# Supported assignment methods are:
# - fixed
# - categorical
.state_init_draw_attribute <- function(attribute_spec) {
  if (!is.list(attribute_spec)) {
    stop("Attribute specification must be a list.", call. = FALSE)
  }
  
  assignment <- attribute_spec$assignment
  
  if (!is.list(assignment)) {
    stop("Attribute specification must contain an assignment list.", call. = FALSE)
  }
  
  method <- assignment$method %||% NA_character_
  
  if (identical(method, "fixed")) {
    value <- assignment$value
    
    if (!is_scalar_string(value)) {
      stop("Fixed attribute assignment requires a non-empty string value.", call. = FALSE)
    }
    
    return(value)
  }
  
  if (identical(method, "categorical")) {
    return(
      sample_from_named_probs(
        prob_map = assignment$probs,
        context = "Attribute assignment probabilities"
      )
    )
  }
  
  stop(
    "Unsupported attribute assignment method: ", method,
    call. = FALSE
  )
}

# Draw a baseline attribute only if the dimension is enabled.
#
# If the dimension is disabled, return NA_character_.
.state_init_draw_attribute_if_enabled <- function(attribute_spec) {
  if (!is.list(attribute_spec)) {
    return(NA_character_)
  }
  
  if (!isTRUE(attribute_spec$enabled)) {
    return(NA_character_)
  }
  
  .state_init_draw_attribute(attribute_spec)
}

# Draw socioeconomic status conditional on education.
.state_init_draw_ses <- function(spec, education) {
  population <- get_population_spec(spec)
  ses_given_education <- population$ses_given_education
  
  if (!is.list(ses_given_education) || is.null(ses_given_education[[education]])) {
    stop(
      "No SES distribution available for education level '", education, "'.",
      call. = FALSE
    )
  }
  
  sample_from_named_probs(
    prob_map = ses_given_education[[education]],
    context = paste0("SES probabilities for education level '", education, "'")
  )
}

# Draw a Bernoulli outcome from a logit model specification.
#
# The model specification is expected to contain:
# - intercept
# - coefficients
.state_init_draw_bernoulli_logit <- function(model_spec, context = list()) {
  if (!is.list(model_spec)) {
    stop("Bernoulli logit model specification must be a list.", call. = FALSE)
  }
  
  lp <- linear_predictor(
    intercept = model_spec$intercept %||% 0,
    coefficients = model_spec$coefficients %||% list(),
    context = context
  )
  
  rbinom(1, 1, invlogit(lp))
}

# Draw a normal value from a linear model specification.
#
# The model specification is expected to contain:
# - intercept
# - coefficients
# - sd
.state_init_draw_normal_linear <- function(model_spec, context = list()) {
  if (!is.list(model_spec)) {
    stop("Normal linear model specification must be a list.", call. = FALSE)
  }
  
  mean_value <- linear_predictor(
    intercept = model_spec$intercept %||% 0,
    coefficients = model_spec$coefficients %||% list(),
    context = context
  )
  
  sd_value <- model_spec$sd
  
  if (!is_scalar_number(sd_value) || sd_value <= 0) {
    stop("Normal linear model requires sd > 0.", call. = FALSE)
  }
  
  rnorm(1, mean = mean_value, sd = sd_value)
}

# Initialise one simulated person state from the spec.
#
# Returned fields include:
# - selected disease and mortality model names
# - baseline demographics and socioeconomic variables
# - family-history and inherited predisposition lists
# - latent traits
# - initial adiposity and insulin state
# - baseline event placeholders used by the current simulator
#
# The returned object is a list intended to be consumed by the later
# person-level simulation loop.
initialize_person_state <- function(spec,
                                    disease = NULL,
                                    mortality_name = NULL) {
  if (!is.list(spec)) {
    stop("`spec` must be a nested list.", call. = FALSE)
  }
  
  study <- get_study_spec(spec)
  population <- get_population_spec(spec)
  latent_traits <- get_latent_traits_spec(spec)
  
  disease_name <- get_disease_name(spec, disease = disease)
  disease_spec <- get_disease_spec(spec, disease = disease_name)
  
  mortality_name <- get_mortality_name(spec, mortality_name = mortality_name)
  mortality_spec <- get_mortality_spec(spec, mortality_name = mortality_name)
  
  if (!is.list(study)) {
    stop("Study specification must be a list.", call. = FALSE)
  }
  
  if (!is.list(population)) {
    stop("Population specification must be a list.", call. = FALSE)
  }
  
  if (!is.list(latent_traits)) {
    stop("Latent-traits specification must be a list.", call. = FALSE)
  }
  
  # Draw baseline demographic variables.
  sex <- sample_from_named_probs(
    prob_map = population$sex_probs,
    context = "Sex probabilities"
  )
  
  education <- sample_from_named_probs(
    prob_map = population$education_probs,
    context = "Education probabilities"
  )
  
  ses <- .state_init_draw_ses(spec, education)
  
  ethnicity <- .state_init_draw_attribute_if_enabled(population$ethnicity)
  geography <- .state_init_draw_attribute_if_enabled(population$geography)
  
  # Build the baseline covariate context used by downstream baseline models.
  base_context <- list(
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography
  )
  
  # Generate family-history variables sequentially.
  #
  # Sequential generation allows later family-history variables to depend on
  # earlier ones if needed.
  family_history_models <- population$family_history_models
  family_history <- list()
  
  for (nm in names(family_history_models)) {
    family_history[[nm]] <- .state_init_draw_bernoulli_logit(
      model_spec = family_history_models[[nm]],
      context = c(base_context, family_history)
    )
  }
  
  # Generate inherited predisposition variables sequentially.
  genetic_models <- population$genetic_models
  genetic <- list()
  
  for (nm in names(genetic_models)) {
    genetic[[nm]] <- .state_init_draw_bernoulli_logit(
      model_spec = genetic_models[[nm]],
      context = c(base_context, family_history, genetic)
    )
  }
  
  # Sample entry age and entry calendar time.
  entry_age <- .state_init_sample_entry_age(spec)
  
  if (isTRUE(study$sample_entry_cal_time)) {
    latest_entry <- study$calendar_end - study$min_followup_time
    
    if (!is_scalar_number(latest_entry) || latest_entry <= study$calendar_start) {
      stop("Study window too short for minimum follow-up requirement.", call. = FALSE)
    }
    
    entry_cal_time <- runif(1, study$calendar_start, latest_entry)
  } else {
    entry_cal_time <- study$calendar_start
  }
  
  # Generate latent traits.
  #
  # Latent traits may depend on baseline covariates, family history, and genetic
  # variables. Sequential generation also allows later latent traits to depend
  # on earlier ones if needed.
  latent_context <- c(base_context, family_history, genetic)
  latent_values <- list()
  
  for (nm in names(latent_traits)) {
    latent_values[[nm]] <- .state_init_draw_normal_linear(
      model_spec = latent_traits[[nm]],
      context = c(latent_context, latent_values)
    )
  }
  
  latent_metabolic <- latent_values$latent_metabolic %||% 0
  latent_crc <- latent_values$latent_crc %||% 0
  
  # Set the initial time state.
  age <- entry_age
  cal_time <- entry_cal_time
  
  # Initialise adiposity and insulin via their dedicated exposure modules.
  adiposity_init <- initialize_adiposity_state(
    spec = spec,
    cal_time = cal_time,
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    latent_metabolic = latent_metabolic,
    disease = disease_name
  )
  
  insulin_init <- initialize_insulin_state(
    spec = spec,
    prev_visceral = adiposity_init$prev_visceral,
    latent_metabolic = latent_metabolic
  )
  
  # Assemble the baseline state object.
  #
  # This structure is intentionally close to the current bridge-model state
  # object so that the later person-level simulation loop remains simple.
  out <- list(
    disease = disease_name,
    mortality_name = mortality_name,
    disease_spec = disease_spec,
    mortality_spec = mortality_spec,
    event_name = disease_spec$event_name %||% disease_name,
    sex = sex,
    education = education,
    ses = ses,
    ethnicity = ethnicity,
    geography = geography,
    family_history = family_history,
    genetic = genetic,
    latent = latent_values,
    latent_metabolic = latent_metabolic,
    latent_crc = latent_crc,
    entry_age = entry_age,
    entry_cal_time = entry_cal_time,
    age = age,
    cal_time = cal_time,
    visit = 1L,
    event = 0L,
    death = 0L,
    death_before_event = 0L,
    age_at_diagnosis = NA_real_,
    cal_time_at_diagnosis = NA_real_,
    age_at_death = NA_real_,
    cal_time_at_death = NA_real_,
    death_cause = NA_character_,
    stage = NA_character_,
    prev_alcohol_score = 0,
    p_obesity_target_0 = adiposity_init$p_obesity_target_0,
    target_mean_visceral_0 = adiposity_init$target_mean_visceral_0,
    prev_visceral = adiposity_init$prev_visceral,
    prev_log_insulin = insulin_init$prev_log_insulin,
    prev_insulin = insulin_init$prev_insulin
  )
  
  # Expose commonly used variables at top level when they exist.
  #
  # This keeps downstream current-model code readable while the framework is
  # transitioning toward a more fully generic structure.
  if ("fh_crc" %in% names(family_history)) {
    out$fh_crc <- family_history$fh_crc
  }
  
  if ("fh_diabetes" %in% names(family_history)) {
    out$fh_diabetes <- family_history$fh_diabetes
  }
  
  if ("genetic_crc_predisposition" %in% names(genetic)) {
    out$genetic_crc_predisposition <- genetic$genetic_crc_predisposition
  }
  
  out
}
