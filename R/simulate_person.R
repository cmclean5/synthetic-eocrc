# Simulate one person under the current modular tumour-simulator architecture.
#
# This script contains the person-level simulation loop. It coordinates:
# - baseline state initialisation
# - within-visit state updating
# - visit recording
# - visit-gap generation
# - disease hazard calculation
# - background mortality hazard calculation
# - competing-risk event sampling
# - diagnosis-stage sampling
# - final patient-level output assembly
#
# Main user-facing functions:
# - simulate_person()
# - simulate_person_short()
# - simulate_person_long()
#
# Current design notes:
# - this script currently supports the CRC bridge model
# - disease hazards are dispatched through disease_crc.R
# - background mortality is handled locally in this script
# - cohort-level wrappers belong in simulate_cohort.R
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()
#   - is_scalar_integerish()
#   - linear_predictor()
#   - list_numeric_or_default()
# - from utils_sampling.R:
#   - sample_exponential_wait_time()
#   - bind_rows_or_empty()
# - from config_accessors.R:
#   - get_study_spec()
# - from state_init.R:
#   - initialize_person_state()
# - from history_features.R:
#   - initialize_history_state()
# - from state_update.R:
#   - update_person_state()
# - from disease_crc.R:
#   - get_crc_disease_spec()
#   - compute_crc_hazard()
#   - sample_crc_stage()

# Check whether the currently selected disease is compatible with the CRC bridge
# model implemented in this script.
#
# This validation is intentionally strict so that non-CRC diseases fail early
# with a clear message rather than producing silently incorrect behaviour.
.simulate_person_validate_crc_bridge_disease <- function(spec,
                                                         disease_name) {
  disease_spec <- get_crc_disease_spec(
    spec = spec,
    disease = disease_name
  )
  
  if (!is.list(disease_spec)) {
    stop("CRC disease specification must be a list.", call. = FALSE)
  }
  
  invisible(TRUE)
}

# Build the mortality-model covariate context.
#
# The current background mortality model uses:
# - age_per_year
# - sex
# - ses
# - fh_diabetes
# - latent_metabolic
#
# Additional fields can be added later without changing the calling code.
.simulate_person_build_mortality_context <- function(mortality_spec,
                                                     age,
                                                     sex,
                                                     ses,
                                                     fh_diabetes,
                                                     latent_metabolic) {
  age_ref <- mortality_spec$age_ref
  
  if (!is_scalar_number(age_ref)) {
    stop("Mortality model age_ref must be a numeric scalar.", call. = FALSE)
  }
  
  list(
    age_per_year = age - age_ref,
    sex = sex,
    ses = ses,
    fh_diabetes = fh_diabetes,
    latent_metabolic = latent_metabolic
  )
}

# Compute the background mortality log-hazard for the current interval.
.simulate_person_compute_mortality_log_hazard <- function(state) {
  mortality_spec <- state$mortality_spec
  
  if (!is.list(mortality_spec)) {
    stop("Mortality specification is missing from state.", call. = FALSE)
  }
  
  context <- .simulate_person_build_mortality_context(
    mortality_spec = mortality_spec,
    age = state$age,
    sex = state$sex,
    ses = state$ses,
    fh_diabetes = list_numeric_or_default(state$family_history, "fh_diabetes", 0),
    latent_metabolic = state$latent_metabolic
  )
  
  linear_predictor(
    intercept = mortality_spec$intercept %||% 0,
    coefficients = mortality_spec$coefficients %||% list(),
    context = context
  )
}

# Compute the background mortality hazard for the current interval.
.simulate_person_compute_mortality_hazard <- function(state) {
  exp(.simulate_person_compute_mortality_log_hazard(state))
}

# Sample the next visit gap.
#
# This remains a current bridge-model compatibility process and has not yet
# been moved into the JSON config.
#
# The gap depends on:
# - the person's SES
# - the magnitude of the current latent metabolic state
.simulate_person_sample_visit_gap <- function(state,
                                              gap_min = 0.40,
                                              gap_mean_base = 1.2,
                                              gap_mean_low_ses_add = 0.25,
                                              gap_mean_abs_latent_add = 0.10,
                                              gap_sdlog = 0.30) {
  if (!all(vapply(
    c(gap_min, gap_mean_base, gap_mean_low_ses_add, gap_mean_abs_latent_add, gap_sdlog),
    is_scalar_number,
    logical(1)
  ))) {
    stop("Visit-gap parameters must all be numeric scalars.", call. = FALSE)
  }
  
  if (gap_min <= 0) {
    stop("`gap_min` must be > 0.", call. = FALSE)
  }
  
  if (gap_sdlog <= 0) {
    stop("`gap_sdlog` must be > 0.", call. = FALSE)
  }
  
  pmax(
    gap_min,
    rlnorm(
      1,
      meanlog = log(
        gap_mean_base +
          gap_mean_low_ses_add * as.numeric(state$ses == "Low") +
          gap_mean_abs_latent_add * abs(state$latent_metabolic)
      ),
      sdlog = gap_sdlog
    )
  )
}

# Build one visit-level output row from the current person state and current
# visit values.
#
# The output is intentionally close to the current bridge-model long table so
# that regression comparisons remain straightforward.
.simulate_person_build_visit_row <- function(id,
                                             state,
                                             current,
                                             memory_model) {
  base_row <- data.frame(
    id = id,
    visit = state$visit,
    age_raw = state$age,
    age = round(state$age, 2),
    cal_time_raw = state$cal_time,
    cal_time = round(state$cal_time, 2),
    sex = state$sex,
    education = state$education,
    ses = state$ses,
    ethnicity = state$ethnicity,
    geography = state$geography,
    fh_crc = list_numeric_or_default(state$family_history, "fh_crc", NA_real_),
    fh_diabetes = list_numeric_or_default(state$family_history, "fh_diabetes", NA_real_),
    genetic_crc_predisposition = list_numeric_or_default(
      state$genetic,
      "genetic_crc_predisposition",
      NA_real_
    ),
    alcohol_state = current$alcohol_state,
    alcohol_score = current$alcohol_score,
    alcohol_units = current$alcohol_units,
    prob_non_drinker = current$prob_non_drinker,
    prob_moderate = current$prob_moderate,
    prob_hazardous = current$prob_hazardous,
    obesity_target_prob = round(current$p_obesity_target, 3),
    obese_indicator = current$obese_indicator,
    visceral_adipose = current$visceral_obs,
    fasting_insulin = current$insulin_obs,
    p_measure_visceral = current$p_measure_visceral,
    p_measure_insulin = current$p_measure_insulin,
    stringsAsFactors = FALSE
  )
  
  if (identical(memory_model, "long")) {
    base_row$recent_mean_alcohol_score <- round(current$recent_mean_score, 3)
    base_row$cum_mean_alcohol_score <- round(current$cum_mean_score, 3)
    base_row$haz_recent_prop <- round(current$haz_recent_prop, 3)
  }
  
  base_row
}

# Build the final patient-level output row.
#
# This is intentionally close to the current bridge-model patient table and also
# includes compatibility event columns such as eo_crc when event_name == "eo_crc".
.simulate_person_build_patient_row <- function(id,
                                               state,
                                               spec,
                                               memory_model) {
  study <- get_study_spec(spec)
  event_name <- state$event_name %||% state$disease
  
  exit_age <- if (state$event == 1) {
    state$age_at_diagnosis
  } else if (state$death == 1) {
    state$age_at_death
  } else {
    state$age
  }
  
  exit_cal_time <- if (state$event == 1) {
    state$cal_time_at_diagnosis
  } else if (state$death == 1) {
    state$cal_time_at_death
  } else {
    state$cal_time
  }
  
  patient <- data.frame(
    id = id,
    disease = state$disease,
    event_name = event_name,
    memory_model = memory_model,
    sex = state$sex,
    education = state$education,
    ses = state$ses,
    ethnicity = state$ethnicity,
    geography = state$geography,
    fh_crc = list_numeric_or_default(state$family_history, "fh_crc", NA_real_),
    fh_diabetes = list_numeric_or_default(state$family_history, "fh_diabetes", NA_real_),
    genetic_crc_predisposition = list_numeric_or_default(
      state$genetic,
      "genetic_crc_predisposition",
      NA_real_
    ),
    entry_age_raw = state$entry_age,
    entry_age = round(state$entry_age, 2),
    entry_cal_time_raw = state$entry_cal_time,
    entry_cal_time = round(state$entry_cal_time, 2),
    study_start_cal_time = study$calendar_start,
    study_end_cal_time = study$calendar_end,
    event = state$event,
    death = state$death,
    death_before_event = state$death_before_event,
    control = 1 - state$event,
    age_at_diagnosis_raw = ifelse(state$event == 1, state$age_at_diagnosis, NA_real_),
    age_at_diagnosis = ifelse(state$event == 1, round(state$age_at_diagnosis, 2), NA_real_),
    cal_time_at_diagnosis_raw = ifelse(state$event == 1, state$cal_time_at_diagnosis, NA_real_),
    cal_time_at_diagnosis = ifelse(state$event == 1, round(state$cal_time_at_diagnosis, 2), NA_real_),
    age_at_death_raw = ifelse(state$death == 1, state$age_at_death, NA_real_),
    age_at_death = ifelse(state$death == 1, round(state$age_at_death, 2), NA_real_),
    cal_time_at_death_raw = ifelse(state$death == 1, state$cal_time_at_death, NA_real_),
    cal_time_at_death = ifelse(state$death == 1, round(state$cal_time_at_death, 2), NA_real_),
    death_cause = ifelse(state$death == 1, state$death_cause, NA_character_),
    censor_age_raw = exit_age,
    censor_age = round(exit_age, 2),
    censor_cal_time_raw = exit_cal_time,
    censor_cal_time = round(exit_cal_time, 2),
    stage = ifelse(state$event == 1, state$stage, NA_character_),
    severe_case = ifelse(
      state$event == 1 && !is.na(state$stage) && state$stage %in% c("III", "IV"),
      1,
      ifelse(state$event == 1, 0, NA)
    ),
    stringsAsFactors = FALSE
  )
  
  # Add disease-specific compatibility columns.
  patient[[event_name]] <- patient$event
  patient[[paste0("death_before_", event_name)]] <- patient$death_before_event
  
  patient
}

# Simulate one person under the current modular architecture.
#
# Arguments:
# - id:
#   person identifier
# - spec:
#   validated simulation specification
# - disease:
#   optional disease name; if NULL, initialize_person_state() selects one
# - memory_model:
#   either "short" or "long"
# - mortality_name:
#   optional mortality model name; if NULL, initialize_person_state() selects one
# - recent_window_years:
#   width of the recent-history window used for long-memory features
# - visit-gap parameters:
#   current compatibility parameters controlling the visit-gap process
#
# Returned value:
# - a list with:
#   - patient
#   - long
simulate_person <- function(id,
                            spec,
                            disease = NULL,
                            memory_model = c("short", "long"),
                            mortality_name = NULL,
                            recent_window_years = 2,
                            gap_min = 0.40,
                            gap_mean_base = 1.2,
                            gap_mean_low_ses_add = 0.25,
                            gap_mean_abs_latent_add = 0.10,
                            gap_sdlog = 0.30) {
  if (!is_scalar_integerish(id)) {
    stop("`id` must be an integer-valued numeric scalar.", call. = FALSE)
  }
  
  if (!is.list(spec)) {
    stop("`spec` must be a nested list.", call. = FALSE)
  }
  
  memory_model <- match.arg(memory_model)
  
  state <- initialize_person_state(
    spec = spec,
    disease = disease,
    mortality_name = mortality_name
  )
  
  # This person-level simulator currently implements the CRC bridge model.
  .simulate_person_validate_crc_bridge_disease(
    spec = spec,
    disease_name = state$disease
  )
  
  history <- initialize_history_state()
  rows <- list()
  
  study <- get_study_spec(spec)
  
  # Main person-level simulation loop.
  #
  # The loop continues until the person:
  # - reaches the study maximum age
  # - reaches the end of calendar-time follow-up
  # - develops the disease event
  # - dies before the disease event
  while (state$age < study$max_age &&
         state$cal_time < study$calendar_end &&
         state$event == 0 &&
         state$death == 0) {
    # Update within-visit behavioural and biological state.
    upd <- update_person_state(
      spec = spec,
      state = state,
      history = history,
      memory_model = memory_model,
      recent_window_years = recent_window_years
    )
    
    state <- upd$state
    history <- upd$history
    current <- upd$current
    
    # Record the visit row before moving forward in time.
    rows[[state$visit]] <- .simulate_person_build_visit_row(
      id = id,
      state = state,
      current = current,
      memory_model = memory_model
    )
    
    # Draw the next visit interval.
    gap <- .simulate_person_sample_visit_gap(
      state = state,
      gap_min = gap_min,
      gap_mean_base = gap_mean_base,
      gap_mean_low_ses_add = gap_mean_low_ses_add,
      gap_mean_abs_latent_add = gap_mean_abs_latent_add,
      gap_sdlog = gap_sdlog
    )
    
    # Truncate the interval so follow-up cannot extend beyond the allowed
    # administrative limits.
    gap <- min(
      gap,
      study$max_age - state$age,
      study$calendar_end - state$cal_time
    )
    
    if (gap <= 0) {
      break
    }
    
    # Evaluate interval hazards at the interval midpoint.
    cal_time_mid <- state$cal_time + 0.5 * gap
    
    h_event <- compute_crc_hazard(
      spec = spec,
      cal_time = cal_time_mid,
      memory_model = memory_model,
      age = state$age,
      sex = state$sex,
      ses = state$ses,
      education = state$education,
      ethnicity = state$ethnicity,
      geography = state$geography,
      alcohol_score = current$alcohol_score,
      recent_mean_score = current$recent_mean_score,
      haz_recent_prop = current$haz_recent_prop,
      visceral_true = current$visceral_true,
      recent_visceral = current$recent_visceral,
      cum_visceral = current$cum_visceral,
      insulin_true = current$insulin_true,
      recent_log_insulin = current$recent_log_insulin,
      high_insulin_recent_prop = current$high_insulin_recent_prop,
      fh_crc = list_numeric_or_default(state$family_history, "fh_crc", 0),
      fh_diabetes = list_numeric_or_default(state$family_history, "fh_diabetes", 0),
      genetic_crc_predisposition = list_numeric_or_default(
        state$genetic,
        "genetic_crc_predisposition",
        0
      ),
      latent_crc = state$latent_crc,
      disease = state$disease
    )
    
    h_death <- .simulate_person_compute_mortality_hazard(state)
    
    # Sample competing waiting times.
    t_event <- sample_crc_event_time(h_event)
    t_death <- sample_exponential_wait_time(h_death)
    
    # Check whether CRC occurs first within the interval.
    if (t_event <= gap && t_event < t_death) {
      state$event <- 1L
      event_gap <- t_event
      state$age_at_diagnosis <- state$age + event_gap
      state$cal_time_at_diagnosis <- state$cal_time + event_gap
      
      stage_out <- sample_crc_stage(
        spec = spec,
        memory_model = memory_model,
        ses = state$ses,
        latent_crc = state$latent_crc,
        visceral_true = current$visceral_true,
        insulin_true = current$insulin_true,
        gap = gap,
        cum_visceral = current$cum_visceral,
        recent_log_insulin = current$recent_log_insulin,
        disease = state$disease
      )
      
      state$stage <- stage_out$stage
      break
    }
    
    # Check whether background death occurs first within the interval.
    if (t_death <= gap && t_death < t_event) {
      state$death <- 1L
      state$death_before_event <- 1L
      state$age_at_death <- state$age + t_death
      state$cal_time_at_death <- state$cal_time + t_death
      state$death_cause <- state$mortality_name
      break
    }
    
    # No event in this interval, so advance to the next visit time.
    state$age <- state$age + gap
    state$cal_time <- state$cal_time + gap
    state$visit <- state$visit + 1L
  }
  
  patient <- .simulate_person_build_patient_row(
    id = id,
    state = state,
    spec = spec,
    memory_model = memory_model
  )
  
  long <- bind_rows_or_empty(rows)
  
  list(
    patient = patient,
    long = long
  )
}

# Convenience wrapper for a short-memory person simulation.
simulate_person_short <- function(id,
                                  spec,
                                  disease = NULL,
                                  mortality_name = NULL,
                                  recent_window_years = 2,
                                  gap_min = 0.40,
                                  gap_mean_base = 1.2,
                                  gap_mean_low_ses_add = 0.25,
                                  gap_mean_abs_latent_add = 0.10,
                                  gap_sdlog = 0.30) {
  simulate_person(
    id = id,
    spec = spec,
    disease = disease,
    memory_model = "short",
    mortality_name = mortality_name,
    recent_window_years = recent_window_years,
    gap_min = gap_min,
    gap_mean_base = gap_mean_base,
    gap_mean_low_ses_add = gap_mean_low_ses_add,
    gap_mean_abs_latent_add = gap_mean_abs_latent_add,
    gap_sdlog = gap_sdlog
  )
}

# Convenience wrapper for a long-memory person simulation.
simulate_person_long <- function(id,
                                 spec,
                                 disease = NULL,
                                 mortality_name = NULL,
                                 recent_window_years = 2,
                                 gap_min = 0.40,
                                 gap_mean_base = 1.2,
                                 gap_mean_low_ses_add = 0.25,
                                 gap_mean_abs_latent_add = 0.10,
                                 gap_sdlog = 0.30) {
  simulate_person(
    id = id,
    spec = spec,
    disease = disease,
    memory_model = "long",
    mortality_name = mortality_name,
    recent_window_years = recent_window_years,
    gap_min = gap_min,
    gap_mean_base = gap_mean_base,
    gap_mean_low_ses_add = gap_mean_low_ses_add,
    gap_mean_abs_latent_add = gap_mean_abs_latent_add,
    gap_sdlog = gap_sdlog
  )
}
