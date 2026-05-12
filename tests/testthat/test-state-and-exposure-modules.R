# Tests for history, state-initialisation, exposure modules,
# observation models, and the CRC disease module.

testthat::skip_if_not_installed("jsonlite")
testthat::skip_if_not_installed("jsonvalidate")
testthat::skip_if_not_installed("EnvStats")

testthat::test_that("history features are computed correctly on a simple example", {
  .test_source_module_stack()
  
  history <- initialize_history_state()
  
  history <- append_history_state(
    history = history,
    cal_time = 2015.2,
    alcohol_score = 1,
    alcohol_units = 8,
    visceral_true = 27.5,
    insulin_true = 9.3
  )
  
  history <- append_history_state(
    history = history,
    cal_time = 2016.4,
    alcohol_score = 2,
    alcohol_units = 22,
    visceral_true = 30.1,
    insulin_true = 12.7
  )
  
  features <- compute_history_features(
    history = history,
    current_time = 2017.0,
    recent_window_years = 2,
    high_log_insulin_threshold = log(15),
    fallback_visceral = 27.5,
    fallback_insulin = 9.3
  )
  
  testthat::expect_equal(features$recent_mean_score, 1.5, tolerance = 1e-8)
  testthat::expect_equal(features$cum_mean_score, 1.5, tolerance = 1e-8)
  testthat::expect_equal(features$haz_recent_prop, 0.5, tolerance = 1e-8)
  testthat::expect_equal(features$recent_visceral, mean(c(27.5, 30.1)), tolerance = 1e-8)
  testthat::expect_equal(features$cum_visceral, mean(c(27.5, 30.1)), tolerance = 1e-8)
  testthat::expect_equal(features$high_insulin_recent_prop, 0, tolerance = 1e-8)
})

testthat::test_that("initialize_person_state returns a coherent baseline state", {
  spec <- .test_load_spec()
  
  set.seed(123)
  st <- initialize_person_state(spec)
  
  required_fields <- c(
    "disease", "mortality_name", "sex", "education", "ses",
    "ethnicity", "geography", "family_history", "genetic",
    "latent_metabolic", "latent_crc", "entry_age", "entry_cal_time",
    "prev_visceral", "prev_insulin", "prev_alcohol_score"
  )
  
  testthat::expect_true(all(required_fields %in% names(st)))
  testthat::expect_true(st$entry_age >= 20)
  testthat::expect_true(st$entry_age <= 49)
  testthat::expect_true(st$prev_visceral >= 8)
  testthat::expect_true(st$prev_visceral <= 70)
  testthat::expect_true(st$prev_insulin > 0)
  testthat::expect_true(st$disease == "crc")
})

testthat::test_that("alcohol exposure module returns valid short-memory updates", {
  spec <- .test_load_spec()
  
  set.seed(123)
  
  alc <- sample_alcohol_exposure_short(
    spec = spec,
    age = 34,
    sex = "Male",
    ses = "Low",
    education = "Medium",
    ethnicity = "Unspecified",
    geography = "Unspecified",
    cal_time = 2018.5,
    latent_metabolic = 0.4,
    prev_score = 1,
    disease = "crc"
  )
  
  states <- get_alcohol_states(spec)
  
  testthat::expect_true(alc$state %in% states)
  testthat::expect_true(is.numeric(alc$score))
  testthat::expect_true(is.numeric(alc$units))
  testthat::expect_true(is.numeric(alc$probs))
  testthat::expect_equal(sum(alc$probs), 1, tolerance = 1e-8)
  
  if (alc$state == "non_drinker") {
    testthat::expect_equal(alc$units, 0)
  }
  
  if (alc$state == "moderate_drinker") {
    testthat::expect_true(alc$units >= 1)
    testthat::expect_true(alc$units <= 14)
  }
  
  if (alc$state == "hazardous_drinker") {
    testthat::expect_true(alc$units >= 15)
    testthat::expect_true(alc$units <= 60)
  }
})

testthat::test_that("alcohol exposure module returns valid long-memory updates", {
  spec <- .test_load_spec()
  
  set.seed(123)
  
  alc <- sample_alcohol_exposure_long(
    spec = spec,
    age = 34,
    sex = "Male",
    ses = "Low",
    education = "Medium",
    ethnicity = "Unspecified",
    geography = "Unspecified",
    cal_time = 2018.5,
    latent_metabolic = 0.4,
    history_features = list(
      recent_mean_score = 1.2,
      cum_mean_score = 0.9,
      haz_recent_prop = 0.4
    ),
    disease = "crc"
  )
  
  states <- get_alcohol_states(spec)
  
  testthat::expect_true(alc$state %in% states)
  testthat::expect_equal(sum(alc$probs), 1, tolerance = 1e-8)
  testthat::expect_equal(alc$recent_mean_score, 1.2)
  testthat::expect_equal(alc$cum_mean_score, 0.9)
  testthat::expect_equal(alc$haz_recent_prop, 0.4)
})

testthat::test_that("adiposity and insulin modules return coherent values", {
  spec <- .test_load_spec()
  
  set.seed(123)
  
  adip_init <- initialize_adiposity_state(
    spec = spec,
    cal_time = 2012.4,
    age = 31.7,
    sex = "Female",
    ses = "Medium",
    education = "High",
    ethnicity = "Unspecified",
    geography = "Unspecified",
    latent_metabolic = 0.3,
    disease = "crc"
  )
  
  testthat::expect_true(adip_init$p_obesity_target_0 >= 0)
  testthat::expect_true(adip_init$p_obesity_target_0 <= 1)
  testthat::expect_true(adip_init$prev_visceral >= 8)
  testthat::expect_true(adip_init$prev_visceral <= 70)
  
  adip_upd <- update_adiposity_exposure_long(
    spec = spec,
    cal_time = 2015.6,
    age = 34.2,
    sex = "Male",
    ses = "Low",
    education = "Medium",
    ethnicity = "Unspecified",
    geography = "Unspecified",
    latent_metabolic = 0.4,
    alcohol_score = 2,
    fh_diabetes = 1,
    prev_visceral = 29.1,
    history_features = list(
      recent_mean_score = 1.1,
      cum_mean_score = 0.9,
      recent_visceral = 28.7
    ),
    disease = "crc"
  )
  
  testthat::expect_true(adip_upd$p_obesity_target >= 0)
  testthat::expect_true(adip_upd$p_obesity_target <= 1)
  testthat::expect_true(adip_upd$visceral_true >= 8)
  testthat::expect_true(adip_upd$visceral_true <= 70)
  testthat::expect_true(adip_upd$obese_indicator %in% c(0, 1))
  
  ins_init <- initialize_insulin_state(
    spec = spec,
    prev_visceral = adip_init$prev_visceral,
    latent_metabolic = 0.3
  )
  
  testthat::expect_true(ins_init$prev_insulin > 0)
  testthat::expect_true(ins_init$high_insulin_indicator %in% c(0, 1))
  
  ins_upd <- update_insulin_exposure_long(
    spec = spec,
    prev_insulin = ins_init$prev_insulin,
    visceral_true = adip_upd$visceral_true,
    latent_metabolic = adip_upd$latent_metabolic,
    alcohol_score = 2,
    fh_diabetes = 1,
    history_features = list(
      recent_visceral = 29.1,
      cum_visceral = 28.4,
      recent_mean_score = 1.0
    )
  )
  
  testthat::expect_true(ins_upd$insulin_true > 0)
  testthat::expect_true(ins_upd$high_insulin_indicator %in% c(0, 1))
  testthat::expect_true(ins_upd$high_log_threshold == get_insulin_high_log_threshold(spec))
})

testthat::test_that("observation models return valid probabilities and observed values", {
  spec <- .test_load_spec()
  
  ctx <- build_observation_context(
    sex = "Male",
    ses = "Low",
    education = "Medium",
    ethnicity = "Unspecified",
    geography = "Unspecified",
    fh_diabetes = 1
  )
  
  p_vis <- compute_adiposity_measurement_probability(
    spec = spec,
    context = ctx
  )
  
  p_ins <- compute_insulin_measurement_probability(
    spec = spec,
    context = ctx
  )
  
  testthat::expect_true(p_vis >= 0 && p_vis <= 1)
  testthat::expect_true(p_ins >= 0 && p_ins <= 1)
  
  set.seed(123)
  
  obs <- generate_current_observations(
    spec = spec,
    visceral_true = 31.27,
    insulin_true = 12.84,
    context = ctx
  )
  
  testthat::expect_true(obs$p_measure_visceral >= 0 && obs$p_measure_visceral <= 1)
  testthat::expect_true(obs$p_measure_insulin >= 0 && obs$p_measure_insulin <= 1)
  testthat::expect_true(obs$visceral_observed_indicator %in% c(0, 1))
  testthat::expect_true(obs$insulin_observed_indicator %in% c(0, 1))
  
  if (!is.na(obs$visceral_obs)) {
    testthat::expect_true(is.numeric(obs$visceral_obs))
  }
  
  if (!is.na(obs$insulin_obs)) {
    testthat::expect_true(is.numeric(obs$insulin_obs))
  }
})

testthat::test_that("CRC disease module returns valid hazards and stage outputs", {
  spec <- .test_load_spec()
  
  h_crc <- compute_crc_hazard(
    spec = spec,
    cal_time = 2018.5,
    memory_model = "long",
    age = 36,
    sex = "Male",
    ses = "Low",
    education = "Medium",
    ethnicity = "Unspecified",
    geography = "Unspecified",
    alcohol_score = 2,
    recent_mean_score = 1.1,
    haz_recent_prop = 0.4,
    visceral_true = 29.8,
    recent_visceral = 28.9,
    cum_visceral = 28.2,
    insulin_true = 11.3,
    recent_log_insulin = log(10.2),
    high_insulin_recent_prop = 0.2,
    fh_crc = 1,
    fh_diabetes = 1,
    genetic_crc_predisposition = 0,
    latent_crc = 0.5,
    disease = "crc"
  )
  
  testthat::expect_true(is.numeric(h_crc))
  testthat::expect_length(h_crc, 1)
  testthat::expect_true(h_crc > 0)
  
  set.seed(123)
  
  stage_out <- sample_crc_stage(
    spec = spec,
    memory_model = "long",
    ses = "Low",
    latent_crc = 0.5,
    visceral_true = 29.8,
    insulin_true = 11.3,
    gap = 1.2,
    cum_visceral = 28.2,
    recent_log_insulin = log(10.2),
    disease = "crc"
  )
  
  testthat::expect_true(stage_out$stage %in% c("I", "II", "III", "IV"))
  testthat::expect_true(stage_out$advanced %in% c(0, 1))
  testthat::expect_true(stage_out$p_advanced >= 0)
  testthat::expect_true(stage_out$p_advanced <= 1)
})
