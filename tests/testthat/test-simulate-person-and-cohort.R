# Tests for person-level and cohort-level simulation using the modular stack.

testthat::skip_if_not_installed("jsonlite")
testthat::skip_if_not_installed("jsonvalidate")
testthat::skip_if_not_installed("EnvStats")

testthat::test_that("simulate_person_long returns expected structure", {
  spec <- .test_load_spec()
  
  set.seed(123)
  
  out <- simulate_person_long(
    id = 1,
    spec = spec
  )
  
  testthat::expect_true(is.list(out))
  testthat::expect_true(all(c("patient", "long") %in% names(out)))
  
  testthat::expect_true(is.data.frame(out$patient))
  testthat::expect_equal(nrow(out$patient), 1)
  
  testthat::expect_true(is.data.frame(out$long))
  testthat::expect_true(nrow(out$long) >= 1)
  
  required_patient_cols <- c(
    "id", "disease", "event_name", "memory_model",
    "sex", "education", "ses", "ethnicity", "geography",
    "event", "death", "control",
    "entry_age", "entry_cal_time",
    "censor_age", "censor_cal_time"
  )
  
  required_long_cols <- c(
    "id", "visit", "age", "cal_time",
    "sex", "education", "ses", "ethnicity", "geography",
    "alcohol_state", "alcohol_score", "alcohol_units",
    "obesity_target_prob", "obese_indicator",
    "visceral_adipose", "fasting_insulin"
  )
  
  testthat::expect_true(all(required_patient_cols %in% names(out$patient)))
  testthat::expect_true(all(required_long_cols %in% names(out$long)))
  testthat::expect_true(unique(out$patient$memory_model) == "long")
})

testthat::test_that("simulate_person_short returns expected structure", {
  spec <- .test_load_spec()
  
  set.seed(456)
  
  out <- simulate_person_short(
    id = 2,
    spec = spec
  )
  
  testthat::expect_equal(nrow(out$patient), 1)
  testthat::expect_true(out$patient$memory_model == "short")
  testthat::expect_true(nrow(out$long) >= 1)
})

testthat::test_that("small long-memory cohort simulation respects key constraints", {
  spec <- .test_load_spec()
  
  out <- simulate_cohort_long(
    n = 50,
    spec = spec,
    seed = 123
  )
  
  patient <- out$patient
  long <- out$long
  
  testthat::expect_true(is.data.frame(patient))
  testthat::expect_true(is.data.frame(long))
  
  testthat::expect_equal(nrow(patient), 50)
  testthat::expect_equal(length(unique(patient$id)), 50)
  
  testthat::expect_true(all(long$id %in% patient$id))
  testthat::expect_true(all(patient$memory_model == "long"))
  
  testthat::expect_true(all(patient$entry_age >= 20))
  testthat::expect_true(all(patient$entry_age <= 49))
  testthat::expect_true(all(patient$censor_age <= 50 + 1e-8))
  
  testthat::expect_true(all(patient$event %in% c(0, 1)))
  testthat::expect_true(all(patient$death %in% c(0, 1)))
  testthat::expect_true(all((patient$event + patient$death) <= 1))
  
  testthat::expect_true("eo_crc" %in% names(patient))
  testthat::expect_true(all(patient$eo_crc == patient$event))
  
  prob_cols <- c("prob_non_drinker", "prob_moderate", "prob_hazardous")
  prob_mat <- as.matrix(long[, prob_cols, drop = FALSE])
  
  testthat::expect_true(all(prob_mat >= 0, na.rm = TRUE))
  testthat::expect_true(all(prob_mat <= 1, na.rm = TRUE))
  testthat::expect_equal(
    rowSums(prob_mat),
    rep(1, nrow(prob_mat)),
    tolerance = 1e-8
  )
})

testthat::test_that("small short-memory cohort simulation runs and has expected structure", {
  spec <- .test_load_spec()
  
  out <- simulate_cohort_short(
    n = 50,
    spec = spec,
    seed = 456
  )
  
  patient <- out$patient
  long <- out$long
  
  testthat::expect_equal(nrow(patient), 50)
  testthat::expect_true(all(patient$memory_model == "short"))
  testthat::expect_true(nrow(long) >= 50)
  
  testthat::expect_true(all(c(
    "prob_non_drinker", "prob_moderate", "prob_hazardous"
  ) %in% names(long)))
  
  testthat::expect_false(any(c(
    "recent_mean_alcohol_score", "cum_mean_alcohol_score", "haz_recent_prop"
  ) %in% setdiff(names(long), character(0))))
  
  testthat::expect_true(all(patient$control == 1 - patient$event))
})

testthat::test_that("cohort outputs are sorted by id and visit order", {
  spec <- .test_load_spec()
  
  out <- simulate_cohort_long(
    n = 30,
    spec = spec,
    seed = 789
  )
  
  patient <- out$patient
  long <- out$long
  
  testthat::expect_true(is.unsorted(patient$id) == FALSE)
  
  ordering <- order(long$id, long$cal_time_raw, long$age_raw)
  testthat::expect_identical(ordering, seq_len(nrow(long)))
})

testthat::test_that("simulate_cohort handles n = 0", {
  spec <- .test_load_spec()
  
  out <- simulate_cohort_long(
    n = 0,
    spec = spec,
    seed = 101
  )
  
  testthat::expect_true(is.list(out))
  testthat::expect_true(is.data.frame(out$patient))
  testthat::expect_true(is.data.frame(out$long))
  testthat::expect_equal(nrow(out$patient), 0)
  testthat::expect_equal(nrow(out$long), 0)
})
