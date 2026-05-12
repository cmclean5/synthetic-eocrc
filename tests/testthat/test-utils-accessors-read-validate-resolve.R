# Tests for utility helpers, config accessors, spec reading, validation,
# and rule resolution.

testthat::skip_if_not_installed("jsonlite")
testthat::skip_if_not_installed("jsonvalidate")

testthat::test_that("utility helpers behave as expected", {
  .test_source_module_stack()
  
  testthat::expect_true(is_scalar_number(1.25))
  testthat::expect_false(is_scalar_number(c(1, 2)))
  testthat::expect_true(is_scalar_integerish(5))
  testthat::expect_false(is_scalar_integerish(5.2))
  testthat::expect_true(is_scalar_string("abc"))
  testthat::expect_false(is_scalar_string(""))
  
  probs <- softmax(c(0, 1, 2))
  testthat::expect_equal(sum(probs), 1, tolerance = 1e-8)
  testthat::expect_true(probs[3] > probs[2])
  testthat::expect_true(probs[2] > probs[1])
  
  testthat::expect_equal(
    list_numeric_or_default(list(a = 2.5), "a", 0),
    2.5
  )
  
  testthat::expect_equal(
    list_numeric_or_default(list(a = "x"), "a", 0),
    0
  )
  
  testthat::expect_equal(
    list_numeric_or_default(list(), "a", 7),
    7
  )
})

testthat::test_that("colorectal example spec reads and validates successfully", {
  spec <- .test_load_spec()
  
  testthat::expect_s3_class(spec, "sim_spec")
  testthat::expect_true(is.list(spec))
  
  testthat::expect_true(isTRUE(attr(spec, "schema_validated")))
  testthat::expect_true(isTRUE(attr(spec, "semantic_validated")))
  
  validation <- attr(spec, "validation")
  
  testthat::expect_true(is.list(validation))
  testthat::expect_true(isTRUE(validation$valid))
  testthat::expect_length(validation$errors, 0)
})

testthat::test_that("config accessors return expected blocks and names", {
  spec <- .test_load_spec()
  
  study <- get_study_spec(spec)
  population <- get_population_spec(spec)
  latent_traits <- get_latent_traits_spec(spec)
  alcohol_spec <- get_alcohol_spec(spec)
  adiposity_spec <- get_adiposity_spec(spec)
  insulin_spec <- get_insulin_spec(spec)
  diseases <- get_diseases_spec(spec)
  mortality <- get_mortality_models_spec(spec)
  
  testthat::expect_true(is.list(study))
  testthat::expect_true(is.list(population))
  testthat::expect_true(is.list(latent_traits))
  testthat::expect_true(is.list(alcohol_spec))
  testthat::expect_true(is.list(adiposity_spec))
  testthat::expect_true(is.list(insulin_spec))
  testthat::expect_true(is.list(diseases))
  testthat::expect_true(is.list(mortality))
  
  testthat::expect_equal(study$calendar_start, 1995)
  testthat::expect_equal(get_disease_name(spec), "crc")
  testthat::expect_true("crc" %in% get_disease_names(spec))
  testthat::expect_true("crc" %in% get_enabled_disease_names(spec))
  testthat::expect_true(get_mortality_name(spec) %in% get_mortality_names(spec))
})

testthat::test_that("alcohol state probability rule resolves to valid probabilities", {
  spec <- .test_load_spec()
  
  probs <- resolve_rule(
    spec = spec,
    target = "alcohol.state_probs",
    year = 2018,
    age = 35,
    ses = "Low"
  )
  
  testthat::expect_true(is.numeric(probs))
  testthat::expect_equal(length(probs), 3)
  testthat::expect_true(all(c("non_drinker", "moderate_drinker", "hazardous_drinker") %in% names(probs)))
  testthat::expect_true(all(probs >= 0))
  testthat::expect_true(all(probs <= 1))
  testthat::expect_equal(sum(probs), 1, tolerance = 1e-8)
})

testthat::test_that("obesity base probability resolves to a scalar in [0, 1]", {
  spec <- .test_load_spec()
  
  p_obesity <- resolve_rule(
    spec = spec,
    target = "adiposity.obesity_probability.base",
    year = 2015,
    sex = "Male",
    ses = "Low"
  )
  
  testthat::expect_true(is.numeric(p_obesity))
  testthat::expect_length(p_obesity, 1)
  testthat::expect_true(p_obesity >= 0)
  testthat::expect_true(p_obesity <= 1)
})

testthat::test_that("CRC APC resolves to the expected piecewise log offset", {
  spec <- .test_load_spec()
  
  year <- 2018
  
  observed <- resolve_rule(
    spec = spec,
    target = "disease.crc.apc",
    year = year,
    disease = "crc"
  )
  
  expected <- log1p(0.005) * (2010 - 1995) +
    log1p(0.01) * (year - 2010)
  
  testthat::expect_true(is.numeric(observed))
  testthat::expect_length(observed, 1)
  testthat::expect_equal(observed, expected, tolerance = 1e-10)
})

testthat::test_that("candidate rule inspection returns true matching alcohol candidates", {
  spec <- .test_load_spec()
  
  candidates <- resolve_rule_candidates(
    spec = spec,
    target = "alcohol.state_probs",
    year = 2020,
    age = 35,
    ses = "Low"
  )
  
  testthat::expect_true(is.data.frame(candidates))
  testthat::expect_true(nrow(candidates) >= 2)
  testthat::expect_true(all(candidates$matched_selectors))
  testthat::expect_true(all(candidates$can_evaluate))
  testthat::expect_true(all(candidates$target == "alcohol.state_probs"))
  testthat::expect_true(any(grepl("ses=Low", candidates$selector_key, fixed = TRUE)))
})

testthat::test_that("SES-specific alcohol rules produce different results for Low and High SES", {
  spec <- .test_load_spec()
  
  low_probs <- resolve_rule(
    spec = spec,
    target = "alcohol.state_probs",
    year = 2025,
    age = 35,
    ses = "Low"
  )
  
  high_probs <- resolve_rule(
    spec = spec,
    target = "alcohol.state_probs",
    year = 2025,
    age = 35,
    ses = "High"
  )
  
  testthat::expect_false(isTRUE(all.equal(low_probs, high_probs)))
  testthat::expect_true(low_probs["non_drinker"] > high_probs["non_drinker"])
})
