# Helper functions for tumour simulator tests.
#
# This helper file is intended to be automatically sourced by testthat before
# the individual test files are run.
#
# It provides:
# - file helpers based on here::here()
# - sourcing of modular scripts in dependency order
# - a shared helper to load the example colorectal specification

testthat::skip_if_not_installed("here")

# Construct an absolute path relative to the project root.
.test_project_file <- function(...) {
  path <- here::here(...)
  
  if (!file.exists(path)) {
    stop(
      "Project file does not exist: ", path,
      call. = FALSE
    )
  }
  
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# Source one script into the global environment.
.test_source_script <- function(script_path) {
  source(script_path, local = globalenv())
}

# Source the full module stack in dependency order.
.test_source_module_stack <- function() {
  .test_source_script(.test_project_file("R", "utils_math.R"))
  .test_source_script(.test_project_file("R", "utils_sampling.R"))
  .test_source_script(.test_project_file("R", "config_validate.R"))
  .test_source_script(.test_project_file("R", "config_read.R"))
  .test_source_script(.test_project_file("R", "config_accessors.R"))
  .test_source_script(.test_project_file("R", "dimensions.R"))
  .test_source_script(.test_project_file("R", "interpolation.R"))
  .test_source_script(.test_project_file("R", "rule_resolver.R"))
  .test_source_script(.test_project_file("R", "history_features.R"))
  .test_source_script(.test_project_file("R", "exposure_alcohol.R"))
  .test_source_script(.test_project_file("R", "exposure_adiposity.R"))
  .test_source_script(.test_project_file("R", "exposure_insulin.R"))
  .test_source_script(.test_project_file("R", "observation_models.R"))
  .test_source_script(.test_project_file("R", "state_init.R"))
  .test_source_script(.test_project_file("R", "disease_crc.R"))
  .test_source_script(.test_project_file("R", "state_update.R"))
  .test_source_script(.test_project_file("R", "simulate_person.R"))
  .test_source_script(.test_project_file("R", "simulate_cohort.R"))
  
  invisible(TRUE)
}

# Load the colorectal example spec with schema and semantic validation.
.test_load_spec <- function() {
  .test_source_module_stack()
  
  config_path <- .test_project_file("inst", "configs", "colorectal_eo.json")
  schema_path <- .test_project_file("inst", "schemas", "sim_spec.schema.json")
  
  read_sim_spec(
    path = config_path,
    schema_path = schema_path,
    validate_schema = TRUE,
    validate_semantics = TRUE
  )
}
