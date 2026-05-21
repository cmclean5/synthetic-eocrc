# Source the full module stack in dependency order.
#source_module_stack <- function() {
  source(here::here("R", "utils_math.R"))
  source(here::here("R", "utils_sampling.R"))
  source(here::here("R", "config_validate.R"))
  source(here::here("R", "config_read.R"))
  source(here::here("R", "config_accessors.R"))
  source(here::here("R", "dimensions.R"))
  source(here::here("R", "interpolation.R"))
  source(here::here("R", "rule_resolver.R"))
  source(here::here("R", "history_features.R"))
  source(here::here("R", "exposure_alcohol.R"))
  source(here::here("R", "exposure_adiposity.R"))
  source(here::here("R", "exposure_insulin.R"))
  source(here::here("R", "observation_models.R"))
  source(here::here("R", "state_init.R"))
  source(here::here("R", "disease_crc.R"))
  source(here::here("R", "state_update.R"))
  source(here::here("R", "simulate_person.R"))
  source(here::here("R", "simulate_cohort.R"))
  
  ##invisible(TRUE)
#}