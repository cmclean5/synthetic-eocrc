# Assumes:
# - modules have been sourced (so simulate_cohort_long + read_sim_spec exist)
# - we are in a clean-ish environment where we can define spec globally

# load scripts
source(here::here("scripts", "load_scripts.R"))

# 1) Create `spec` by reading the JSON config (IMPORTANT: variable name is `spec`)
spec <- read_sim_spec(here::here("inst", "configs", "colorectal_eo.json"))
validate_sim_spec(spec)

# 2) Choose CRC parameters
target_cases <- 10
batch_n      <- 10000L
max_batches  <- 100L
id_start     <- 1L
seed         <- 123L

disease <- "crc"
mortality_name <- "death_other"
memory_model <- "long"  # for info/consistency

if (!exists("simulate_cohort_long")) {
  stop("`simulate_cohort_long` is not found. Make sure R/simulate_cohort.R was sourced.", call. = FALSE)
}

# 3) Determine which event column represents CRC
event_name <- get_crc_event_name(spec, disease = disease)

# 4) Helper: call simulate_cohort_long() correctly depending on its signature
simulate_one_batch <- function(n, id_start) {
  fmls <- names(formals(simulate_cohort_long))
  
  # If simulate_cohort_long has a `spec` formal, pass it.
  if ("spec" %in% fmls) {
    simulate_cohort_long(
      n = n,
      spec = spec,
      disease = disease,
      mortality_name = mortality_name,
      id_start = id_start,
      seed = NULL
    )
  } else {
    # Otherwise it likely relies on the global `spec` variable.
    simulate_cohort_long(
      n = n,
      disease = disease,
      mortality_name = mortality_name,
      id_start = id_start,
      seed = NULL
    )
  }
}

# 5) Run the case-target loop
if (!is.null(seed)) set.seed(seed)

patient_parts <- list()
long_parts <- list()
current_id_start <- id_start

source_cohort <- NULL

for (b in seq_len(max_batches)) {
  message("Running batch ", b, " (id_start=", current_id_start, ") ...")
  
  sim_batch <- simulate_one_batch(n = batch_n, id_start = current_id_start)
  
  patient_parts[[b]] <- sim_batch$patient
  long_parts[[b]]    <- sim_batch$long
  current_id_start   <- current_id_start + batch_n
  
  source_patient <- bind_rows_or_empty(patient_parts)
  source_long    <- bind_rows_or_empty(long_parts)
  
  # Count CRC cases using the detected event column
  n_cases <- sum(source_patient[[event_name]] == 1, na.rm = TRUE)
  
  # Optional: calibration rate summary if calibration_tables.R is sourced
  if (exists("calibration_table_overall_summary")) {
    sim_out_tmp <- list(patient = source_patient, long = source_long)
    overall_summary <- calibration_table_overall_summary(sim_out = sim_out_tmp, event_var = event_name)
    
    message(
      "Batch ", b, ": n_cases=", n_cases,
      " rate_per_100k_py=", round(overall_summary$rate_per_100k_py[1], 3),
      " (CI ",
      round(overall_summary$cl_rate_per_100k_py[1], 3), "-",
      round(overall_summary$cu_rate_per_100k_py[1], 3),
      " )"
    )
  } else {
    message("Batch ", b, ": n_cases=", n_cases)
  }
  
  if (is.finite(n_cases) && n_cases >= target_cases) {
    source_cohort <- list(
      patient = source_patient,
      long = source_long
    )
    break
  }
}

if (is.null(source_cohort)) {
  stop("Unable to reach target_cases within max_batches.", call. = FALSE)
}

source_cohort
