# Simulate cohorts under the current modular tumour-simulator architecture.
#
# This script contains the cohort-level wrappers around the person-level
# simulator. It is responsible for:
# - optionally setting the random seed
# - simulating multiple people in sequence
# - binding person-level and visit-level outputs
# - sorting outputs into a stable order
#
# Main user-facing functions:
# - simulate_cohort()
# - simulate_cohort_short()
# - simulate_cohort_long()
#
# Current design notes:
# - this script is intentionally thin
# - person-level dynamics belong in simulate_person.R
# - output shaping is kept minimal here so later output_builders.R can remain
#   focused on summaries, tables, and diagnostics
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()
#   - is_scalar_integerish()
# - from utils_sampling.R:
#   - bind_rows_or_empty()
# - from simulate_person.R:
#   - simulate_person()
#   - simulate_person_short()
#   - simulate_person_long()

# Sort a data frame by the supplied column names if possible.
#
# If sort_cols is NULL or empty, the input data frame is returned unchanged.
.simulate_cohort_sort_df <- function(df,
                                     sort_cols = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  
  if (is.null(sort_cols) || length(sort_cols) == 0) {
    return(df)
  }
  
  if (!all(sort_cols %in% names(df))) {
    return(df)
  }
  
  ord_args <- unname(df[sort_cols])
  ord <- do.call(order, ord_args)
  
  out <- df[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Return the preferred sort columns for the patient-level output.
#
# Preferred ordering is by id if present. If not, return NULL and leave the
# data frame unchanged.
.simulate_cohort_get_patient_sort_cols <- function(patient_df) {
  if (!is.data.frame(patient_df) || nrow(patient_df) == 0) {
    return(NULL)
  }
  
  if ("id" %in% names(patient_df)) {
    return("id")
  }
  
  NULL
}

# Return the preferred sort columns for the visit-level output.
#
# Preferred ordering is:
# - id
# - cal_time_raw
# - age_raw
#
# If those are not all available, fall back through a small priority list.
.simulate_cohort_get_long_sort_cols <- function(long_df) {
  if (!is.data.frame(long_df) || nrow(long_df) == 0) {
    return(NULL)
  }
  
  preferred <- c("id", "cal_time_raw", "age_raw")
  if (all(preferred %in% names(long_df))) {
    return(preferred)
  }
  
  fallback_1 <- c("id", "visit")
  if (all(fallback_1 %in% names(long_df))) {
    return(fallback_1)
  }
  
  fallback_2 <- c("id", "cal_time")
  if (all(fallback_2 %in% names(long_df))) {
    return(fallback_2)
  }
  
  if ("id" %in% names(long_df)) {
    return("id")
  }
  
  NULL
}

# Simulate a cohort under the current modular architecture.
#
# Arguments:
# - n:
#   number of people to simulate
# - spec:
#   validated simulation specification
# - disease:
#   optional disease name; if NULL, simulate_person() chooses one
# - memory_model:
#   either "short" or "long"
# - mortality_name:
#   optional mortality model name
# - id_start:
#   first person identifier to use
# - seed:
#   optional random seed
# - recent_window_years:
#   width of the recent-history window used for long-memory summaries
# - visit-gap parameters:
#   current compatibility parameters controlling the visit-gap process
#
# Returned value:
# - a list with:
#   - patient
#   - long
simulate_cohort <- function(n,
                            spec,
                            disease = NULL,
                            memory_model = c("short", "long"),
                            mortality_name = NULL,
                            id_start = 1,
                            seed = NULL,
                            recent_window_years = 2,
                            gap_min = 0.40,
                            gap_mean_base = 1.2,
                            gap_mean_low_ses_add = 0.25,
                            gap_mean_abs_latent_add = 0.10,
                            gap_sdlog = 0.30) {
  
  if (!is_scalar_integerish(n) || n < 0) {
    stop("`n` must be a non-negative integer-valued numeric scalar.", call. = FALSE)
  }
  
  if (!is.list(spec)) {
    stop("`spec` must be a nested list.", call. = FALSE)
  }
  
  if (!is_scalar_integerish(id_start)) {
    stop("`id_start` must be an integer-valued numeric scalar.", call. = FALSE)
  }
  
  if (!is.null(seed) && !is_scalar_number(seed)) {
    stop("`seed` must be NULL or a numeric scalar.", call. = FALSE)
  }
  
  memory_model <- match.arg(memory_model)
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  if (n == 0) {
    return(list(
      patient = data.frame(),
      long = data.frame()
    ))
  }
  
  out <- lapply(
    seq_len(n),
    function(i) {
      simulate_person(
        id = id_start + i - 1,
        spec = spec,
        disease = disease,
        memory_model = memory_model,
        mortality_name = mortality_name,
        recent_window_years = recent_window_years,
        gap_min = gap_min,
        gap_mean_base = gap_mean_base,
        gap_mean_low_ses_add = gap_mean_low_ses_add,
        gap_mean_abs_latent_add = gap_mean_abs_latent_add,
        gap_sdlog = gap_sdlog
      )
    }
  )
  
  patient <- bind_rows_or_empty(lapply(out, `[[`, "patient"))
  long <- bind_rows_or_empty(lapply(out, `[[`, "long"))
  
  patient <- .simulate_cohort_sort_df(
    df = patient,
    sort_cols = .simulate_cohort_get_patient_sort_cols(patient)
  )
  
  long <- .simulate_cohort_sort_df(
    df = long,
    sort_cols = .simulate_cohort_get_long_sort_cols(long)
  )
  
  list(
    patient = patient,
    long = long
  )
}

# Convenience wrapper for short-memory cohort simulation.
simulate_cohort_short <- function(n,
                                  spec,
                                  disease = NULL,
                                  mortality_name = NULL,
                                  id_start = 1,
                                  seed = NULL,
                                  recent_window_years = 2,
                                  gap_min = 0.40,
                                  gap_mean_base = 1.2,
                                  gap_mean_low_ses_add = 0.25,
                                  gap_mean_abs_latent_add = 0.10,
                                  gap_sdlog = 0.30) {
  simulate_cohort(
    n = n,
    spec = spec,
    disease = disease,
    memory_model = "short",
    mortality_name = mortality_name,
    id_start = id_start,
    seed = seed,
    recent_window_years = recent_window_years,
    gap_min = gap_min,
    gap_mean_base = gap_mean_base,
    gap_mean_low_ses_add = gap_mean_low_ses_add,
    gap_mean_abs_latent_add = gap_mean_abs_latent_add,
    gap_sdlog = gap_sdlog
  )
}

# Convenience wrapper for long-memory cohort simulation.
simulate_cohort_long <- function(n,
                                 spec,
                                 disease = NULL,
                                 mortality_name = NULL,
                                 id_start = 1,
                                 seed = NULL,
                                 recent_window_years = 2,
                                 gap_min = 0.40,
                                 gap_mean_base = 1.2,
                                 gap_mean_low_ses_add = 0.25,
                                 gap_mean_abs_latent_add = 0.10,
                                 gap_sdlog = 0.30) {
  simulate_cohort(
    n = n,
    spec = spec,
    disease = disease,
    memory_model = "long",
    mortality_name = mortality_name,
    id_start = id_start,
    seed = seed,
    recent_window_years = recent_window_years,
    gap_min = gap_min,
    gap_mean_base = gap_mean_base,
    gap_mean_low_ses_add = gap_mean_low_ses_add,
    gap_mean_abs_latent_add = gap_mean_abs_latent_add,
    gap_sdlog = gap_sdlog
  )
}
