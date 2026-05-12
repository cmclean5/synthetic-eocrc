# General sampling and row-binding utilities for the tumour simulator.
#
# This script provides small reusable helpers that are needed across multiple
# modules, including:
# - probability-map normalisation
# - categorical sampling from named probabilities
# - truncated normal sampling
# - exponential waiting-time sampling
# - safe row-binding of data frames
#
# Expected dependency:
# - utils_math.R should be sourced before this file so that helpers such as:
#   - as_named_numeric()
#   - is_scalar_number()
#   - clamp_unit_interval()
# are available

# Check that the core math helper functions are available.
.utils_sampling_require_math_helpers <- function() {
  required_funs <- c(
    "as_named_numeric",
    "is_scalar_number"
  )
  
  missing_funs <- required_funs[!vapply(
    required_funs,
    exists,
    logical(1),
    mode = "function"
  )]
  
  if (length(missing_funs) > 0) {
    stop(
      "utils_sampling.R requires the following functions from utils_math.R:\n",
      paste(" -", missing_funs, collapse = "\n"),
      "\nSource or load utils_math.R before using this file.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

# Normalise a named probability map so it sums to 1.
#
# The input must be coercible to a named numeric vector.
#
# Arguments:
# - prob_map:
#   named probability map
# - allow_zero_sum:
#   if TRUE and the total is zero, return the original vector unchanged
#   if FALSE and the total is zero, stop with an error
# - context:
#   text used in error messages
normalize_named_probs <- function(prob_map,
                                  allow_zero_sum = FALSE,
                                  context = "Probability map") {
  .utils_sampling_require_math_helpers()
  
  probs <- as_named_numeric(prob_map)
  
  if (is.null(probs) || length(probs) == 0) {
    stop(context, " must be a non-empty named numeric object.", call. = FALSE)
  }
  
  if (any(probs < 0, na.rm = TRUE)) {
    stop(context, " cannot contain negative values.", call. = FALSE)
  }
  
  total <- sum(probs)
  
  if (total == 0) {
    if (isTRUE(allow_zero_sum)) {
      return(probs)
    }
    
    stop(context, " cannot sum to zero.", call. = FALSE)
  }
  
  probs / total
}

# Draw one category from a named probability map.
#
# The map is normalised before sampling as a safeguard against rounding error.
sample_from_named_probs <- function(prob_map,
                                    context = "Probability map") {
  probs <- normalize_named_probs(
    prob_map = prob_map,
    allow_zero_sum = FALSE,
    context = context
  )
  
  sample(names(probs), size = 1, prob = probs)
}

# Sample from a truncated normal distribution.
#
# This is a thin wrapper around EnvStats::rnormTrunc so that package checks
# and argument validation are kept in one place.
rnorm_trunc <- function(n = 1, mean, sd, min, max) {
  .utils_sampling_require_math_helpers()
  
  if (!requireNamespace("EnvStats", quietly = TRUE)) {
    stop(
      "Package 'EnvStats' is required for truncated normal sampling.",
      call. = FALSE
    )
  }
  
  if (!all(vapply(c(n, mean, sd, min, max), is_scalar_number, logical(1)))) {
    stop(
      "rnorm_trunc() requires scalar numeric values for n, mean, sd, min, and max.",
      call. = FALSE
    )
  }
  
  if (n <= 0 || n != as.integer(n)) {
    stop("`n` must be a positive integer-valued numeric scalar.", call. = FALSE)
  }
  
  if (sd <= 0) {
    stop("`sd` must be > 0.", call. = FALSE)
  }
  
  if (max <= min) {
    stop("`max` must be greater than `min`.", call. = FALSE)
  }
  
  EnvStats::rnormTrunc(
    n = n,
    mean = mean,
    sd = sd,
    min = min,
    max = max
  )
}

# Sample an exponential waiting time from a hazard rate.
#
# Behaviour:
# - if rate > 0, sample from rexp(rate = rate)
# - if rate == 0, return Inf
#
# This is useful for interval-level event-time sampling in competing-risk models.
sample_exponential_wait_time <- function(rate) {
  .utils_sampling_require_math_helpers()
  
  if (!is_scalar_number(rate) || rate < 0) {
    stop("`rate` must be a non-negative numeric scalar.", call. = FALSE)
  }
  
  if (rate == 0) {
    return(Inf)
  }
  
  rexp(1, rate = rate)
}

# Safely row-bind a list of data frames.
#
# If the input list is empty, return an empty data frame rather than erroring.
bind_rows_or_empty <- function(x) {
  if (length(x) == 0) {
    return(data.frame())
  }
  
  out <- do.call(rbind, x)
  rownames(out) <- NULL
  out
}
