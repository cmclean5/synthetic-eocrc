# General mathematical and coercion utilities for the tumour simulator.
#
# This script provides small reusable helpers that are needed across multiple
# modules, including:
# - scalar checks
# - vector coercion helpers
# - named numeric coercion helpers
# - inverse-logit and softmax transforms
# - shared list-extraction helpers
# - generic coefficient and linear-predictor helpers
#
# These functions are intended to replace duplicated local helpers across:
# - config validation
# - rule resolution
# - state initialisation
# - exposure modules
# - disease modules
# - observation models
# - person and cohort simulation

# Return the right-hand value if the left-hand value is NULL.
#
# This is a small convenience helper used throughout the simulator to provide
# defaults while preserving non-NULL values.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Check whether an object is a single non-missing numeric value.
is_scalar_number <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x)
}

# Check whether an object is a single non-missing integer-like numeric value.
#
# This is useful when validating counts, indices, or seeds.
is_scalar_integerish <- function(x) {
  is_scalar_number(x) && (x == as.integer(x))
}

# Check whether an object is a single non-empty character string.
is_scalar_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

# Check whether an object is either:
# - NULL
# - a single non-missing numeric value
#
# This is useful for optional scalar arguments that may be omitted.
is_numeric_or_null <- function(x) {
  is.null(x) || is_scalar_number(x)
}

# Convert an object to a character vector if possible.
#
# This supports either:
# - a regular character vector
# - a JSON-style list of scalar strings produced by:
#   jsonlite::read_json(..., simplifyVector = FALSE)
#
# If conversion is not possible, return NULL.
as_character_vector <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  
  if (is.character(x)) {
    return(x)
  }
  
  if (is.list(x)) {
    vals <- vapply(
      x,
      function(el) {
        if (is_scalar_string(el)) {
          as.character(el)
        } else {
          NA_character_
        }
      },
      character(1)
    )
    
    if (anyNA(vals)) {
      return(NULL)
    }
    
    return(unname(vals))
  }
  
  NULL
}

# Convert an object to a numeric vector if possible.
#
# This supports either:
# - a regular numeric vector
# - a JSON-style list of scalar numerics produced by:
#   jsonlite::read_json(..., simplifyVector = FALSE)
#
# If conversion is not possible, return NULL.
as_numeric_vector <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  
  if (is.numeric(x)) {
    return(x)
  }
  
  if (is.list(x)) {
    vals <- vapply(
      x,
      function(el) {
        if (is_scalar_number(el)) {
          as.numeric(el)
        } else {
          NA_real_
        }
      },
      numeric(1)
    )
    
    if (anyNA(vals)) {
      return(NULL)
    }
    
    return(unname(vals))
  }
  
  NULL
}

# Convert an object to a named numeric vector if possible.
#
# This supports either:
# - a named numeric vector
# - a named list of scalar numeric values
#
# If conversion is not possible, return NULL.
as_named_numeric <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  
  if (is.numeric(x) && !is.null(names(x)) && all(nzchar(names(x)))) {
    return(x)
  }
  
  if (is.list(x) && !is.data.frame(x) && length(x) > 0 && !is.null(names(x))) {
    vals <- vapply(
      x,
      function(el) {
        if (is_scalar_number(el)) {
          as.numeric(el)
        } else {
          NA_real_
        }
      },
      numeric(1)
    )
    
    if (anyNA(vals) || any(!nzchar(names(vals)))) {
      return(NULL)
    }
    
    return(vals)
  }
  
  NULL
}

# Return a list element if present, otherwise return a fallback.
#
# This is the most generic shared list-extraction helper and is useful whenever
# a downstream module wants a simple safe read from a named list.
list_element_or_default <- function(x, name, fallback = NULL) {
  if (!is.list(x) || is.null(name) || length(name) != 1 || is.na(name)) {
    return(fallback)
  }
  
  if (is.null(x[[name]])) {
    return(fallback)
  }
  
  x[[name]]
}

# Return a numeric scalar from a named list if present and valid.
#
# If the field is absent or not a valid numeric scalar, return `fallback`.
#
# This is intended to replace repeated local helpers such as:
# - feature-or-default extraction from history feature lists
# - top-level scalar extraction from state$family_history
# - top-level scalar extraction from state$genetic
list_numeric_or_default <- function(x, name, fallback = NA_real_) {
  value <- list_element_or_default(x, name, fallback = NULL)
  
  if (is.null(value) || !is_scalar_number(value)) {
    return(fallback)
  }
  
  as.numeric(value)
}

# Return a string scalar from a named list if present and valid.
#
# If the field is absent or not a valid non-empty string, return `fallback`.
list_string_or_default <- function(x, name, fallback = NA_character_) {
  value <- list_element_or_default(x, name, fallback = NULL)
  
  if (is.null(value) || !is_scalar_string(value)) {
    return(fallback)
  }
  
  as.character(value)
}

# Clamp a numeric value or vector to the unit interval [0, 1].
clamp_unit_interval <- function(x) {
  pmin(pmax(x, 0), 1)
}

# Convert a log-odds value to a probability.
invlogit <- function(x) {
  1 / (1 + exp(-x))
}

# Convert a vector of unnormalised log-scores into probabilities summing to 1.
#
# This is useful for categorical-state models defined on the log-score scale.
softmax <- function(x) {
  z <- x - max(x)
  exp(z) / sum(exp(z))
}

# Convert a coefficient specification into a numeric contribution.
#
# Supported forms are:
# - scalar numeric coefficient with a scalar numeric or logical covariate
# - named numeric map with a categorical covariate
#
# Design choice:
# - if a covariate is absent from the context, the contribution is 0
# - if a categorical level is not present in the coefficient map, the
#   contribution is 0
#
# This makes it safe to use shared coefficient lists where only some terms are
# relevant for a given model or memory setting.
coef_contribution <- function(coef_spec, covariate_value) {
  if (is.null(covariate_value)) {
    return(0)
  }
  
  if (is_scalar_number(coef_spec)) {
    if (is.logical(covariate_value) && length(covariate_value) == 1 && !is.na(covariate_value)) {
      return(as.numeric(coef_spec) * as.numeric(covariate_value))
    }
    
    if (is_scalar_number(covariate_value)) {
      return(as.numeric(coef_spec) * as.numeric(covariate_value))
    }
    
    return(0)
  }
  
  coef_map <- as_named_numeric(coef_spec)
  
  if (!is.null(coef_map)) {
    key <- as.character(covariate_value)[1]
    
    if (is.na(key) || !(key %in% names(coef_map))) {
      return(0)
    }
    
    return(unname(coef_map[[key]]))
  }
  
  stop("Unsupported coefficient specification.", call. = FALSE)
}

# Evaluate a linear predictor from an intercept, coefficient list, and context.
#
# Context is a named list of covariate values. Numeric terms are multiplied by
# scalar coefficients. Categorical terms are matched against named coefficient
# maps.
linear_predictor <- function(intercept = 0,
                             coefficients = list(),
                             context = list()) {
  lp <- if (is_scalar_number(intercept)) {
    as.numeric(intercept)
  } else {
    0
  }
  
  if (is.null(coefficients)) {
    return(lp)
  }
  
  if (is.numeric(coefficients) && !is.null(names(coefficients))) {
    coefficients <- as.list(coefficients)
  }
  
  if (!is.list(coefficients) || length(coefficients) == 0) {
    return(lp)
  }
  
  for (nm in names(coefficients)) {
    lp <- lp + coef_contribution(
      coef_spec = coefficients[[nm]],
      covariate_value = context[[nm]]
    )
  }
  
  lp
}
