# Canonical selector and dimension helpers for rule matching.
#
# This script centralises the meaning of subgroup selectors used by the
# simulator. Its main current use is rule resolution, where a rule may apply
# only to a subset of people defined by selectors such as:
# - age_range
# - sex
# - ses
# - education
# - ethnicity
# - geography
# - disease
#
# Main internal helpers:
# - .dimensions_build_context()
# - .dimensions_rule_matches()
# - .dimensions_rule_specificity()
# - .dimensions_selector_key()
#
# Current design notes:
# - a missing selector means "applies to all"
# - a present scalar selector requires exact match to the supplied context
# - age_range is interpreted inclusively at both ends
# - selector specificity is the number of explicitly constrained dimensions
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()
#   - as_numeric_vector()

# Return the non-age selector names used by the current rule system.
#
# Age is handled separately because it is represented as an interval
# (`age_range`) rather than a single scalar selector value.
.dimensions_rule_selector_names <- function() {
  c("sex", "ses", "education", "ethnicity", "geography", "disease")
}

# Build a selector context list used for rule matching.
#
# Each element may be NULL if not supplied. A missing context value only matters
# when the rule explicitly constrains that dimension.
.dimensions_build_context <- function(age = NULL,
                                      sex = NULL,
                                      ses = NULL,
                                      education = NULL,
                                      ethnicity = NULL,
                                      geography = NULL,
                                      disease = NULL) {
  list(
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    disease = disease
  )
}

# Match a scalar selector value against the supplied context value.
#
# Behaviour:
# - if the selector is missing, it matches everything
# - if the selector is present but the context value is missing, it does not
#   match
# - otherwise matching is exact after conversion to character
#
# The character conversion keeps the behaviour aligned with the previous
# resolver logic and avoids accidental mismatch due only to type differences
# such as factor vs character.
.dimensions_match_scalar_selector <- function(selector_value,
                                              context_value) {
  if (is.null(selector_value)) {
    return(TRUE)
  }
  
  if (is.null(context_value)) {
    return(FALSE)
  }
  
  identical(as.character(selector_value), as.character(context_value))
}

# Check whether a selector list matches the requested context.
#
# Behaviour:
# - a missing selector means "applies to all"
# - a present scalar selector means exact match is required
# - age_range requires age to be supplied and to lie within the range
#
# Age-range interpretation:
# - lower and upper bounds are treated as inclusive
# - a small tolerance is allowed to avoid edge-case floating-point failures
.dimensions_selector_matches <- function(selectors,
                                         context,
                                         age_tol = 1e-8) {
  selectors <- selectors %||% list()
  
  if (!is.list(selectors)) {
    return(FALSE)
  }
  
  if (!is.null(selectors$age_range)) {
    age_range <- as_numeric_vector(selectors$age_range)
    
    if (is.null(age_range) || length(age_range) != 2 || anyNA(age_range)) {
      return(FALSE)
    }
    
    if (!is_scalar_number(context$age)) {
      return(FALSE)
    }
    
    if (context$age < age_range[1] - age_tol ||
        context$age > age_range[2] + age_tol) {
      return(FALSE)
    }
  }
  
  for (nm in .dimensions_rule_selector_names()) {
    if (!.dimensions_match_scalar_selector(selectors[[nm]], context[[nm]])) {
      return(FALSE)
    }
  }
  
  TRUE
}

# Check whether a rule's selectors match the requested context.
#
# This is a thin wrapper around .dimensions_selector_matches() so that calling
# code can work directly with the full rule record.
.dimensions_rule_matches <- function(rule,
                                     context,
                                     age_tol = 1e-8) {
  .dimensions_selector_matches(
    selectors = rule$selectors %||% list(),
    context = context,
    age_tol = age_tol
  )
}

# Compute a selector-specificity score for a selector list.
#
# The score is the number of selector dimensions explicitly constrained by the
# rule.
#
# Higher score means more specific.
.dimensions_selector_specificity <- function(selectors) {
  selectors <- selectors %||% list()
  
  if (!is.list(selectors)) {
    return(0L)
  }
  
  score <- 0L
  
  if (!is.null(selectors$age_range)) {
    score <- score + 1L
  }
  
  for (nm in .dimensions_rule_selector_names()) {
    if (!is.null(selectors[[nm]])) {
      score <- score + 1L
    }
  }
  
  score
}

# Compute a selector-specificity score for a full rule record.
#
# This is a convenience wrapper so downstream code can work directly with
# complete rules rather than manually extracting `rule$selectors`.
.dimensions_rule_specificity <- function(rule) {
  .dimensions_selector_specificity(rule$selectors %||% list())
}

# Convert a selector list into a readable key for debugging.
#
# This is mainly used in candidate summaries and diagnostic output. It produces
# a stable text representation such as:
# - "<all>"
# - "sex=Female|ses=Low"
# - "age_range=30:39|sex=Male"
.dimensions_selector_key <- function(selectors) {
  if (is.null(selectors) || length(selectors) == 0) {
    return("<all>")
  }
  
  nms <- sort(names(selectors))
  
  parts <- vapply(
    nms,
    function(nm) {
      value <- selectors[[nm]]
      
      if (is.list(value)) {
        value <- paste(as.character(unlist(value, use.names = FALSE)), collapse = ":")
      } else {
        value <- as.character(value)
      }
      
      paste0(nm, "=", value)
    },
    character(1)
  )
  
  paste(parts, collapse = "|")
}
