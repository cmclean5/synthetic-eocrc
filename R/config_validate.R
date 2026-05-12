# Semantic validation for tumour simulation specifications.
#
# This validator is intended to run after JSON Schema validation.
# JSON Schema checks the broad structure of the file, while this script checks
# semantic conditions that are difficult to enforce in JSON Schema alone.
#
# Examples of semantic checks handled here include:
# - probability vectors summing to 1
# - age and calendar bounds being in the correct order
# - ethnicity and geography assignment rules matching declared levels
# - anchor years being strictly increasing and lying within rule periods
# - disease time-trend targets matching declared rules
# - rule ambiguity caused by overlapping periods and identical priority
#
# Main function:
#   validate_sim_spec(spec)
#
# Expected input:
# - `spec` should be a nested R list
# - ideally read with jsonlite::read_json(..., simplifyVector = FALSE)
#
# Default behaviour:
# - stop with an error if validation fails
# - return the original `spec` unchanged if validation succeeds
# - attach a "validation" attribute containing:
#   - valid
#   - errors
#   - warnings

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Check whether an object is a single non-missing numeric value.
.sim_is_scalar_number <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x)
}

# Check whether an object is a single non-empty character string.
.sim_is_scalar_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

# Convert an object to a character vector if possible.
#
# This supports either:
# - a regular character vector
# - a JSON-style list of scalar strings produced by:
#   jsonlite::read_json(..., simplifyVector = FALSE)
#
# If conversion is not possible, return NULL.
.sim_as_character_vector <- function(x) {
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
        if (.sim_is_scalar_string(el)) {
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
.sim_as_numeric_vector <- function(x) {
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
        if (.sim_is_scalar_number(el)) {
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
# - a named list whose elements are scalar numeric values
#
# If conversion is not possible, return NULL.
.sim_as_named_numeric <- function(x) {
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
        if (.sim_is_scalar_number(el)) el else NA_real_
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

# Convert an object into a list of record-like lists.
#
# This helps normalise JSON-derived structures that may appear as:
# - a list of lists
# - a data frame
# - an empty list
#
# The validator works internally with a plain list of record-like lists.
.sim_as_record_list <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  }
  
  if (is.list(x) && length(x) == 0) {
    return(list())
  }
  
  if (is.list(x) && all(vapply(x, is.list, logical(1)))) {
    return(x)
  }
  
  list()
}

# Convert a selector value into a simple comparable string.
#
# This is used when constructing selector signatures for rule ambiguity checks.
.sim_selector_value_as_string <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return("")
  }
  
  paste(as.character(unlist(x, use.names = FALSE)), collapse = ":")
}

# Build a canonical key representing a selector combination.
#
# Examples:
# - empty selectors become "<all>"
# - non-empty selectors are sorted by name and concatenated
#
# This lets the validator compare selector patterns between rules.
.sim_selector_key <- function(selectors) {
  if (is.null(selectors) || length(selectors) == 0) {
    return("<all>")
  }
  
  nms <- sort(names(selectors))
  
  parts <- vapply(
    nms,
    function(nm) {
      paste0(nm, "=", .sim_selector_value_as_string(selectors[[nm]]))
    },
    character(1)
  )
  
  paste(parts, collapse = "|")
}

# Check whether two periods overlap strictly.
#
# If any endpoint is missing or non-numeric, return FALSE rather than erroring.
# This keeps validation robust even when earlier parts of the spec are invalid.
.sim_periods_overlap_strict <- function(start1, end1, start2, end2) {
  if (!all(vapply(c(start1, end1, start2, end2), .sim_is_scalar_number, logical(1)))) {
    return(FALSE)
  }
  
  max(start1, start2) < min(end1, end2)
}

# Attach validation results as an attribute to the returned spec object.
.sim_add_validation_attr <- function(spec, valid, errors, warnings) {
  attr(spec, "validation") <- list(
    valid = valid,
    errors = unique(errors),
    warnings = unique(warnings)
  )
  spec
}

# Format multiple validation messages into a readable block.
.sim_format_messages <- function(header, messages) {
  paste(c(header, paste0(" - ", unique(messages))), collapse = "\n")
}

# Validate a simulation specification.
#
# Arguments:
# - spec: nested list containing the simulation spec
# - error_on_fail: if TRUE, stop on any validation error
# - warn_on_nonfatal: if TRUE, emit warnings for non-fatal problems
# - tol: numerical tolerance for probability sums and boundary checks
# - return_result: if TRUE, return a validation result list instead of the spec
#
# Returns:
# - by default, the original spec with a "validation" attribute
# - if return_result = TRUE, a list with valid/errors/warnings
validate_sim_spec <- function(spec,
                              error_on_fail = TRUE,
                              warn_on_nonfatal = TRUE,
                              tol = 1e-8,
                              return_result = FALSE) {
  # Store validation messages as they are discovered.
  errors <- character(0)
  warnings <- character(0)
  
  # Local helper to append an error message.
  add_error <- function(...) {
    errors <<- c(errors, paste0(...))
  }
  
  # Local helper to append a warning message.
  add_warning <- function(...) {
    warnings <<- c(warnings, paste0(...))
  }
  
  # Validate a named probability map.
  #
  # This function is used for objects such as:
  # - sex probabilities
  # - education probabilities
  # - SES conditional probabilities
  # - stage probability maps
  #
  # It checks:
  # - object can be interpreted as named numeric values
  # - names match the expected set if provided
  # - values lie in [0, 1]
  # - values sum to 1 if required
  check_prob_map <- function(x, path, expected_names = NULL, require_sum_1 = TRUE) {
    vals <- .sim_as_named_numeric(x)
    
    if (is.null(vals)) {
      add_error(path, " must be a named numeric map.")
      return(invisible(NULL))
    }
    
    if (!is.null(expected_names)) {
      if (!setequal(names(vals), expected_names) ||
          length(vals) != length(expected_names)) {
        add_error(
          path, " must have exactly these names: ",
          paste(expected_names, collapse = ", "), "."
        )
      }
    }
    
    if (any(vals < -tol) || any(vals > 1 + tol)) {
      add_error(path, " probabilities must lie in [0, 1].")
    }
    
    if (require_sum_1 && abs(sum(vals) - 1) > tol) {
      add_error(
        path, " probabilities must sum to 1; found ",
        signif(sum(vals), 8), "."
      )
    }
    
    invisible(vals)
  }
  
  # Validate a single scalar probability.
  check_scalar_probability <- function(x, path) {
    if (!.sim_is_scalar_number(x)) {
      add_error(path, " must be a numeric scalar.")
      return(invisible(NULL))
    }
    
    if (x < -tol || x > 1 + tol) {
      add_error(path, " must lie in [0, 1].")
    }
    
    invisible(x)
  }
  
  # Validate a generic attribute dimension such as ethnicity or geography.
  #
  # The schema allows either:
  # - a fixed assignment, where every simulated person gets the same value
  # - a categorical assignment, where values are sampled from a probability map
  #
  # This function checks:
  # - levels are non-empty, unique strings
  # - fixed values belong to declared levels
  # - categorical probabilities match declared levels and sum to 1
  check_attribute_dimension <- function(dim_spec, path) {
    if (!is.list(dim_spec)) {
      add_error(path, " must be a list.")
      return(invisible(NULL))
    }
    
    levels <- .sim_as_character_vector(dim_spec$levels)
    
    if (is.null(levels) || length(levels) < 1 || anyNA(levels) ||
        any(!nzchar(levels)) || anyDuplicated(levels)) {
      add_error(path, ".levels must be a non-empty character vector with unique values.")
    }
    
    assignment <- dim_spec$assignment
    
    if (!is.list(assignment)) {
      add_error(path, ".assignment must be a list.")
      return(invisible(NULL))
    }
    
    method <- assignment$method
    
    if (identical(method, "fixed")) {
      value <- assignment$value
      
      if (!.sim_is_scalar_string(value)) {
        add_error(path, ".assignment.value must be a non-empty string for fixed assignment.")
      } else if (!is.null(levels) && !(value %in% levels)) {
        add_error(
          path, ".assignment.value ('", value,
          "') must be one of: ", paste(levels, collapse = ", "), "."
        )
      }
    } else if (identical(method, "categorical")) {
      expected_names <- if (!is.null(levels) && length(levels) > 0) levels else NULL
      
      check_prob_map(
        assignment$probs,
        paste0(path, ".assignment.probs"),
        expected_names = expected_names,
        require_sum_1 = TRUE
      )
    } else {
      add_error(path, ".assignment.method must be either 'fixed' or 'categorical'.")
    }
  }
  
  # Validate a target value for alcohol state probabilities.
  #
  # This is a special case because the value must be a named probability map
  # with names exactly matching the declared alcohol states when available.
  check_state_prob_value <- function(value, path, expected_states) {
    expected_names <- if (!is.null(expected_states) && length(expected_states) > 0) {
      expected_states
    } else {
      NULL
    }
    
    check_prob_map(
      value,
      path = path,
      expected_names = expected_names,
      require_sum_1 = TRUE
    )
  }
  
  # Validate a generic parameter value appearing inside a rule.
  #
  # This function adapts validation to different target types:
  # - alcohol.state_probs must match alcohol states and sum to 1
  # - identity-scale probability-like targets should be in [0, 1]
  # - otherwise the value must be either a scalar numeric or named numeric map
  check_parameter_value <- function(value, path, target, scale, alcohol_states) {
    if (identical(target, "alcohol.state_probs") && identical(scale, "identity")) {
      check_state_prob_value(value, path, alcohol_states)
      return(invisible(NULL))
    }
    
    if (grepl("prob", target, ignore.case = TRUE) && identical(scale, "identity")) {
      if (.sim_is_scalar_number(value)) {
        check_scalar_probability(value, path)
        return(invisible(NULL))
      }
      
      named_vals <- .sim_as_named_numeric(value)
      
      if (!is.null(named_vals)) {
        if (any(named_vals < -tol) || any(named_vals > 1 + tol)) {
          add_error(path, " contains probability values outside [0, 1].")
        }
        return(invisible(NULL))
      }
    }
    
    if (.sim_is_scalar_number(value)) {
      return(invisible(NULL))
    }
    
    named_vals <- .sim_as_named_numeric(value)
    
    if (!is.null(named_vals)) {
      return(invisible(NULL))
    }
    
    add_error(path, " must be either a numeric scalar or a named numeric map.")
    invisible(NULL)
  }
  
  # The validator expects a nested list-like object.
  if (!is.list(spec)) {
    stop("`spec` must be a nested list.", call. = FALSE)
  }
  
  # Define canonical levels that are reused in multiple checks.
  sex_levels <- c("Female", "Male")
  ordered_3_levels <- c("Low", "Medium", "High")
  
  # Extract top-level sections once so they can be checked and reused.
  meta <- spec$meta
  study <- spec$study
  simulation <- spec$simulation
  population <- spec$population
  latent_traits <- spec$latent_traits
  exposures <- spec$exposures
  diseases <- spec$diseases
  mortality <- spec$mortality
  rules <- .sim_as_record_list(spec$rules)
  
  # Validate study settings.
  if (!is.list(study)) {
    add_error("study must be a list.")
  } else {
    if (.sim_is_scalar_number(study$calendar_start) &&
        .sim_is_scalar_number(study$calendar_end)) {
      if (study$calendar_end <= study$calendar_start) {
        add_error("study.calendar_end must be greater than study.calendar_start.")
      }
    }
    
    if (.sim_is_scalar_number(study$max_age) && study$max_age <= 0) {
      add_error("study.max_age must be > 0.")
    }
    
    if (.sim_is_scalar_number(study$min_followup_time) && study$min_followup_time < 0) {
      add_error("study.min_followup_time must be >= 0.")
    }
  }
  
  # Validate simulation settings, especially memory-model configuration.
  supported_memory_models <- character(0)
  
  if (!is.list(simulation)) {
    add_error("simulation must be a list.")
  } else {
    supported_memory_models <- .sim_as_character_vector(
      simulation$supported_memory_models %||% character(0)
    )
    supported_memory_models <- supported_memory_models %||% character(0)
    
    default_memory_model <- simulation$default_memory_model
    
    if (!default_memory_model %in% supported_memory_models) {
      add_error(
        "simulation.default_memory_model ('", default_memory_model,
        "') must be included in simulation.supported_memory_models."
      )
    }
  }
  
  # If the simulation block is invalid or absent, use a fallback so later checks
  # can still proceed without failing unexpectedly.
  if (length(supported_memory_models) == 0) {
    supported_memory_models <- c("short", "long")
  }
  
  # Validate population-generation settings.
  if (!is.list(population)) {
    add_error("population must be a list.")
  } else {
    # Check entry age distribution parameters.
    entry_age <- population$entry_age
    
    if (is.list(entry_age)) {
      if (.sim_is_scalar_number(entry_age$min) &&
          .sim_is_scalar_number(entry_age$max) &&
          entry_age$max <= entry_age$min) {
        add_error("population.entry_age.max must be greater than population.entry_age.min.")
      }
      
      if (.sim_is_scalar_number(entry_age$max) &&
          is.list(study) &&
          .sim_is_scalar_number(study$max_age) &&
          entry_age$max > study$max_age + tol) {
        add_error(
          "population.entry_age.max must be <= study.max_age. Found ",
          entry_age$max, " > ", study$max_age, "."
        )
      }
    }
    
    # Check marginal sex distribution.
    check_prob_map(
      population$sex_probs,
      "population.sex_probs",
      expected_names = sex_levels,
      require_sum_1 = TRUE
    )
    
    # Check marginal education distribution.
    check_prob_map(
      population$education_probs,
      "population.education_probs",
      expected_names = ordered_3_levels,
      require_sum_1 = TRUE
    )
    
    # Check SES conditional on education.
    ses_given_education <- population$ses_given_education
    
    if (!is.list(ses_given_education)) {
      add_error("population.ses_given_education must be a list.")
    } else {
      for (edu in ordered_3_levels) {
        check_prob_map(
          ses_given_education[[edu]],
          paste0("population.ses_given_education.", edu),
          expected_names = ordered_3_levels,
          require_sum_1 = TRUE
        )
      }
    }
    
    # Check ethnicity configuration if present.
    if (!is.null(population$ethnicity)) {
      check_attribute_dimension(population$ethnicity, "population.ethnicity")
    }
    
    # Check geography configuration if present.
    if (!is.null(population$geography)) {
      check_attribute_dimension(population$geography, "population.geography")
    }
  }
  
  # Cache allowed ethnicity and geography levels for later rule-selector checks.
  allowed_ethnicity <- character(0)
  allowed_geography <- character(0)
  ethnicity_enabled <- FALSE
  geography_enabled <- FALSE
  
  if (is.list(population) && is.list(population$ethnicity)) {
    allowed_ethnicity <- .sim_as_character_vector(
      population$ethnicity$levels %||% character(0)
    )
    allowed_ethnicity <- allowed_ethnicity %||% character(0)
    ethnicity_enabled <- isTRUE(population$ethnicity$enabled)
  }
  
  if (is.list(population) && is.list(population$geography)) {
    allowed_geography <- .sim_as_character_vector(
      population$geography$levels %||% character(0)
    )
    allowed_geography <- allowed_geography %||% character(0)
    geography_enabled <- isTRUE(population$geography$enabled)
  }
  
  # Validate latent trait models.
  if (!is.list(latent_traits) || length(latent_traits) == 0) {
    add_error("latent_traits must be a non-empty list.")
  } else {
    for (nm in names(latent_traits)) {
      lt <- latent_traits[[nm]]
      
      if (!is.list(lt)) {
        add_error("latent_traits.", nm, " must be a list.")
        next
      }
      
      if (.sim_is_scalar_number(lt$sd) && lt$sd <= 0) {
        add_error("latent_traits.", nm, ".sd must be > 0.")
      }
    }
  }
  
  # Validate exposure modules.
  alcohol_states <- character(0)
  
  if (!is.list(exposures)) {
    add_error("exposures must be a list.")
  } else {
    # Validate alcohol exposure structure.
    alcohol <- exposures$alcohol
    
    if (is.list(alcohol)) {
      alcohol_states <- .sim_as_character_vector(alcohol$states %||% character(0))
      alcohol_states <- alcohol_states %||% character(0)
      
      if (length(alcohol_states) == 0 || anyDuplicated(alcohol_states)) {
        add_error("exposures.alcohol.states must contain unique state names.")
      }
      
      score_map <- .sim_as_named_numeric(alcohol$score_map)
      
      if (is.null(score_map)) {
        add_error("exposures.alcohol.score_map must be a named numeric map.")
      } else {
        if (!setequal(names(score_map), alcohol_states) ||
            length(score_map) != length(alcohol_states)) {
          add_error(
            "exposures.alcohol.score_map names must exactly match exposures.alcohol.states."
          )
        }
      }
      
      units_model_names <- names(alcohol$units_model %||% list())
      
      if (!setequal(units_model_names, alcohol_states) ||
          length(units_model_names) != length(alcohol_states)) {
        add_error(
          "exposures.alcohol.units_model must define exactly one model for each alcohol state."
        )
      } else {
        for (st in alcohol_states) {
          um <- alcohol$units_model[[st]]
          dist <- um$distribution %||% NA_character_
          
          if (identical(dist, "point_mass")) {
            if (!.sim_is_scalar_number(um$value)) {
              add_error("exposures.alcohol.units_model.", st, ".value must be numeric.")
            }
          } else if (identical(dist, "truncated_normal")) {
            for (nm in c("mean", "sd", "min", "max")) {
              if (!.sim_is_scalar_number(um[[nm]])) {
                add_error(
                  "exposures.alcohol.units_model.", st, ".", nm,
                  " must be numeric."
                )
              }
            }
            
            if (.sim_is_scalar_number(um$sd) && um$sd <= 0) {
              add_error("exposures.alcohol.units_model.", st, ".sd must be > 0.")
            }
            
            if (.sim_is_scalar_number(um$min) &&
                .sim_is_scalar_number(um$max) &&
                um$max <= um$min) {
              add_error("exposures.alcohol.units_model.", st, ".max must be > .min.")
            }
          } else {
            add_error(
              "exposures.alcohol.units_model.", st,
              ".distribution must be 'point_mass' or 'truncated_normal'."
            )
          }
        }
      }
      
      mem_names <- names(alcohol$memory_models %||% list())
      
      if (!all(supported_memory_models %in% mem_names)) {
        add_error(
          "exposures.alcohol.memory_models must include: ",
          paste(supported_memory_models, collapse = ", "), "."
        )
      }
    }
    
    # Validate adiposity exposure structure.
    adiposity <- exposures$adiposity
    
    if (is.list(adiposity)) {
      if (.sim_is_scalar_number(adiposity$visceral_sd) && adiposity$visceral_sd <= 0) {
        add_error("exposures.adiposity.visceral_sd must be > 0.")
      }
      
      if (.sim_is_scalar_number(adiposity$obesity_threshold)) {
        bounds <- adiposity$baseline_distribution
        
        if (is.list(bounds) &&
            .sim_is_scalar_number(bounds$min) &&
            .sim_is_scalar_number(bounds$max)) {
          if (bounds$max <= bounds$min) {
            add_error("exposures.adiposity.baseline_distribution.max must be > min.")
          }
          
          if (adiposity$obesity_threshold < bounds$min - tol ||
              adiposity$obesity_threshold > bounds$max + tol) {
            add_warning(
              "exposures.adiposity.obesity_threshold lies outside adiposity baseline bounds."
            )
          }
        }
        
        if (is.list(adiposity$target_mapping) &&
            .sim_is_scalar_number(adiposity$target_mapping$threshold) &&
            abs(adiposity$target_mapping$threshold - adiposity$obesity_threshold) > tol) {
          add_warning(
            "exposures.adiposity.target_mapping.threshold differs from ",
            "exposures.adiposity.obesity_threshold."
          )
        }
      }
      
      if (is.list(adiposity$age_effect) &&
          .sim_is_scalar_number(adiposity$age_effect$age_ref) &&
          is.list(study) &&
          .sim_is_scalar_number(study$max_age) &&
          adiposity$age_effect$age_ref > study$max_age + tol) {
        add_warning("exposures.adiposity.age_effect.age_ref is greater than study.max_age.")
      }
      
      dyn_names <- names(adiposity$dynamics %||% list())
      
      if (!all(supported_memory_models %in% dyn_names)) {
        add_error("exposures.adiposity.dynamics must include all supported memory models.")
      }
    }
    
    # Validate insulin exposure structure.
    insulin <- exposures$insulin
    
    if (is.list(insulin)) {
      init <- insulin$initial_state
      
      if (is.list(init) &&
          .sim_is_scalar_number(init$min_log) &&
          .sim_is_scalar_number(init$max_log) &&
          init$max_log <= init$min_log) {
        add_error("exposures.insulin.initial_state.max_log must be > min_log.")
      }
      
      if (is.list(init) &&
          .sim_is_scalar_number(insulin$high_log_threshold) &&
          .sim_is_scalar_number(init$min_log) &&
          .sim_is_scalar_number(init$max_log)) {
        if (insulin$high_log_threshold < init$min_log - tol ||
            insulin$high_log_threshold > init$max_log + tol) {
          add_warning(
            "exposures.insulin.high_log_threshold lies outside the insulin initial_state range."
          )
        }
      }
      
      dyn_names <- names(insulin$dynamics %||% list())
      
      if (!all(supported_memory_models %in% dyn_names)) {
        add_error("exposures.insulin.dynamics must include all supported memory models.")
      }
      
      for (mm in names(insulin$dynamics %||% list())) {
        dyn <- insulin$dynamics[[mm]]
        
        if (is.list(dyn) &&
            .sim_is_scalar_number(dyn$min_log) &&
            .sim_is_scalar_number(dyn$max_log) &&
            dyn$max_log <= dyn$min_log) {
          add_error(
            "exposures.insulin.dynamics.", mm, ".max_log must be > min_log."
          )
        }
      }
    }
  }
  
  # Validate disease modules.
  disease_names <- if (is.list(diseases)) names(diseases) else character(0)
  
  if (!is.list(diseases) || length(disease_names) == 0) {
    add_error("diseases must be a non-empty named list.")
  } else {
    enabled_diseases <- disease_names[vapply(
      diseases,
      function(x) isTRUE(x$enabled),
      logical(1)
    )]
    
    if (length(enabled_diseases) == 0) {
      add_error("At least one disease must have enabled = true.")
    }
    
    # This is a warning rather than an error because the metadata tumour label
    # may be a broader description than the internal disease key.
    meta_tumour_type <- if (is.list(meta)) meta$tumour_type else NULL
    
    if (.sim_is_scalar_string(meta_tumour_type) &&
        !(meta_tumour_type %in% disease_names)) {
      add_warning(
        "meta.tumour_type ('", meta_tumour_type,
        "') is not one of the disease names: ",
        paste(disease_names, collapse = ", "), "."
      )
    }
    
    # Disease event names should be unique so event tables remain unambiguous.
    event_names <- vapply(
      diseases,
      function(d) as.character(d$event_name %||% NA_character_),
      character(1)
    )
    
    event_names_non_na <- event_names[!is.na(event_names)]
    
    if (anyDuplicated(event_names_non_na)) {
      add_error("Disease event_name values must be unique.")
    }
    
    # Collect rule targets so disease time-trend references can be checked.
    rule_targets <- vapply(
      rules,
      function(r) as.character(r$target %||% NA_character_),
      character(1)
    )
    rule_targets <- rule_targets[!is.na(rule_targets)]
    
    for (dname in disease_names) {
      d <- diseases[[dname]]
      
      inc <- d$incidence_model
      
      if (is.list(inc)) {
        tt <- inc$time_trend_target
        
        if (.sim_is_scalar_string(tt) && !(tt %in% rule_targets)) {
          add_error(
            "diseases.", dname, ".incidence_model.time_trend_target ('", tt,
            "') does not match any rule target."
          )
        }
      }
      
      stage_model <- d$stage_model
      
      if (is.list(stage_model)) {
        check_prob_map(
          stage_model$early_stage_probs,
          paste0("diseases.", dname, ".stage_model.early_stage_probs"),
          require_sum_1 = TRUE
        )
        
        check_prob_map(
          stage_model$advanced_stage_probs,
          paste0("diseases.", dname, ".stage_model.advanced_stage_probs"),
          require_sum_1 = TRUE
        )
      }
    }
  }
  
  # Validate mortality models at a basic level.
  if (!is.list(mortality) || length(mortality) == 0) {
    add_error("mortality must be a non-empty named list.")
  }
  
  # Validate external rule definitions.
  if (length(rules) == 0) {
    add_error("rules must be a non-empty list of rule objects.")
  } else {
    for (i in seq_along(rules)) {
      rule <- rules[[i]]
      path <- paste0("rules[[", i, "]]")
      
      target <- rule$target %||% ""
      scale <- rule$scale %||% ""
      rule_type <- rule$rule_type %||% ""
      
      # Check the time period attached to the rule.
      period <- rule$period
      
      if (!is.list(period)) {
        add_error(path, ".period must be a list.")
      } else {
        if (.sim_is_scalar_number(period$start) &&
            .sim_is_scalar_number(period$end) &&
            period$end <= period$start) {
          add_error(path, ".period.end must be greater than .period.start.")
        }
      }
      
      # Check selectors used to restrict the rule to subgroups.
      selectors <- rule$selectors %||% list()
      
      if (!is.list(selectors)) {
        add_error(path, ".selectors must be a list.")
      } else {
        if (!is.null(selectors$age_range)) {
          age_range <- .sim_as_numeric_vector(selectors$age_range)
          
          if (is.null(age_range) || length(age_range) != 2 || anyNA(age_range)) {
            add_error(path, ".selectors.age_range must be a numeric vector of length 2.")
          } else if (age_range[2] <= age_range[1]) {
            add_error(path, ".selectors.age_range upper bound must be > lower bound.")
          }
        }
        
        if (!is.null(selectors$sex) && !(selectors$sex %in% sex_levels)) {
          add_error(
            path, ".selectors.sex must be one of: ",
            paste(sex_levels, collapse = ", "), "."
          )
        }
        
        if (!is.null(selectors$ses) && !(selectors$ses %in% ordered_3_levels)) {
          add_error(
            path, ".selectors.ses must be one of: ",
            paste(ordered_3_levels, collapse = ", "), "."
          )
        }
        
        if (!is.null(selectors$education) && !(selectors$education %in% ordered_3_levels)) {
          add_error(
            path, ".selectors.education must be one of: ",
            paste(ordered_3_levels, collapse = ", "), "."
          )
        }
        
        if (!is.null(selectors$ethnicity)) {
          if (!ethnicity_enabled) {
            add_error(
              path, ".selectors.ethnicity is used but population.ethnicity.enabled = false."
            )
          } else if (!(selectors$ethnicity %in% allowed_ethnicity)) {
            add_error(
              path, ".selectors.ethnicity ('", selectors$ethnicity,
              "') must be one of: ",
              paste(allowed_ethnicity, collapse = ", "), "."
            )
          }
        }
        
        if (!is.null(selectors$geography)) {
          if (!geography_enabled) {
            add_error(
              path, ".selectors.geography is used but population.geography.enabled = false."
            )
          } else if (!(selectors$geography %in% allowed_geography)) {
            add_error(
              path, ".selectors.geography ('", selectors$geography,
              "') must be one of: ",
              paste(allowed_geography, collapse = ", "), "."
            )
          }
        }
        
        if (!is.null(selectors$disease) && !(selectors$disease %in% disease_names)) {
          add_error(
            path, ".selectors.disease ('", selectors$disease,
            "') must be one of: ", paste(disease_names, collapse = ", "), "."
          )
        }
      }
      
      # If the target string begins with "disease.<name>", validate that the
      # disease name exists in the declared disease list.
      if (.sim_is_scalar_string(target) && grepl("^disease\\.", target)) {
        parts <- strsplit(target, "\\.")[[1]]
        
        if (length(parts) >= 2) {
          dname <- parts[2]
          
          if (!(dname %in% disease_names)) {
            add_error(
              path, ".target ('", target,
              "') references unknown disease '", dname, "'."
            )
          }
        }
      }
      
      # Validate rule content according to the declared rule type.
      if (identical(rule_type, "constant")) {
        check_parameter_value(
          rule$value,
          paste0(path, ".value"),
          target = target,
          scale = scale,
          alcohol_states = alcohol_states
        )
      }
      
      if (identical(rule_type, "anchor_points")) {
        anchors <- .sim_as_record_list(rule$anchors)
        
        if (length(anchors) < 2) {
          add_error(path, ".anchors must contain at least 2 anchor points.")
        } else {
          years <- rep(NA_real_, length(anchors))
          
          for (j in seq_along(anchors)) {
            anchor <- anchors[[j]]
            
            years[j] <- if (.sim_is_scalar_number(anchor$year)) {
              anchor$year
            } else {
              NA_real_
            }
            
            if (!.sim_is_scalar_number(anchor$year)) {
              add_error(path, ".anchors[[", j, "]].year must be numeric.")
            }
            
            check_parameter_value(
              anchor$value,
              paste0(path, ".anchors[[", j, "]].value"),
              target = target,
              scale = scale,
              alcohol_states = alcohol_states
            )
          }
          
          # If all years are valid numerics, perform ordering and boundary checks.
          if (!anyNA(years)) {
            if (any(diff(years) <= 0)) {
              add_error(path, ".anchors years must be strictly increasing.")
            }
            
            if (is.list(period) &&
                .sim_is_scalar_number(period$start) &&
                .sim_is_scalar_number(period$end)) {
              if (any(years < period$start - tol | years > period$end + tol)) {
                add_error(path, ".anchors years must lie within the rule period.")
              }
            }
          }
        }
      }
      
      if (identical(rule_type, "annual_percent_change")) {
        if (!.sim_is_scalar_number(rule$annual_rate)) {
          add_error(path, ".annual_rate must be numeric.")
        } else if (rule$annual_rate <= -1) {
          add_error(path, ".annual_rate must be > -1.")
        }
        
        # This is a warning rather than an error because future use cases may
        # intentionally use another scale, but log is the standard choice.
        if (!identical(scale, "log")) {
          add_warning(
            path, " uses rule_type = 'annual_percent_change' with scale = '",
            scale, "'. Usually 'log' is the intended scale."
          )
        }
      }
    }
    
    # Check for ambiguous rule combinations.
    #
    # The main ambiguity case checked here is:
    # - same target
    # - same selector pattern
    # - same priority
    # - overlapping time periods
    #
    # In those circumstances, the resolver may have no principled way to decide
    # which rule should apply.
    tie_breaker <- if (is.list(spec$rule_resolution)) {
      spec$rule_resolution$tie_breaker %||% "priority"
    } else {
      "priority"
    }
    
    rule_meta <- lapply(seq_along(rules), function(i) {
      r <- rules[[i]]
      
      list(
        index = i,
        target = r$target %||% "",
        selector_key = .sim_selector_key(r$selectors %||% list()),
        priority = r$priority %||% NA_integer_,
        period_start = if (is.list(r$period)) r$period$start %||% NA_real_ else NA_real_,
        period_end = if (is.list(r$period)) r$period$end %||% NA_real_ else NA_real_
      )
    })
    
    if (length(rule_meta) > 1) {
      for (i in seq_len(length(rule_meta) - 1)) {
        for (j in (i + 1):length(rule_meta)) {
          a <- rule_meta[[i]]
          b <- rule_meta[[j]]
          
          same_target <- identical(a$target, b$target)
          same_selector_key <- identical(a$selector_key, b$selector_key)
          same_priority <- !anyNA(c(a$priority, b$priority)) &&
            isTRUE(all.equal(a$priority, b$priority))
          overlap <- .sim_periods_overlap_strict(
            a$period_start, a$period_end,
            b$period_start, b$period_end
          )
          
          if (same_target && same_selector_key && same_priority && overlap) {
            if (identical(tie_breaker, "priority")) {
              add_error(
                "Ambiguous rules: rules[[", a$index, "]] and rules[[", b$index,
                "]] have the same target, selectors, priority, and overlapping periods."
              )
            } else if (identical(tie_breaker, "priority_then_latest_period_start")) {
              same_start <- !anyNA(c(a$period_start, b$period_start)) &&
                isTRUE(all.equal(a$period_start, b$period_start))
              
              if (same_start) {
                add_error(
                  "Ambiguous rules: rules[[", a$index, "]] and rules[[", b$index,
                  "]] have the same target, selectors, priority, overlapping periods, ",
                  "and the same period start."
                )
              }
            }
          }
        }
      }
    }
  }
  
  # Deduplicate collected validation messages.
  errors <- unique(errors)
  warnings <- unique(warnings)
  
  # The spec is considered valid if and only if there are no errors.
  valid <- length(errors) == 0
  
  # Create a plain result object in case the caller requests it.
  result <- list(
    valid = valid,
    errors = errors,
    warnings = warnings
  )
  
  # Emit warnings if requested.
  if (warn_on_nonfatal && length(warnings) > 0) {
    warning(
      .sim_format_messages("validate_sim_spec warnings:", warnings),
      call. = FALSE
    )
  }
  
  # Attach validation results to the returned spec.
  spec <- .sim_add_validation_attr(
    spec = spec,
    valid = valid,
    errors = errors,
    warnings = warnings
  )
  
  # Stop if validation failed and the caller wants strict failure behaviour.
  if (!valid && error_on_fail) {
    stop(
      .sim_format_messages("validate_sim_spec failed:", errors),
      call. = FALSE
    )
  }
  
  # If explicitly requested, return only the validation result object.
  if (isTRUE(return_result)) {
    return(result)
  }
  
  # Otherwise return the original validated spec.
  spec
}


# # Shared accessor helpers for the simulation specification.
# #
# # These functions provide a central, consistent way to retrieve commonly used
# # blocks from the loaded simulation specification. They are intended to reduce
# # repeated "get spec block" helper code across:
# # - state initialisation
# # - exposure modules
# # - disease modules
# # - person/cohort simulation
# #
# # Design choices:
# # - accessors validate that expected blocks exist and are of the right broad type
# # - disease and mortality accessors provide sensible default-selection logic
# # - these functions assume package-style availability of core utilities such as:
# #   - `%||%`
# #   - is_scalar_string()
# 
# # Return the meta block from the simulation specification.
# get_meta_spec <- function(spec) {
#   if (!is.list(spec) || !is.list(spec$meta)) {
#     stop("`spec$meta` must be a list.", call. = FALSE)
#   }
# 
#   spec$meta
# }
# 
# # Return the study block from the simulation specification.
# get_study_spec <- function(spec) {
#   if (!is.list(spec) || !is.list(spec$study)) {
#     stop("`spec$study` must be a list.", call. = FALSE)
#   }
# 
#   spec$study
# }
# 
# # Return the simulation-control block from the simulation specification.
# get_simulation_spec <- function(spec) {
#   if (!is.list(spec) || !is.list(spec$simulation)) {
#     stop("`spec$simulation` must be a list.", call. = FALSE)
#   }
# 
#   spec$simulation
# }
# 
# # Return the rule-resolution block from the simulation specification.
# get_rule_resolution_spec <- function(spec) {
#   if (!is.list(spec) || !is.list(spec$rule_resolution)) {
#     stop("`spec$rule_resolution` must be a list.", call. = FALSE)
#   }
# 
#   spec$rule_resolution
# }
# 
# # Return the population block from the simulation specification.
# get_population_spec <- function(spec) {
#   if (!is.list(spec) || !is.list(spec$population)) {
#     stop("`spec$population` must be a list.", call. = FALSE)
#   }
# 
#   spec$population
# }
# 
# # Return the latent-traits block from the simulation specification.
# get_latent_traits_spec <- function(spec) {
#   if (!is.list(spec) || !is.list(spec$latent_traits)) {
#     stop("`spec$latent_traits` must be a list.", call. = FALSE)
#   }
# 
#   spec$latent_traits
# }
# 
# # Return the full exposures block from the simulation specification.
# get_exposures_spec <- function(spec) {
#   if (!is.list(spec) || !is.list(spec$exposures)) {
#     stop("`spec$exposures` must be a list.", call. = FALSE)
#   }
# 
#   spec$exposures
# }
# 
# # Return one named exposure block from the simulation specification.
# #
# # Arguments:
# # - spec:
# #   simulation specification
# # - exposure_name:
# #   name of the exposure block to retrieve, for example:
# #   - "alcohol"
# #   - "adiposity"
# #   - "insulin"
# get_exposure_spec <- function(spec, exposure_name) {
#   exposures <- get_exposures_spec(spec)
# 
#   if (!is.character(exposure_name) || length(exposure_name) != 1 || is.na(exposure_name) || !nzchar(exposure_name)) {
#     stop("`exposure_name` must be a single non-empty string.", call. = FALSE)
#   }
# 
#   if (is.null(exposures[[exposure_name]]) || !is.list(exposures[[exposure_name]])) {
#     stop(
#       "Exposure specification '", exposure_name, "' is missing or not a list.",
#       call. = FALSE
#     )
#   }
# 
#   exposures[[exposure_name]]
# }
# 
# # Return the alcohol exposure block.
# get_alcohol_spec <- function(spec) {
#   get_exposure_spec(spec, "alcohol")
# }
# 
# # Return the adiposity exposure block.
# get_adiposity_spec <- function(spec) {
#   get_exposure_spec(spec, "adiposity")
# }
# 
# # Return the insulin exposure block.
# get_insulin_spec <- function(spec) {
#   get_exposure_spec(spec, "insulin")
# }
# 
# # Return the diseases block from the simulation specification.
# get_diseases_spec <- function(spec) {
#   if (!is.list(spec) || !is.list(spec$diseases)) {
#     stop("`spec$diseases` must be a list.", call. = FALSE)
#   }
# 
#   spec$diseases
# }
# 
# # Return all disease names from the diseases block.
# get_disease_names <- function(spec) {
#   diseases <- get_diseases_spec(spec)
#   names(diseases)
# }
# 
# # Return the enabled disease names from the diseases block.
# get_enabled_disease_names <- function(spec) {
#   diseases <- get_diseases_spec(spec)
# 
#   names(diseases)[vapply(
#     diseases,
#     function(x) isTRUE(x$enabled),
#     logical(1)
#   )]
# }
# 
# # Return the active disease name.
# #
# # Selection logic:
# # - if `disease` is supplied:
# #   - it must exist in spec$diseases
# #   - and, by default, must be enabled
# # - otherwise:
# #   - use meta.tumour_type if it is an enabled disease
# #   - otherwise use the first enabled disease
# #
# # Arguments:
# # - spec:
# #   simulation specification
# # - disease:
# #   optional disease name
# # - must_be_enabled:
# #   if TRUE, the returned disease must be enabled
# get_disease_name <- function(spec,
#                              disease = NULL,
#                              must_be_enabled = TRUE) {
#   diseases <- get_diseases_spec(spec)
# 
#   if (!is.null(disease)) {
#     if (!is.character(disease) || length(disease) != 1 || is.na(disease) || !nzchar(disease)) {
#       stop("`disease` must be NULL or a single non-empty string.", call. = FALSE)
#     }
# 
#     if (is.null(diseases[[disease]])) {
#       stop("Unknown disease '", disease, "'.", call. = FALSE)
#     }
# 
#     if (isTRUE(must_be_enabled) && !isTRUE(diseases[[disease]]$enabled)) {
#       stop("Disease '", disease, "' is not enabled.", call. = FALSE)
#     }
# 
#     return(disease)
#   }
# 
#   meta <- get_meta_spec(spec)
#   enabled <- get_enabled_disease_names(spec)
#   meta_tumour <- meta$tumour_type
# 
#   if (isTRUE(must_be_enabled)) {
#     if (is_scalar_string(meta_tumour) && meta_tumour %in% enabled) {
#       return(meta_tumour)
#     }
# 
#     if (length(enabled) == 0) {
#       stop("No enabled disease found in `spec$diseases`.", call. = FALSE)
#     }
# 
#     return(enabled[1])
#   }
# 
#   all_names <- names(diseases)
# 
#   if (is_scalar_string(meta_tumour) && meta_tumour %in% all_names) {
#     return(meta_tumour)
#   }
# 
#   if (length(all_names) == 0) {
#     stop("No diseases found in `spec$diseases`.", call. = FALSE)
#   }
# 
#   all_names[1]
# }
# 
# # Return one disease specification block.
# #
# # Arguments:
# # - spec:
# #   simulation specification
# # - disease:
# #   optional disease name; if NULL, get_disease_name() is used
# # - must_be_enabled:
# #   passed to get_disease_name()
# get_disease_spec <- function(spec,
#                              disease = NULL,
#                              must_be_enabled = TRUE) {
#   disease_name <- get_disease_name(
#     spec = spec,
#     disease = disease,
#     must_be_enabled = must_be_enabled
#   )
# 
#   diseases <- get_diseases_spec(spec)
#   disease_spec <- diseases[[disease_name]]
# 
#   if (!is.list(disease_spec)) {
#     stop("Disease specification for '", disease_name, "' must be a list.", call. = FALSE)
#   }
# 
#   disease_spec
# }
# 
# # Return the mortality block from the simulation specification.
# get_mortality_models_spec <- function(spec) {
#   if (!is.list(spec) || !is.list(spec$mortality)) {
#     stop("`spec$mortality` must be a list.", call. = FALSE)
#   }
# 
#   spec$mortality
# }
# 
# # Return all mortality-model names.
# get_mortality_names <- function(spec) {
#   mortality <- get_mortality_models_spec(spec)
#   names(mortality)
# }
# 
# # Return the active mortality-model name.
# #
# # Selection logic:
# # - if `mortality_name` is supplied, it must exist
# # - otherwise use the first mortality model in the spec
# get_mortality_name <- function(spec,
#                                mortality_name = NULL) {
#   mortality <- get_mortality_models_spec(spec)
# 
#   if (!is.null(mortality_name)) {
#     if (!is.character(mortality_name) || length(mortality_name) != 1 || is.na(mortality_name) || !nzchar(mortality_name)) {
#       stop("`mortality_name` must be NULL or a single non-empty string.", call. = FALSE)
#     }
# 
#     if (is.null(mortality[[mortality_name]])) {
#       stop("Unknown mortality model '", mortality_name, "'.", call. = FALSE)
#     }
# 
#     return(mortality_name)
#   }
# 
#   names_mortality <- names(mortality)
# 
#   if (length(names_mortality) == 0) {
#     stop("No mortality model found in `spec$mortality`.", call. = FALSE)
#   }
# 
#   names_mortality[1]
# }
# 
# # Return one mortality-model specification block.
# #
# # Arguments:
# # - spec:
# #   simulation specification
# # - mortality_name:
# #   optional mortality-model name; if NULL, get_mortality_name() is used
# get_mortality_spec <- function(spec,
#                                mortality_name = NULL) {
#   selected_name <- get_mortality_name(
#     spec = spec,
#     mortality_name = mortality_name
#   )
# 
#   mortality <- get_mortality_models_spec(spec)
#   mortality_spec <- mortality[[selected_name]]
# 
#   if (!is.list(mortality_spec)) {
#     stop("Mortality specification for '", selected_name, "' must be a list.", call. = FALSE)
#   }
# 
#   mortality_spec
# }
# 
# # Return the rules array from the simulation specification.
# get_rules_spec <- function(spec) {
#   if (!is.list(spec) || is.null(spec$rules) || !is.list(spec$rules)) {
#     stop("`spec$rules` must be a list.", call. = FALSE)
#   }
# 
#   spec$rules
# }
# 
