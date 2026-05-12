# Shared accessor helpers for the simulation specification.
#
# This script provides a central, consistent way to retrieve commonly used
# blocks from a loaded simulation specification. It reduces repeated
# spec-navigation code across:
# - state initialisation
# - exposure modules
# - disease modules
# - simulation modules
#
# Main user-facing functions:
# - get_meta_spec()
# - get_study_spec()
# - get_simulation_spec()
# - get_rule_resolution_spec()
# - get_population_spec()
# - get_latent_traits_spec()
# - get_exposures_spec()
# - get_exposure_spec()
# - get_alcohol_spec()
# - get_adiposity_spec()
# - get_adiposity_reference_value()
# - get_adiposity_latent_age_ref()
# - get_adiposity_target_probability_bounds()
# - get_insulin_spec()
# - get_diseases_spec()
# - get_disease_names()
# - get_enabled_disease_names()
# - get_disease_name()
# - get_disease_spec()
# - get_mortality_models_spec()
# - get_mortality_names()
# - get_mortality_name()
# - get_mortality_spec()
# - get_rules_spec()
#
# Package design note:
# - these functions assume package-style namespace availability of shared
#   helpers such as:
#   - `%||%`
#   - is_scalar_string()
#   - is_scalar_number()
# - they do not use per-file require guards

# Validate a spec-block name used in generic accessors.
#
# This is an internal helper used when retrieving one named block from a parent
# list, such as one exposure block from spec$exposures.
.config_accessors_validate_name <- function(name,
                                            arg_name = "name") {
  if (!is_scalar_string(name)) {
    stop(
      "`", arg_name, "` must be a single non-empty string.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

# Return one named top-level list block from the simulation specification.
#
# Arguments:
# - spec:
#   simulation specification
# - block_name:
#   name of the top-level list element to retrieve
#
# This helper checks that:
# - spec is a list
# - spec[[block_name]] exists
# - spec[[block_name]] is a list
.config_accessors_get_top_level_list_block <- function(spec,
                                                       block_name) {
  .config_accessors_validate_name(block_name, arg_name = "block_name")
  
  if (!is.list(spec)) {
    stop("`spec` must be a list.", call. = FALSE)
  }
  
  block <- spec[[block_name]]
  
  if (!is.list(block)) {
    stop(
      "`spec$", block_name, "` must be a list.",
      call. = FALSE
    )
  }
  
  block
}

# Return one named nested list block from a parent list.
#
# Arguments:
# - parent:
#   parent list
# - parent_name:
#   text name of the parent used in error messages
# - child_name:
#   name of the nested block to retrieve
#
# This helper checks that:
# - parent is a list
# - parent[[child_name]] exists
# - parent[[child_name]] is a list
.config_accessors_get_nested_list_block <- function(parent,
                                                    parent_name,
                                                    child_name) {
  .config_accessors_validate_name(parent_name, arg_name = "parent_name")
  .config_accessors_validate_name(child_name, arg_name = "child_name")
  
  if (!is.list(parent)) {
    stop(
      "`", parent_name, "` must be a list.",
      call. = FALSE
    )
  }
  
  block <- parent[[child_name]]
  
  if (!is.list(block)) {
    stop(
      "`", parent_name, "$", child_name, "` must be a list.",
      call. = FALSE
    )
  }
  
  block
}

# Return the meta block from the simulation specification.
get_meta_spec <- function(spec) {
  .config_accessors_get_top_level_list_block(spec, "meta")
}

# Return the study block from the simulation specification.
get_study_spec <- function(spec) {
  .config_accessors_get_top_level_list_block(spec, "study")
}

# Return the simulation-control block from the simulation specification.
get_simulation_spec <- function(spec) {
  .config_accessors_get_top_level_list_block(spec, "simulation")
}

# Return the rule-resolution block from the simulation specification.
get_rule_resolution_spec <- function(spec) {
  .config_accessors_get_top_level_list_block(spec, "rule_resolution")
}

# Return the population block from the simulation specification.
get_population_spec <- function(spec) {
  .config_accessors_get_top_level_list_block(spec, "population")
}

# Return the latent-traits block from the simulation specification.
get_latent_traits_spec <- function(spec) {
  .config_accessors_get_top_level_list_block(spec, "latent_traits")
}

# Return the full exposures block from the simulation specification.
get_exposures_spec <- function(spec) {
  .config_accessors_get_top_level_list_block(spec, "exposures")
}

# Return one named exposure block from the simulation specification.
#
# Arguments:
# - spec:
#   simulation specification
# - exposure_name:
#   name of the exposure block to retrieve, for example:
#   - "alcohol"
#   - "adiposity"
#   - "insulin"
get_exposure_spec <- function(spec,
                              exposure_name) {
  .config_accessors_validate_name(exposure_name, arg_name = "exposure_name")
  
  exposures <- get_exposures_spec(spec)
  
  .config_accessors_get_nested_list_block(
    parent = exposures,
    parent_name = "spec$exposures",
    child_name = exposure_name
  )
}

# Return the alcohol exposure block.
get_alcohol_spec <- function(spec) {
  get_exposure_spec(spec, "alcohol")
}

# Return the adiposity exposure block.
get_adiposity_spec <- function(spec) {
  get_exposure_spec(spec, "adiposity")
}

# Return the configured adiposity reference value.
#
# This value is the shared centring constant for visceral adiposity used across
# the current simulator, including:
# - adiposity-state updates
# - insulin models
# - CRC incidence and stage contexts
get_adiposity_reference_value <- function(spec) {
  adiposity_spec <- get_adiposity_spec(spec)
  reference_value <- adiposity_spec$reference_value
  
  if (!is_scalar_number(reference_value)) {
    stop(
      "`spec$exposures$adiposity$reference_value` must be a numeric scalar.",
      call. = FALSE
    )
  }
  
  as.numeric(reference_value)
}

# Return the configured adiposity latent-age reference.
#
# This value is currently used when centring age in the latent-metabolic update
# linked to adiposity dynamics.
get_adiposity_latent_age_ref <- function(spec) {
  adiposity_spec <- get_adiposity_spec(spec)
  latent_age_ref <- adiposity_spec$latent_age_ref
  
  if (!is_scalar_number(latent_age_ref)) {
    stop(
      "`spec$exposures$adiposity$latent_age_ref` must be a numeric scalar.",
      call. = FALSE
    )
  }
  
  as.numeric(latent_age_ref)
}

# Return the configured bounds used when clamping obesity target probabilities.
#
# The returned object is a list containing:
# - min
# - max
#
# These bounds are currently used after applying the age effect to the
# rule-resolved baseline obesity probability target.
get_adiposity_target_probability_bounds <- function(spec) {
  adiposity_spec <- get_adiposity_spec(spec)
  bounds <- adiposity_spec$target_probability_bounds
  
  if (!is.list(bounds)) {
    stop(
      "`spec$exposures$adiposity$target_probability_bounds` must be a list.",
      call. = FALSE
    )
  }
  
  min_value <- bounds$min
  max_value <- bounds$max
  
  if (!all(vapply(c(min_value, max_value), is_scalar_number, logical(1)))) {
    stop(
      "`spec$exposures$adiposity$target_probability_bounds` must contain numeric `min` and `max`.",
      call. = FALSE
    )
  }
  
  min_value <- as.numeric(min_value)
  max_value <- as.numeric(max_value)
  
  if (min_value < 0 || min_value > 1 || max_value < 0 || max_value > 1) {
    stop(
      "Adiposity target probability bounds must lie between 0 and 1.",
      call. = FALSE
    )
  }
  
  if (max_value <= min_value) {
    stop(
      "Adiposity target probability bounds require `max` to be greater than `min`.",
      call. = FALSE
    )
  }
  
  list(
    min = min_value,
    max = max_value
  )
}

# Return the insulin exposure block.
get_insulin_spec <- function(spec) {
  get_exposure_spec(spec, "insulin")
}

# Return the diseases block from the simulation specification.
get_diseases_spec <- function(spec) {
  .config_accessors_get_top_level_list_block(spec, "diseases")
}

# Return all disease names from the diseases block.
get_disease_names <- function(spec) {
  diseases <- get_diseases_spec(spec)
  names(diseases)
}

# Return the enabled disease names from the diseases block.
get_enabled_disease_names <- function(spec) {
  diseases <- get_diseases_spec(spec)
  
  names(diseases)[vapply(
    diseases,
    function(x) isTRUE(x$enabled),
    logical(1)
  )]
}

# Return the active disease name.
#
# Selection logic:
# - if `disease` is supplied:
#   - it must exist in spec$diseases
#   - and, by default, must be enabled
# - otherwise:
#   - use meta.tumour_type if it is an enabled disease
#   - otherwise use the first enabled disease
#
# Arguments:
# - spec:
#   simulation specification
# - disease:
#   optional disease name
# - must_be_enabled:
#   if TRUE, the returned disease must be enabled
get_disease_name <- function(spec,
                             disease = NULL,
                             must_be_enabled = TRUE) {
  diseases <- get_diseases_spec(spec)
  
  if (!is.null(disease)) {
    .config_accessors_validate_name(disease, arg_name = "disease")
    
    if (is.null(diseases[[disease]])) {
      stop("Unknown disease '", disease, "'.", call. = FALSE)
    }
    
    if (isTRUE(must_be_enabled) && !isTRUE(diseases[[disease]]$enabled)) {
      stop("Disease '", disease, "' is not enabled.", call. = FALSE)
    }
    
    return(disease)
  }
  
  meta <- get_meta_spec(spec)
  enabled <- get_enabled_disease_names(spec)
  meta_tumour <- meta$tumour_type
  
  if (isTRUE(must_be_enabled)) {
    if (is_scalar_string(meta_tumour) && meta_tumour %in% enabled) {
      return(meta_tumour)
    }
    
    if (length(enabled) == 0) {
      stop("No enabled disease found in `spec$diseases`.", call. = FALSE)
    }
    
    return(enabled[1])
  }
  
  all_names <- names(diseases)
  
  if (is_scalar_string(meta_tumour) && meta_tumour %in% all_names) {
    return(meta_tumour)
  }
  
  if (length(all_names) == 0) {
    stop("No diseases found in `spec$diseases`.", call. = FALSE)
  }
  
  all_names[1]
}

# Return one disease specification block.
#
# Arguments:
# - spec:
#   simulation specification
# - disease:
#   optional disease name; if NULL, get_disease_name() is used
# - must_be_enabled:
#   passed to get_disease_name()
get_disease_spec <- function(spec,
                             disease = NULL,
                             must_be_enabled = TRUE) {
  disease_name <- get_disease_name(
    spec = spec,
    disease = disease,
    must_be_enabled = must_be_enabled
  )
  
  diseases <- get_diseases_spec(spec)
  disease_spec <- diseases[[disease_name]]
  
  if (!is.list(disease_spec)) {
    stop(
      "Disease specification for '", disease_name, "' must be a list.",
      call. = FALSE
    )
  }
  
  disease_spec
}

# Return the mortality-model block from the simulation specification.
get_mortality_models_spec <- function(spec) {
  .config_accessors_get_top_level_list_block(spec, "mortality")
}

# Return all mortality-model names.
get_mortality_names <- function(spec) {
  mortality <- get_mortality_models_spec(spec)
  names(mortality)
}

# Return the active mortality-model name.
#
# Selection logic:
# - if `mortality_name` is supplied, it must exist
# - otherwise use the first mortality model in the spec
get_mortality_name <- function(spec,
                               mortality_name = NULL) {
  mortality <- get_mortality_models_spec(spec)
  
  if (!is.null(mortality_name)) {
    .config_accessors_validate_name(mortality_name, arg_name = "mortality_name")
    
    if (is.null(mortality[[mortality_name]])) {
      stop("Unknown mortality model '", mortality_name, "'.", call. = FALSE)
    }
    
    return(mortality_name)
  }
  
  names_mortality <- names(mortality)
  
  if (length(names_mortality) == 0) {
    stop("No mortality model found in `spec$mortality`.", call. = FALSE)
  }
  
  names_mortality[1]
}

# Return one mortality-model specification block.
#
# Arguments:
# - spec:
#   simulation specification
# - mortality_name:
#   optional mortality-model name; if NULL, get_mortality_name() is used
get_mortality_spec <- function(spec,
                               mortality_name = NULL) {
  selected_name <- get_mortality_name(
    spec = spec,
    mortality_name = mortality_name
  )
  
  mortality <- get_mortality_models_spec(spec)
  mortality_spec <- mortality[[selected_name]]
  
  if (!is.list(mortality_spec)) {
    stop(
      "Mortality specification for '", selected_name, "' must be a list.",
      call. = FALSE
    )
  }
  
  mortality_spec
}

# Return the rules array from the simulation specification.
get_rules_spec <- function(spec) {
  if (!is.list(spec) || is.null(spec$rules) || !is.list(spec$rules)) {
    stop("`spec$rules` must be a list.", call. = FALSE)
  }
  
  spec$rules
}
