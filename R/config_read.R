# Read and validate a simulation specification JSON file.
#
# This script provides the main entry point for loading simulation
# configuration files such as:
# - inst/configs/colorectal_eo.json
#
# Validation is performed in two layers:
# 1. JSON Schema validation through jsonvalidate
# 2. semantic validation through validate_sim_spec()
#
# Main user-facing function:
# - read_sim_spec()
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_string()
#
# Expected shared package functions:
# - from config_validate.R:
#   - validate_sim_spec()
#
# Package design note:
# - this file assumes package-style namespace availability
# - it does not use per-file require guards
# - package dependencies such as jsonlite and jsonvalidate are still checked
#   via requireNamespace() because they are external packages

# Check whether a string looks like a URL or URI.
#
# This is used so schema references such as:
# - https://...
# - file://...
# are not incorrectly treated as local relative file paths.
.config_read_is_url_or_uri <- function(x) {
  is_scalar_string(x) &&
    grepl("^[A-Za-z][A-Za-z0-9+.-]*://", x)
}

# Check whether a path string is absolute.
#
# This handles:
# - Unix-like absolute paths such as /home/user/file.json
# - Windows absolute paths such as C:/path/file.json
# - Windows UNC paths such as \\server\share\file.json
.config_read_is_absolute_path <- function(x) {
  if (!is_scalar_string(x)) {
    return(FALSE)
  }
  
  if (.Platform$OS.type == "windows") {
    return(grepl("^(?:[A-Za-z]:[\\\\/]|\\\\\\\\)", x))
  }
  
  startsWith(x, "/")
}

# Resolve a schema reference into a usable schema path.
#
# Resolution strategy:
# - if the schema reference is a URL or URI, return it unchanged
# - if it is an absolute local path, return it unchanged
# - if it exists relative to the current working directory, use that
# - otherwise resolve it relative to the config file directory
#
# This is useful because JSON configs often use a relative schema reference such
# as:
#   "../schemas/sim_spec.schema.json"
#
# and that path should be interpreted relative to the config file itself.
.config_read_resolve_schema_path <- function(schema_ref,
                                             config_path) {
  if (!is_scalar_string(schema_ref)) {
    return(NULL)
  }
  
  if (.config_read_is_url_or_uri(schema_ref)) {
    return(schema_ref)
  }
  
  if (.config_read_is_absolute_path(schema_ref)) {
    return(schema_ref)
  }
  
  if (file.exists(schema_ref)) {
    return(normalizePath(schema_ref, winslash = "/", mustWork = FALSE))
  }
  
  normalizePath(
    file.path(dirname(config_path), schema_ref),
    winslash = "/",
    mustWork = FALSE
  )
}

# Read a text file into a single character string.
#
# This is used for JSON Schema validation because jsonvalidate works naturally
# with raw JSON text input.
.config_read_text_file <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# Attach useful metadata attributes to a loaded spec object.
#
# These attributes help with debugging and reproducibility by recording:
# - where the config came from
# - whether schema validation was run
# - whether semantic validation was run
# - which schema path was used
.config_read_attach_metadata <- function(spec,
                                         source_path,
                                         schema_path = NULL,
                                         raw_schema_ref = NULL,
                                         schema_validated = FALSE,
                                         semantic_validated = FALSE) {
  attr(spec, "source_path") <- source_path
  attr(spec, "schema_path") <- schema_path
  attr(spec, "raw_schema_ref") <- raw_schema_ref
  attr(spec, "schema_validated") <- schema_validated
  attr(spec, "semantic_validated") <- semantic_validated
  
  class(spec) <- unique(c("sim_spec", class(spec)))
  spec
}

# Read a simulation specification from JSON and optionally validate it.
#
# Arguments:
# - path:
#   path to the config JSON file
# - schema_path:
#   optional explicit path to the JSON Schema file
#   if NULL, the function will try to read the "$schema" field from the config
# - validate_schema:
#   if TRUE, validate the JSON file against the schema using jsonvalidate
# - validate_semantics:
#   if TRUE, run validate_sim_spec() after reading the JSON
# - simplifyVector:
#   passed to jsonlite::read_json()
#   this should normally be FALSE because semantic validation expects a nested
#   list structure
# - error_on_semantic_fail:
#   passed through to validate_sim_spec()
# - warn_on_nonfatal:
#   passed through to validate_sim_spec()
#
# Returns:
# - the spec as a nested list
# - with validation and provenance metadata attached as attributes
read_sim_spec <- function(path,
                          schema_path = NULL,
                          validate_schema = TRUE,
                          validate_semantics = TRUE,
                          simplifyVector = FALSE,
                          error_on_semantic_fail = TRUE,
                          warn_on_nonfatal = TRUE) {
  if (!is_scalar_string(path)) {
    stop("`path` must be a single non-empty string.", call. = FALSE)
  }
  
  if (!file.exists(path)) {
    stop("Config file does not exist: ", path, call. = FALSE)
  }
  
  if (isTRUE(validate_semantics) && !identical(simplifyVector, FALSE)) {
    stop(
      "`validate_semantics = TRUE` requires `simplifyVector = FALSE` so that ",
      "the spec is read as a nested list.",
      call. = FALSE
    )
  }
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Package 'jsonlite' is required to read simulation spec files.",
      call. = FALSE
    )
  }
  
  if (isTRUE(validate_schema) &&
      !requireNamespace("jsonvalidate", quietly = TRUE)) {
    stop(
      "Package 'jsonvalidate' is required when `validate_schema = TRUE`.",
      call. = FALSE
    )
  }
  
  # Read the JSON file as raw text for schema validation.
  json_txt <- tryCatch(
    .config_read_text_file(path),
    error = function(e) {
      stop(
        "Failed to read config file as text: ", path, "\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  
  # Parse the JSON file into an R object.
  spec <- tryCatch(
    jsonlite::read_json(path, simplifyVector = simplifyVector),
    error = function(e) {
      stop(
        "Failed to parse config JSON: ", path, "\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  
  raw_schema_ref <- NULL
  resolved_schema_path <- NULL
  
  # Resolve and apply JSON Schema validation if requested.
  if (isTRUE(validate_schema)) {
    raw_schema_ref <- if (!is.null(schema_path)) {
      schema_path
    } else {
      spec[["$schema"]]
    }
    
    resolved_schema_path <- .config_read_resolve_schema_path(
      schema_ref = raw_schema_ref,
      config_path = path
    )
    
    if (is.null(resolved_schema_path)) {
      stop(
        "Schema validation was requested, but no schema reference was available.\n",
        "Provide `schema_path = ...` explicitly or include a `$schema` field in the config.",
        call. = FALSE
      )
    }
    
    if (!.config_read_is_url_or_uri(resolved_schema_path) &&
        !file.exists(resolved_schema_path)) {
      stop(
        "Schema file does not exist: ", resolved_schema_path,
        call. = FALSE
      )
    }
    
    tryCatch(
      {
        jsonvalidate::json_validate(
          json = json_txt,
          schema = resolved_schema_path,
          engine = "ajv",
          error = TRUE
        )
      },
      error = function(e) {
        stop(
          "JSON Schema validation failed for config file: ", path, "\n",
          "Schema used: ", resolved_schema_path, "\n",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  
  # Apply semantic validation if requested.
  if (isTRUE(validate_semantics)) {
    spec <- validate_sim_spec(
      spec = spec,
      error_on_fail = error_on_semantic_fail,
      warn_on_nonfatal = warn_on_nonfatal,
      return_result = FALSE
    )
  }
  
  # Attach provenance and validation metadata.
  spec <- .config_read_attach_metadata(
    spec = spec,
    source_path = normalizePath(path, winslash = "/", mustWork = FALSE),
    schema_path = if (is.null(resolved_schema_path)) {
      NULL
    } else if (.config_read_is_url_or_uri(resolved_schema_path)) {
      resolved_schema_path
    } else {
      normalizePath(resolved_schema_path, winslash = "/", mustWork = FALSE)
    },
    raw_schema_ref = raw_schema_ref,
    schema_validated = isTRUE(validate_schema),
    semantic_validated = isTRUE(validate_semantics)
  )
  
  spec
}
