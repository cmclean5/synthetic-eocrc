# Temporal evaluation and interpolation helpers for rule resolution.
#
# This script centralises the time-based logic used by the simulator's rule
# system. It handles:
# - period evaluability and extrapolation
# - anchor-point interpolation
# - annual percent change evaluation
# - conversion of raw rule values into usable numeric forms
#
# Main internal helpers:
# - .interpolation_period_status()
# - .interpolation_evaluate_rule()
#
# Current design notes:
# - temporal evaluation is always done at a requested calendar year
# - rule periods may clamp or reject evaluation outside their bounds
# - anchor-point interpolation is linear within the anchor range
# - annual percent change rules return either multiplicative or additive
#   contributions depending on scale
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()
#   - is_scalar_string()
#   - as_named_numeric()
# - from config accessor utilities:
#   - get_rule_resolution_spec()

# Convert an object into a list of record-like lists.
#
# This is useful because JSON arrays may appear in R as:
# - a list of lists
# - a data frame
# - an empty list
#
# Internally the interpolation helpers work with a plain list of record-like
# lists.
.interpolation_as_record_list <- function(x) {
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

# Convert a raw parameter value into a usable R value.
#
# Supported forms are:
# - numeric scalar
# - named numeric vector
# - named list of scalar numerics
#
# If conversion is not possible, stop with an informative error.
.interpolation_parameter_value <- function(x,
                                           context = "rule value") {
  if (is_scalar_number(x)) {
    return(as.numeric(x))
  }
  
  named_vals <- as_named_numeric(x)
  
  if (!is.null(named_vals)) {
    return(named_vals)
  }
  
  stop(
    context, " must be either a numeric scalar or a named numeric map.",
    call. = FALSE
  )
}

# Return the extrapolation behaviour for a rule.
#
# Resolution order:
# 1. rule-specific period extrapolation
# 2. spec-level default extrapolation
# 3. fallback to "clamp"
.interpolation_get_extrapolation <- function(rule, spec) {
  if (is.list(rule$period) &&
      is_scalar_string(rule$period$extrapolation)) {
    return(rule$period$extrapolation)
  }
  
  rule_resolution <- get_rule_resolution_spec(spec)
  
  if (is_scalar_string(rule_resolution$default_extrapolation)) {
    return(rule_resolution$default_extrapolation)
  }
  
  "clamp"
}

# Evaluate the relationship between a requested year and a rule period.
#
# Returned fields include:
# - can_evaluate: whether the rule can be used at the requested year
# - inside: whether year lies inside the rule period
# - relation: one of "inside", "before", "after", "invalid"
# - distance: distance from the period if outside, otherwise 0
# - effective_year: year after applying extrapolation
# - start, end, extrapolation: convenience metadata
#
# If extrapolation is "error" and the requested year lies outside the period,
# the rule is treated as not evaluable.
.interpolation_period_status <- function(rule, year, spec) {
  start <- if (is.list(rule$period)) rule$period$start %||% NA_real_ else NA_real_
  end <- if (is.list(rule$period)) rule$period$end %||% NA_real_ else NA_real_
  extrapolation <- .interpolation_get_extrapolation(rule, spec)
  
  if (!is_scalar_number(year) ||
      !is_scalar_number(start) ||
      !is_scalar_number(end) ||
      end <= start) {
    return(list(
      can_evaluate = FALSE,
      inside = FALSE,
      relation = "invalid",
      distance = Inf,
      effective_year = NA_real_,
      start = start,
      end = end,
      extrapolation = extrapolation
    ))
  }
  
  if (year >= start && year <= end) {
    return(list(
      can_evaluate = TRUE,
      inside = TRUE,
      relation = "inside",
      distance = 0,
      effective_year = year,
      start = start,
      end = end,
      extrapolation = extrapolation
    ))
  }
  
  if (year < start) {
    if (extrapolation %in% c("clamp", "nearest")) {
      return(list(
        can_evaluate = TRUE,
        inside = FALSE,
        relation = "before",
        distance = start - year,
        effective_year = start,
        start = start,
        end = end,
        extrapolation = extrapolation
      ))
    }
    
    return(list(
      can_evaluate = FALSE,
      inside = FALSE,
      relation = "before",
      distance = start - year,
      effective_year = NA_real_,
      start = start,
      end = end,
      extrapolation = extrapolation
    ))
  }
  
  if (year > end) {
    if (extrapolation %in% c("clamp", "nearest")) {
      return(list(
        can_evaluate = TRUE,
        inside = FALSE,
        relation = "after",
        distance = year - end,
        effective_year = end,
        start = start,
        end = end,
        extrapolation = extrapolation
      ))
    }
    
    return(list(
      can_evaluate = FALSE,
      inside = FALSE,
      relation = "after",
      distance = year - end,
      effective_year = NA_real_,
      start = start,
      end = end,
      extrapolation = extrapolation
    ))
  }
  
  list(
    can_evaluate = FALSE,
    inside = FALSE,
    relation = "invalid",
    distance = Inf,
    effective_year = NA_real_,
    start = start,
    end = end,
    extrapolation = extrapolation
  )
}

# Interpolate between two scalar anchor points.
.interpolation_interp_scalar <- function(x0, y0, x1, y1, x) {
  if (!all(vapply(c(x0, y0, x1, y1, x), is_scalar_number, logical(1)))) {
    stop("Scalar interpolation requires numeric scalar inputs.", call. = FALSE)
  }
  
  if (abs(x1 - x0) < .Machine$double.eps^0.5) {
    return(y0)
  }
  
  y0 + (y1 - y0) * (x - x0) / (x1 - x0)
}

# Interpolate between two named numeric maps.
#
# Names must match exactly. Interpolation is done element-wise.
.interpolation_interp_named_map <- function(x0, y0, x1, y1, x) {
  ny0 <- as_named_numeric(y0)
  ny1 <- as_named_numeric(y1)
  
  if (is.null(ny0) || is.null(ny1)) {
    stop("Named-map interpolation requires named numeric values.", call. = FALSE)
  }
  
  if (!setequal(names(ny0), names(ny1)) || length(ny0) != length(ny1)) {
    stop("Named-map interpolation requires matching names.", call. = FALSE)
  }
  
  ny1 <- ny1[names(ny0)]
  
  out <- vapply(
    seq_along(ny0),
    function(i) {
      .interpolation_interp_scalar(x0, ny0[i], x1, ny1[i], x)
    },
    numeric(1)
  )
  
  names(out) <- names(ny0)
  out
}

# Evaluate a constant rule.
.interpolation_eval_constant <- function(rule, year, spec) {
  status <- .interpolation_period_status(rule, year, spec)
  
  if (!isTRUE(status$can_evaluate)) {
    stop(
      "Constant rule cannot be evaluated at year ", year, ".",
      call. = FALSE
    )
  }
  
  .interpolation_parameter_value(
    rule$value,
    context = "Constant rule value"
  )
}

# Evaluate an anchor-point rule at a requested year.
#
# Within the anchor range, interpolation is linear.
# Outside the anchor range, values are clamped to the nearest anchor after
# period extrapolation has already been applied.
.interpolation_eval_anchor_points <- function(rule, year, spec) {
  status <- .interpolation_period_status(rule, year, spec)
  
  if (!isTRUE(status$can_evaluate)) {
    stop(
      "Anchor-point rule cannot be evaluated at year ", year, ".",
      call. = FALSE
    )
  }
  
  anchors <- .interpolation_as_record_list(rule$anchors)
  
  if (length(anchors) < 2) {
    stop("Anchor-point rule must contain at least two anchors.", call. = FALSE)
  }
  
  anchor_years <- vapply(
    anchors,
    function(a) {
      if (is_scalar_number(a$year)) {
        as.numeric(a$year)
      } else {
        NA_real_
      }
    },
    numeric(1)
  )
  
  if (anyNA(anchor_years)) {
    stop("All anchor years must be numeric.", call. = FALSE)
  }
  
  anchor_values <- lapply(
    anchors,
    function(a) {
      .interpolation_parameter_value(
        a$value,
        context = "Anchor-point value"
      )
    }
  )
  
  target_year <- status$effective_year
  
  if (target_year <= anchor_years[1]) {
    return(anchor_values[[1]])
  }
  
  if (target_year >= anchor_years[length(anchor_years)]) {
    return(anchor_values[[length(anchor_values)]])
  }
  
  left_idx <- max(which(anchor_years <= target_year))
  right_idx <- left_idx + 1L
  
  if (anchor_years[left_idx] == target_year) {
    return(anchor_values[[left_idx]])
  }
  
  left_value <- anchor_values[[left_idx]]
  right_value <- anchor_values[[right_idx]]
  
  if (is_scalar_number(left_value) && is_scalar_number(right_value)) {
    return(
      .interpolation_interp_scalar(
        x0 = anchor_years[left_idx],
        y0 = left_value,
        x1 = anchor_years[right_idx],
        y1 = right_value,
        x = target_year
      )
    )
  }
  
  .interpolation_interp_named_map(
    x0 = anchor_years[left_idx],
    y0 = left_value,
    x1 = anchor_years[right_idx],
    y1 = right_value,
    x = target_year
  )
}

# Evaluate an annual percent change rule.
#
# Interpretation by scale:
# - identity: returns the cumulative multiplicative factor
# - log:      returns the cumulative additive offset on the log scale
# - logit:    returns the cumulative additive offset on the logit scale
#
# For years before the period start, clamped evaluation returns a zero-length
# effect:
# - identity -> 1
# - log/logit -> 0
.interpolation_eval_annual_percent_change <- function(rule, year, spec) {
  status <- .interpolation_period_status(rule, year, spec)
  
  if (!isTRUE(status$can_evaluate)) {
    stop(
      "Annual percent change rule cannot be evaluated at year ", year, ".",
      call. = FALSE
    )
  }
  
  annual_rate <- rule$annual_rate
  scale <- rule$scale %||% "log"
  
  if (!is_scalar_number(annual_rate)) {
    stop("Annual percent change rule requires numeric annual_rate.", call. = FALSE)
  }
  
  elapsed_years <- status$effective_year - status$start
  
  if (!is_scalar_number(elapsed_years) || elapsed_years < 0) {
    stop("Invalid elapsed time for annual percent change rule.", call. = FALSE)
  }
  
  if (identical(scale, "identity")) {
    return((1 + annual_rate)^elapsed_years)
  }
  
  if (scale %in% c("log", "logit")) {
    return(log1p(annual_rate) * elapsed_years)
  }
  
  stop(
    "Unsupported scale for annual percent change rule: ", scale,
    call. = FALSE
  )
}

# Evaluate a single rule at a requested year.
#
# Supported rule types are:
# - constant
# - anchor_points
# - annual_percent_change
.interpolation_evaluate_rule <- function(rule, year, spec) {
  rule_type <- rule$rule_type %||% ""
  
  if (identical(rule_type, "constant")) {
    return(.interpolation_eval_constant(rule, year, spec))
  }
  
  if (identical(rule_type, "anchor_points")) {
    return(.interpolation_eval_anchor_points(rule, year, spec))
  }
  
  if (identical(rule_type, "annual_percent_change")) {
    return(.interpolation_eval_annual_percent_change(rule, year, spec))
  }
  
  stop(
    "Unsupported rule_type: ", rule_type,
    call. = FALSE
  )
}
