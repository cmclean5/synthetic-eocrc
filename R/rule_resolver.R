# Resolve rule-based parameter values from a simulation specification.
#
# This script provides the core rule-resolution engine for the simulator.
# It is intended to turn fragmented external evidence into usable model inputs.
#
# Main user-facing functions:
# - resolve_rule()
# - resolve_rule_candidates()
#
# Current implementation supports:
# - matching rules by target and subgroup selectors
# - choosing the most specific matching selector set
# - evaluating rule values at a requested calendar year
# - applying piecewise annual percent change rules across multiple periods
# - optional debugging output showing which rules were considered
#
# Current design notes:
# - selector semantics are delegated to dimensions.R
# - period handling and interpolation are delegated to interpolation.R
# - "most_specific" matching is applied first
# - non-APC rule composition is then done within the selected specificity level
# - APC rules at the selected specificity level are composed across periods
#
# Expected shared package utilities:
# - from utils_math.R:
#   - `%||%`
#   - is_scalar_number()
#   - is_scalar_string()
#   - as_named_numeric()
# - from config accessor utilities:
#   - get_rule_resolution_spec()
#   - get_rules_spec()
# - from dimensions.R:
#   - .dimensions_build_context()
#   - .dimensions_rule_matches()
#   - .dimensions_rule_specificity()
#   - .dimensions_selector_key()
# - from interpolation.R:
#   - .interpolation_as_record_list()
#   - .interpolation_period_status()
#   - .interpolation_evaluate_rule()

# Add two resolved values together.
#
# Supported combinations are:
# - scalar + scalar
# - named numeric map + named numeric map with matching names
#
# If one input is NULL, the other is returned unchanged.
.rule_resolver_add_values <- function(x,
                                      y,
                                      context = "rule combination") {
  if (is.null(x)) {
    return(y)
  }
  
  if (is.null(y)) {
    return(x)
  }
  
  if (is_scalar_number(x) && is_scalar_number(y)) {
    return(as.numeric(x) + as.numeric(y))
  }
  
  nx <- as_named_numeric(x)
  ny <- as_named_numeric(y)
  
  if (!is.null(nx) && !is.null(ny)) {
    if (!setequal(names(nx), names(ny)) || length(nx) != length(ny)) {
      stop(
        context, ": named values must have the same names for addition.",
        call. = FALSE
      )
    }
    
    ny <- ny[names(nx)]
    return(nx + ny)
  }
  
  stop(
    context, ": incompatible value types for addition.",
    call. = FALSE
  )
}

# Multiply two resolved values together.
#
# Supported combinations are:
# - scalar * scalar
# - named numeric map * named numeric map with matching names
#
# If one input is NULL, the other is returned unchanged.
.rule_resolver_multiply_values <- function(x,
                                           y,
                                           context = "rule combination") {
  if (is.null(x)) {
    return(y)
  }
  
  if (is.null(y)) {
    return(x)
  }
  
  if (is_scalar_number(x) && is_scalar_number(y)) {
    return(as.numeric(x) * as.numeric(y))
  }
  
  nx <- as_named_numeric(x)
  ny <- as_named_numeric(y)
  
  if (!is.null(nx) && !is.null(ny)) {
    if (!setequal(names(nx), names(ny)) || length(nx) != length(ny)) {
      stop(
        context, ": named values must have the same names for multiplication.",
        call. = FALSE
      )
    }
    
    ny <- ny[names(nx)]
    return(nx * ny)
  }
  
  stop(
    context, ": incompatible value types for multiplication.",
    call. = FALSE
  )
}

# Reduce a list of values by repeated addition or multiplication.
.rule_resolver_reduce_values <- function(values,
                                         method = c("add", "multiply"),
                                         context = "rule combination") {
  method <- match.arg(method)
  
  if (length(values) == 0) {
    return(NULL)
  }
  
  out <- values[[1]]
  
  if (length(values) == 1) {
    return(out)
  }
  
  for (i in 2:length(values)) {
    if (identical(method, "add")) {
      out <- .rule_resolver_add_values(
        out,
        values[[i]],
        context = context
      )
    } else {
      out <- .rule_resolver_multiply_values(
        out,
        values[[i]],
        context = context
      )
    }
  }
  
  out
}

# Attach rule indices so matched rules can always be traced back to spec$rules.
.rule_resolver_get_rules_with_index <- function(spec) {
  rules <- .interpolation_as_record_list(get_rules_spec(spec))
  
  if (length(rules) == 0) {
    return(list())
  }
  
  for (i in seq_along(rules)) {
    rules[[i]][["..rule_index"]] <- i
  }
  
  rules
}

# Extract rule indices from a list of rule records.
#
# Missing indices are returned as NA_integer_ so callers can rely on a stable
# integer vector shape even if malformed rules are encountered.
.rule_resolver_rule_indices <- function(rules) {
  if (length(rules) == 0) {
    return(integer(0))
  }
  
  vapply(
    rules,
    function(rule) as.integer(rule[["..rule_index"]] %||% NA_integer_),
    integer(1)
  )
}

# Return an empty candidate summary table with the standard columns.
#
# This keeps resolve_rule_candidates() and return_details output aligned even
# when no rules match.
.rule_resolver_empty_candidate_table <- function() {
  data.frame(
    rule_index = integer(0),
    target = character(0),
    rule_type = character(0),
    operation = character(0),
    scale = character(0),
    specificity = integer(0),
    selector_key = character(0),
    priority = numeric(0),
    period_start = numeric(0),
    period_end = numeric(0),
    extrapolation = character(0),
    period_inside = logical(0),
    period_relation = character(0),
    period_distance = numeric(0),
    can_evaluate = logical(0),
    matched_selectors = logical(0),
    stringsAsFactors = FALSE
  )
}

# Return a numeric rank for operations that set a base value.
#
# Higher rank means higher precedence.
# Current precedence:
# - override = 2
# - set = 1
# - everything else = 0
.rule_resolver_operation_rank <- function(operation) {
  if (identical(operation, "override")) {
    return(2L)
  }
  
  if (identical(operation, "set")) {
    return(1L)
  }
  
  0L
}

# Build a summary row for a matched rule.
#
# This is used by resolve_rule_candidates() and also by resolve_rule() when
# return_details = TRUE.
.rule_resolver_candidate_summary_row <- function(rule, year, spec, context) {
  status <- .interpolation_period_status(rule, year, spec)
  
  data.frame(
    rule_index = rule[["..rule_index"]] %||% NA_integer_,
    target = as.character(rule$target %||% NA_character_),
    rule_type = as.character(rule$rule_type %||% NA_character_),
    operation = as.character(rule$operation %||% NA_character_),
    scale = as.character(rule$scale %||% NA_character_),
    specificity = .dimensions_rule_specificity(rule),
    selector_key = .dimensions_selector_key(rule$selectors %||% list()),
    priority = as.numeric(rule$priority %||% NA_real_),
    period_start = status$start,
    period_end = status$end,
    extrapolation = as.character(status$extrapolation %||% NA_character_),
    period_inside = isTRUE(status$inside),
    period_relation = as.character(status$relation %||% NA_character_),
    period_distance = as.numeric(status$distance %||% NA_real_),
    can_evaluate = isTRUE(status$can_evaluate),
    matched_selectors = isTRUE(.dimensions_rule_matches(rule, context)),
    stringsAsFactors = FALSE
  )
}

# Build a candidate summary table from a list of matched rules.
#
# If no candidates are supplied, return an empty table with standard columns.
.rule_resolver_candidate_table <- function(candidates, year, spec, context) {
  if (length(candidates) == 0) {
    return(.rule_resolver_empty_candidate_table())
  }
  
  rows <- lapply(
    candidates,
    function(rule) {
      .rule_resolver_candidate_summary_row(
        rule = rule,
        year = year,
        spec = spec,
        context = context
      )
    }
  )
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  
  out
}

# Find all rules that match a target, selectors, and calendar year.
#
# Matching steps are:
# 1. target equality
# 2. selector match
# 3. period evaluability under the rule's extrapolation policy
.rule_resolver_find_candidates <- function(spec,
                                           target,
                                           year,
                                           context) {
  rules <- .rule_resolver_get_rules_with_index(spec)
  
  if (length(rules) == 0) {
    return(list())
  }
  
  out <- list()
  k <- 0L
  
  for (rule in rules) {
    if (!identical(as.character(rule$target %||% ""), as.character(target))) {
      next
    }
    
    if (!isTRUE(.dimensions_rule_matches(rule, context))) {
      next
    }
    
    status <- .interpolation_period_status(rule, year, spec)
    
    if (!isTRUE(status$can_evaluate)) {
      next
    }
    
    k <- k + 1L
    out[[k]] <- rule
  }
  
  out
}

# Choose the best base-setting rule from a set of candidates.
#
# Ranking order:
# 1. operation precedence: override > set
# 2. inside current period beats outside current period
# 3. shorter distance to period is preferred
# 4. higher priority is preferred
# 5. if configured, later period start is preferred
#
# If the top rank is still tied after applying the configured tie-breaker,
# the function stops with an ambiguity error.
.rule_resolver_choose_base_rule <- function(rules, year, spec) {
  if (length(rules) == 0) {
    return(NULL)
  }
  
  rule_resolution <- get_rule_resolution_spec(spec)
  tie_breaker <- rule_resolution$tie_breaker %||% "priority"
  
  meta <- lapply(rules, function(rule) {
    status <- .interpolation_period_status(rule, year, spec)
    
    list(
      rule = rule,
      rule_index = rule[["..rule_index"]] %||% NA_integer_,
      operation_rank = .rule_resolver_operation_rank(rule$operation %||% ""),
      inside = as.integer(isTRUE(status$inside)),
      distance = as.numeric(status$distance %||% Inf),
      priority = if (is_scalar_number(rule$priority)) as.numeric(rule$priority) else -Inf,
      period_start = if (is_scalar_number(status$start)) as.numeric(status$start) else -Inf
    )
  })
  
  ord <- order(
    -vapply(meta, `[[`, numeric(1), "operation_rank"),
    -vapply(meta, `[[`, numeric(1), "inside"),
    vapply(meta, `[[`, numeric(1), "distance"),
    -vapply(meta, `[[`, numeric(1), "priority"),
    if (identical(tie_breaker, "priority_then_latest_period_start")) {
      -vapply(meta, `[[`, numeric(1), "period_start")
    } else {
      seq_along(meta)
    }
  )
  
  meta <- meta[ord]
  best <- meta[[1]]
  
  if (length(meta) > 1) {
    second <- meta[[2]]
    
    same_primary_rank <-
      identical(best$operation_rank, second$operation_rank) &&
      identical(best$inside, second$inside) &&
      isTRUE(all.equal(best$distance, second$distance)) &&
      isTRUE(all.equal(best$priority, second$priority))
    
    same_full_rank <- if (identical(tie_breaker, "priority_then_latest_period_start")) {
      same_primary_rank &&
        isTRUE(all.equal(best$period_start, second$period_start))
    } else {
      same_primary_rank
    }
    
    if (isTRUE(same_full_rank)) {
      stop(
        "Ambiguous base rule resolution for target '",
        best$rule$target %||% "", "'. ",
        "Top-ranked rules include indices ",
        best$rule_index, " and ", second$rule_index, ".",
        call. = FALSE
      )
    }
  }
  
  best$rule
}

# Compose multiple APC rules across periods.
#
# Rules are evaluated individually and then combined by scale:
# - identity scale APC rules are multiplied
# - log/logit scale APC rules are added
#
# This allows piecewise annual percent changes across adjacent periods.
.rule_resolver_compose_apc_rules <- function(rules, year, spec) {
  if (length(rules) == 0) {
    return(NULL)
  }
  
  scales <- unique(vapply(
    rules,
    function(rule) as.character(rule$scale %||% ""),
    character(1)
  ))
  
  if (length(scales) != 1) {
    stop(
      "APC rules for a single target must share the same scale.",
      call. = FALSE
    )
  }
  
  scale <- scales[1]
  
  values <- lapply(
    rules,
    function(rule) .interpolation_evaluate_rule(rule, year, spec)
  )
  
  if (identical(scale, "identity")) {
    return(
      .rule_resolver_reduce_values(
        values,
        method = "multiply",
        context = "APC rule composition"
      )
    )
  }
  
  if (scale %in% c("log", "logit")) {
    return(
      .rule_resolver_reduce_values(
        values,
        method = "add",
        context = "APC rule composition"
      )
    )
  }
  
  stop(
    "Unsupported APC scale: ", scale,
    call. = FALSE
  )
}

# Return a data-frame summary of all matched candidate rules for a target.
#
# This function uses the same core matching logic as resolve_rule():
# - target must match exactly
# - selectors must match the supplied context
# - the rule must be evaluable at the requested year under its extrapolation
#   rule
#
# Returned rows therefore correspond to true candidate rules, not just all
# rules with the same target.
resolve_rule_candidates <- function(spec,
                                    target,
                                    year,
                                    age = NULL,
                                    sex = NULL,
                                    ses = NULL,
                                    education = NULL,
                                    ethnicity = NULL,
                                    geography = NULL,
                                    disease = NULL) {
  if (!is.list(spec)) {
    stop("`spec` must be a nested list.", call. = FALSE)
  }
  
  if (!is_scalar_string(target)) {
    stop("`target` must be a single non-empty string.", call. = FALSE)
  }
  
  if (!is_scalar_number(year)) {
    stop("`year` must be a numeric scalar.", call. = FALSE)
  }
  
  context <- .dimensions_build_context(
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    disease = disease
  )
  
  candidates <- .rule_resolver_find_candidates(
    spec = spec,
    target = target,
    year = year,
    context = context
  )
  
  out <- .rule_resolver_candidate_table(
    candidates = candidates,
    year = year,
    spec = spec,
    context = context
  )
  
  if (nrow(out) == 0) {
    return(out)
  }
  
  out <- out[order(
    -out$specificity,
    -out$period_inside,
    out$period_distance,
    -out$priority,
    out$rule_index
  ), , drop = FALSE]
  
  rownames(out) <- NULL
  out
}

# Resolve a target value from the simulation specification.
#
# Arguments:
# - spec:
#   nested simulation spec list
# - target:
#   target string such as:
#   - "alcohol.state_probs"
#   - "adiposity.obesity_probability.base"
#   - "disease.crc.apc"
# - year:
#   calendar year at which the rule should be resolved
# - age, sex, ses, education, ethnicity, geography, disease:
#   subgroup selectors used when matching rules
# - return_details:
#   if TRUE, return a list containing:
#   - value
#   - details
# - error_if_unresolved:
#   if TRUE, stop when no rule can be resolved
#   if FALSE, return NULL or a details object with value = NULL
#
# Resolution strategy:
# 1. find all target-matching rules whose selectors match the context and whose
#    periods are evaluable at `year`
# 2. keep only rules at the highest selector specificity level
# 3. among non-APC rules at that specificity:
#    - choose one base rule from set/override rules
#    - combine add rules by addition
#    - combine multiply rules by multiplication
# 4. among APC rules at that specificity:
#    - compose them across periods
# 5. combine everything into one resolved value
resolve_rule <- function(spec,
                         target,
                         year,
                         age = NULL,
                         sex = NULL,
                         ses = NULL,
                         education = NULL,
                         ethnicity = NULL,
                         geography = NULL,
                         disease = NULL,
                         return_details = FALSE,
                         error_if_unresolved = TRUE) {
  if (!is.list(spec)) {
    stop("`spec` must be a nested list.", call. = FALSE)
  }
  
  if (!is_scalar_string(target)) {
    stop("`target` must be a single non-empty string.", call. = FALSE)
  }
  
  if (!is_scalar_number(year)) {
    stop("`year` must be a numeric scalar.", call. = FALSE)
  }
  
  rule_resolution <- get_rule_resolution_spec(spec)
  match_strategy <- rule_resolution$match_strategy %||% "most_specific"
  
  if (!identical(match_strategy, "most_specific")) {
    stop(
      "Unsupported match_strategy: ", match_strategy,
      call. = FALSE
    )
  }
  
  context <- .dimensions_build_context(
    age = age,
    sex = sex,
    ses = ses,
    education = education,
    ethnicity = ethnicity,
    geography = geography,
    disease = disease
  )
  
  candidates <- .rule_resolver_find_candidates(
    spec = spec,
    target = target,
    year = year,
    context = context
  )
  
  if (length(candidates) == 0) {
    if (isTRUE(error_if_unresolved)) {
      stop(
        "No evaluable matching rule found for target '", target,
        "' at year ", year, ".",
        call. = FALSE
      )
    }
    
    if (isTRUE(return_details)) {
      return(list(
        value = NULL,
        details = list(
          target = target,
          year = year,
          context = context,
          matched_rule_indices = integer(0),
          selected_specificity = NA_integer_,
          selected_rule_indices = integer(0),
          base_rule_index = NA_integer_,
          add_rule_indices = integer(0),
          multiply_rule_indices = integer(0),
          apc_rule_indices = integer(0)
        )
      ))
    }
    
    return(NULL)
  }
  
  specificity_scores <- vapply(candidates, .dimensions_rule_specificity, integer(1))
  selected_specificity <- max(specificity_scores)
  
  selected <- candidates[specificity_scores == selected_specificity]
  
  selected_non_apc <- Filter(
    function(rule) !identical(rule$rule_type %||% "", "annual_percent_change"),
    selected
  )
  
  selected_apc <- Filter(
    function(rule) identical(rule$rule_type %||% "", "annual_percent_change"),
    selected
  )
  
  # Resolve the base-setting rule, if any.
  base_candidates <- Filter(
    function(rule) (rule$operation %||% "") %in% c("set", "override"),
    selected_non_apc
  )
  
  base_rule <- .rule_resolver_choose_base_rule(
    rules = base_candidates,
    year = year,
    spec = spec
  )
  
  base_value <- if (!is.null(base_rule)) {
    .interpolation_evaluate_rule(base_rule, year, spec)
  } else {
    NULL
  }
  
  # Resolve additive rules at the selected specificity.
  add_rules <- Filter(
    function(rule) identical(rule$operation %||% "", "add"),
    selected_non_apc
  )
  
  add_value <- .rule_resolver_reduce_values(
    values = lapply(add_rules, .interpolation_evaluate_rule, year = year, spec = spec),
    method = "add",
    context = paste0("Resolution of target '", target, "'")
  )
  
  # Resolve multiplicative rules at the selected specificity.
  multiply_rules <- Filter(
    function(rule) identical(rule$operation %||% "", "multiply"),
    selected_non_apc
  )
  
  multiply_value <- .rule_resolver_reduce_values(
    values = lapply(multiply_rules, .interpolation_evaluate_rule, year = year, spec = spec),
    method = "multiply",
    context = paste0("Resolution of target '", target, "'")
  )
  
  # Resolve APC rules across periods at the selected specificity.
  apc_value <- .rule_resolver_compose_apc_rules(
    rules = selected_apc,
    year = year,
    spec = spec
  )
  
  apc_scale <- if (length(selected_apc) > 0) {
    unique(vapply(
      selected_apc,
      function(rule) as.character(rule$scale %||% ""),
      character(1)
    ))
  } else {
    character(0)
  }
  
  # Combine components into a final resolved value.
  #
  # Combination logic:
  # - start from the base rule if present
  # - apply additive modifiers
  # - apply APC if it is on a log/logit scale
  # - apply multiplicative modifiers
  # - apply APC if it is on an identity scale
  #
  # This makes APC log offsets behave naturally for hazards and APC multipliers
  # behave naturally for identity-scale quantities.
  result <- base_value
  
  if (!is.null(add_value)) {
    result <- .rule_resolver_add_values(
      result,
      add_value,
      context = paste0("Resolution of target '", target, "'")
    )
  }
  
  if (length(apc_scale) == 1 && apc_scale %in% c("log", "logit")) {
    result <- .rule_resolver_add_values(
      result,
      apc_value,
      context = paste0("Resolution of target '", target, "'")
    )
  }
  
  if (!is.null(multiply_value)) {
    result <- .rule_resolver_multiply_values(
      result,
      multiply_value,
      context = paste0("Resolution of target '", target, "'")
    )
  }
  
  if (length(apc_scale) == 1 && identical(apc_scale, "identity")) {
    result <- .rule_resolver_multiply_values(
      result,
      apc_value,
      context = paste0("Resolution of target '", target, "'")
    )
  }
  
  # If there was no base rule and no modifiers but APC exists, the result may
  # still be defined correctly as a pure APC contribution.
  if (is.null(result) && !is.null(apc_value)) {
    result <- apc_value
  }
  
  selected_indices <- .rule_resolver_rule_indices(selected)
  matched_indices <- .rule_resolver_rule_indices(candidates)
  add_rule_indices <- .rule_resolver_rule_indices(add_rules)
  multiply_rule_indices <- .rule_resolver_rule_indices(multiply_rules)
  apc_rule_indices <- .rule_resolver_rule_indices(selected_apc)
  base_rule_index <- if (!is.null(base_rule)) {
    as.integer(base_rule[["..rule_index"]] %||% NA_integer_)
  } else {
    NA_integer_
  }
  
  if (is.null(result)) {
    if (isTRUE(error_if_unresolved)) {
      stop(
        "Target '", target,
        "' matched rules, but no final value could be resolved.",
        call. = FALSE
      )
    }
    
    if (isTRUE(return_details)) {
      return(list(
        value = NULL,
        details = list(
          target = target,
          year = year,
          context = context,
          matched_rule_indices = matched_indices,
          selected_specificity = selected_specificity,
          selected_rule_indices = selected_indices,
          base_rule_index = NA_integer_,
          add_rule_indices = add_rule_indices,
          multiply_rule_indices = multiply_rule_indices,
          apc_rule_indices = apc_rule_indices
        )
      ))
    }
    
    return(NULL)
  }
  
  if (isTRUE(return_details)) {
    candidate_table <- .rule_resolver_candidate_table(
      candidates = candidates,
      year = year,
      spec = spec,
      context = context
    )
    
    return(list(
      value = result,
      details = list(
        target = target,
        year = year,
        context = context,
        matched_rule_indices = matched_indices,
        selected_specificity = selected_specificity,
        selected_rule_indices = selected_indices,
        base_rule_index = base_rule_index,
        add_rule_indices = add_rule_indices,
        multiply_rule_indices = multiply_rule_indices,
        apc_rule_indices = apc_rule_indices,
        candidate_table = candidate_table
      )
    ))
  }
  
  result
}
