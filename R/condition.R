#' Split a highlighted vector by condition(s)
#'
#' @description
#'
#' [condition_split()] divides a highlighted vector `x` into groups of elements
#' which receive the same conditional formatting. It is a wrapper around
#' [base::split()].
#'
#' @param x `[vlightr_highlight]`
#'
#' A highlighted vector to split.
#'
#' @param unlit `[logical(1)]`
#'
#' Whether elements of the output list are un-highlighted. Defaults to `TRUE`.
#'
#' @param no_cond `[character(1)]`
#'
#' The name given to the group of elements which receive no conditional
#' formatting. Defaults to `""` (i.e. unnamed).
#'
#' @returns
#'
#' A list of vectors containing the values for each group of elements in `x`. The
#' elements of the list are named using the description (`attr(x, "description"`)
#' corresponding to the conditions (`attr(x, "conditions")`) met by the group.
#'
#' If `unlit = FALSE`, the vectors in the list are highlighted (with the same
#' conditional formatting as `x`). Otherwise, they are un-highlighted.
#'
#' @examples
#' # Define a highlight
#' dummies <- highlight_case(
#'   x = c(1L, 0L, NA, 1L),
#'   .x == 1 ~ paste(.x, "[Yes]"),
#'   .x == 0 ~ paste(.x, "[No]"),
#'   is.na(.x) ~ "Unknown",
#'   .description = c("Yes", "No", "Unknown")
#' )
#' print(dummies)
#'
#' # Group elements by conditional format
#' condition_split(dummies)
#' condition_split(dummies, unlit = FALSE)
#'
#' # The group of unformatted elements is unnamed by default
#' dummies <- c(dummies, hl(2:4))
#' condition_split(dummies)
#' condition_split(dummies, no_cond = "Invalid Response")
#' @family condition utilities
#' @export
condition_split <- function(x, unlit = TRUE, no_cond = "") {
  x <- check_is_highlight(x)
  unlit <- check_is_bool(unlit)
  split(if (unlit) get_data(x) else x, condition_id(x, no_cond = no_cond))
}

#' Identify elements of a highlighted vector which meet a condition(s)
#'
#' @description
#'
#' [condition_id()] identifies elements of a highlighted vector `x` which receive
#' the same conditional formatting. Each element's identifier is a comma separated
#' string of the condition descriptions (`attr(x, "description")`) corresponding
#' to the conditions (`attr(x, "conditions")`) met by the element.
#'
#' @param x `[vlightr_highlight]`
#'
#' A highlighted vector to identify.
#'
#' @param no_cond `[logical(1)]`
#'
#' The identifier assigned to elements which recieve no conditional formatting.
#' Defaults to `""`.
#'
#' @examples
#' # Define a highlight
#' summary_hl <- highlight_case(
#'   x = 1:5,
#'   .x == min(.x, na.rm = TRUE) ~ paste0("(Min = ", .x, ")"),
#'   .x == quantile(.x, 0.5, na.rm = TRUE) ~ paste0("(Mid = ", .x, ")"),
#'   .x == max(.x, na.rm = TRUE) ~ paste0("(Max = ", .x, ")"),
#'   .x > quantile(.x, 0.5, na.rm = TRUE) ~ paste("[ High", .x, "]"),
#'   .x < quantile(.x, 0.5, na.rm = TRUE) ~ paste("[ Low", .x, "]"),
#'   .description = c(
#'     "Min",
#'     "Middle",
#'     "Max",
#'     ">P50",
#'     "<P50"
#'   )
#' )
#' print(summary_hl)
#'
#' # Identify the formatting received by each element
#' condition_id(summary_hl)
#'
#' # Identifies element's which meet multiple conditions
#' summary_hl_multi <- update_highlight(summary_hl, format_once = FALSE)
#' print(summary_hl_multi)
#' condition_id(summary_hl_multi)
#'
#' # Identify elements which meet no condition
#' summary_hl[[1]] <- hl(NA)
#' condition_id(summary_hl)
#' condition_id(summary_hl, no_cond = "No Condition")
#' @family condition utilities
#' @export
condition_id <- function(x, no_cond = "") {

  no_cond <- check_is_vector(no_cond, cls = "character", len = 1L)
  x_data <- get_data(check_is_highlightable(x))
  x_name <- rlang::caller_arg(x)
  precedence <- order(get_precedence(x))
  description <- get_description(x)[precedence]
  conditions <- get_conditions(x)[precedence]

  format_once <- get_format_once(x)
  is_unformatted <- rep(TRUE, length(x))
  id <- vector("character", length(x))

  for (i in seq_along(conditions)) {
    format_at <- evalidate_highlight_fun(
      input = x_data,
      fun = conditions[[i]],
      fun_name = glue::glue('attr({x_name},"conditions")[[{i}]]'),
      fun_type = "condition"
    )
    if (format_once) {
      format_at <- is_unformatted & format_at
      is_unformatted <- is_unformatted & !format_at
    }
    id[format_at] <- paste(id[format_at], description[[i]], sep = ", ")
  }

  unformatted <- id == ""
  id[unformatted] <- no_cond
  # There's a vestigial " , " at the start of every formatted `id`
  id[!unformatted] <- substr(id[!unformatted], 3, nchar(id[!unformatted]))
  id
}

#' Subset elements of a highlighted vector which meet a condition(s)
#'
#' @description
#'
#' [condition_keep()] keeps elements of a highlighted vector `x` which meet the
#' specified conditions (i.e. receive the same conditional formatting).
#'
#' `condition_keep(x, 1)` is equivalent to `x[attr(x, 'conditions')[[1]](x)]`.
#'
#' @param x `[vlightr_highlight]`
#'
#' A highlighted vector to subset.
#'
#' @param which `[character / logical / numeric]`
#'
#' Which conditions are used to subset the vector. Conditions can be selected by
#' index using a numeric or logical vector, or selected using the name of a
#' corresponding description(s) (`attr(x, 'description)`).
#'
#' @param meets `[character(1)]`
#'
#' One of `"all"`, `"any"`, or `"none"`. If `"all"`, then elements of `x` which
#' meet all of the specified conditions are subset, if `"any"`, then elements
#' which meet `"any"` of the condition are subset, and if `"none"`, then elements
#' which meet `"none"` of the conditions are subset
#'
#' @examples
#' # Define a highlight
#' summary_hl <- highlight_case(
#'   x = 1:5,
#'   .x == min(.x) ~ paste0("(Min = ", .x, ")"),
#'   .x == quantile(.x, 0.5) ~ paste0("(Mid = ", .x, ")"),
#'   .x == max(.x) ~ paste0("(Max = ", .x, ")"),
#'   .x > quantile(.x, 0.5) ~ paste("[ High", .x, "]"),
#'   .x < quantile(.x, 0.5) ~ paste("[ Low", .x, "]"),
#'   .description = c(
#'     "Min",
#'     "Middle",
#'     "Max",
#'     ">P50",
#'     "<P50"
#'   )
#' )
#'
#' # Keep the middle element
#' condition_keep(x, which = 3)
#'
#' # Specify a condition by it's description
#' condition_keep(x, which = "Middle")
#'
#' # Keep the minimum and maximum elements
#' condition_keep(x, which = c("Min", "Max"), meets = "any")
#' condition_keep(x, which = c("Min", "Max"), meets = "none")
#' condition_keep(x, which = c("Min", "Max"), meets = "all")
#' @family condition utilities
#' @export
condition_keep <- function(x, which = TRUE, meets = c("all", "any", "none")) {
  x[condition_met(x, which = which, meets = meets)]
}

#' Indicate the elements of a highlighted vector which meet a condition(s)
#'
#' @description
#'
#' [condition_met()] returns a logical vector indicating which elements of `x`
#' meet the specified conditions (i.e. receive the same conditional formatting).
#'
#' @param x `[vlightr_highlight]`
#'
#' A highlighted vector.
#'
#' @param which `[character / logical / numeric]`
#'
#' Which conditions are used to subset the vector. Conditions can be selected by
#' index using a numeric or logical vector, or selected using the name of a
#' corresponding description(s) (`attr(x, 'description)`).
#'
#' @param meets `[character(1)]`
#'
#' One of `"all"`, `"any"`, or `"none"`. The output vector is `TRUE` for
#' elements of `x` which:
#'
#' - meet all of the specified conditions, if `"all"`
#' - meet any of the specified conditions, if `"any"`
#' - meet none of the specified conditions, if `"none"`
#'
#' @examples
#' # Define a highlight
#' summary_hl <- highlight_case(
#'   x = 1:5,
#'   .x == min(.x) ~ paste0("(Min = ", .x, ")"),
#'   .x == quantile(.x, 0.5) ~ paste0("(Mid = ", .x, ")"),
#'   .x == max(.x) ~ paste0("(Max = ", .x, ")"),
#'   .x > quantile(.x, 0.5) ~ paste("[ High", .x, "]"),
#'   .x < quantile(.x, 0.5) ~ paste("[ Low", .x, "]"),
#'   .description = c(
#'     "Min",
#'     "Middle",
#'     "Max",
#'     ">P50",
#'     "<P50"
#'   )
#' )
#'
#' # Which element is the minimum?
#' condition_met(summary_hl, which = 1)
#'
#' # Specify a condition by it's description
#' condition_met(summary_hl, which = "Min")
#'
#' # Which elements are the minimum or maximum?
#' condition_met(summary_hl, which = c("Min", "Max"), meets = "any")
#' condition_met(summary_hl, which = c("Min", "Max"), meets = "none")
#' condition_met(summary_hl, which = c("Min", "Max"), meets = "all")
#' @family condition utilities
#' @export
condition_met <- function(
    x,
    which = TRUE,
    meets = c("all", "any", "none")
  ) {

  meets <- rlang::arg_match(meets)
  x <- check_is_highlight(x)
  x_name <- rlang::caller_arg(x)
  x_data <- get_data(x)

  # Setting names so `conditions` can be indexed by description or index
  conditions <- get_conditions(x)
  names(conditions) <- get_description(x)
  conditions <- vctrs::vec_slice(
    conditions,
    which
    # TODO: Improve the error message on an invalid slice! The error should have
    # a message like:
    # ! `which` must be an integer or vector of condition descriptions.
    # i Descriptions, in order of index, are:
    # 1. Conditional Formatter 1
    # 2. Conditional Formatter 2
    # ...
    # x_arg = glue::glue('attr({x_name},"conditions")'),
    # value_arg = "which"
  )

  # Occurs after slicing, so invalid `which` still errors for empty conditions.
  if (rlang::is_empty(conditions)) {
    return(rep(TRUE, length(x)))
  }

  out <- evalidate_highlight_fun(
    input = x_data,
    fun = conditions[[1]],
    fun_name = glue::glue('attr({x_name},"conditions")[[{i}]]'),
    fun_type = "condition"
  )

  i <- 2
  op <- if (meets != "all") `|` else `&`
  for (condition in conditions[-1]) {
    out <- op(
      out,
      evalidate_highlight_fun(
        input = x_data,
        fun = condition,
        fun_name = glue::glue('attr({x_name},"conditions")[[{i}]]'),
        fun_type = "condition"
      )
    )
    out[is.na(out)] <- FALSE
    i <- i + 1
  }

  if (meets == "none") !out else out
}
