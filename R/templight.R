# templight --------------------------------------------------------------------

# TODO: Document and export
templight <- function(.x, .at, .f = getOption("vlightr.default_formatter")) {
  x <- check_is_highlightable(.x)
  tests <- check_is_list_of_index(.at) |> map(as_index_test)
  formatters <- check_is_list_of_functionish(.f)
  assert_same_length(tests, formatters, x_name = ".at", y_name = ".f")

  validate_highlight(
    x = new_highlight(x, tests, formatters, subclass = "vlightr_templight"),
    x_name = rlang::caller_arg(.x)
  )
}

# TODO: Document and export
tl <- templight

# TODO: Document and export
templight_mult <- function(.x, ...) {
  rlang::check_required(.x)
  x <- check_is_highlightable(.x)
  funs <- prepare_templight_functions(...)

  validate_highlight(
    x = new_highlight(x, funs$tests, funs$formatters, subclass = "vlightr_templight"),
    x_name = rlang::caller_arg(.x)
  )
}

# TODO: Document and export
tl_mult <- templight_mult

# TODO: Document and export
templight_case <- function(x, ...) {
  rlang::check_required(.x)
  x <- check_is_highlightable(.x)
  funs <- prepare_templight_functions(...)

  validate_highlight(
    x = new_highlight(
      x,
      funs$tests,
      funs$formatters,
      subclass = c("vlightr_templight", "vlightr_highlight_case")
    ),
    x_name = rlang::caller_arg(.x)
  )
}

# TODO: Document and export
tl_case <- templight_case

# TODO: Document and export
is_templight <- function(x) {
  inherits(x, "vlightr_templight")
}

# TODO: Document and export
is_templight_case <- function(x) {
  rlang::inherits_all(x, c("vlightr_templight", "vlightr_highlight_case"))
}

# templighter ------------------------------------------------------------------

# TODO: Document and export
new_templighter <- function(tests, formatters, subclass = character()) {
  highlight_subclass <- gsub("highlighter", "highlight", subclass)
  force(tests)
  force(formatters)

  out <- function(.x) {
    rlang::check_required(.x)
    x <- check_is_highlightable(.x)
    validate_highlight(
      x = new_highlight(
        .x,
        tests = tests,
        formatters = formatters,
        subclass = c("vlightr_templight", highlight_subclass)
      ),
      x_name = rlang::caller_arg(.x)
    )
  }
  attr(out, "tests") <- tests
  attr(out, "formatters") <- formatters
  class(out) <- c(
    "vlightr_templighter",
    subclass,
    "vlightr_highlighter",
    "function"
  )
  out
}

# TODO: Document and export
templighter <- function(.t, .at) {
  rlang::check_required(.t)
  rlang::check_required(.at)
  tests <- check_is_list_of_index(.at) |> map(as_index_test)
  formatters <- check_is_list_of_functionish(.f)
  assert_same_length(tests, formatters, x_name = ".at", y_name = ".f")

  new_templighter(tests = tests, formatters = formatters)
}

# TODO: Document and export
templighter_mult <- function(...) {
  funs <- prepare_templight_functions(...)
  new_templighter(tests = funs$tests, formatters = funs$formatters)
}

# TODO: Document and export
templighter_case <- function(...) {
  funs <- prepare_templight_functions(...)
  new_templighter(
    tests = funs$tests,
    formatters = funs$formatters,
    subclass = "vlightr_highlighter_case"
  )
}

is_templighter <- function(x) {
  inherits(x, "vlightr_templighter")
}

is_templighter_case <- function(x) {
  rlang::inherits_all(x, c("vlightr_templighter", "vlighter_highlighter_case"))
}

# templight helpers ------------------------------------------------------------

prepare_templight_functions <- function(...) {

  error_call <- rlang::caller_env()
  dots <- rlang::list2(...)
  formulas_at <- map_lgl(dots, is_formula)
  templighters_at <- map_lgl(dots, is_templighter)

  if (!all(formulas_at | highlighters_at)) {
    invalid_at <- which.min(formulas_at | templighters_at)
    cli::cli_abort(
      c(
        "Arguments to `...` must be a formula or {.cls vlightr_templightr} function.",
        x = "`..{invalid_at}` is {.obj_type_friendly {dots[invalid_at]}}."
      ),
      call = error_call,
      class = "vlighter_error"
    )
  }

  out <- vector("list", length(dots))
  out[formulas_at] <- mapply(
    frm = dots[formulas_at],
    frm_name = paste0("..", which(formulas_at)),
    FUN = split_templight_formula,
    MoreArgs = list(error_call = rlang::caller_env()),
    SIMPLIFY = FALSE
  )
  out[templighters_at] <- map(dots[highlighters_at], get_highlight_functions)

  # See `prepare_highlight_functions()` for details about this line
  if (any(highlighters_at)) {
    out <- vctrs::list_unchop(out)
  }

  list(
    tests = map(out, `[[`, "test"),
    formatters = map(out, `[[`, "formatter")
  )
}

split_templight_formula <- function(frm, frm_name, error_call) {
  if (is_one_sided_formula(frm)) {
    test <- true_along
    formatter <- new_formatter_function(rlang::f_rhs(frm), rlang::f_env(frm))
  }
  else {
    env <- rlang::f_env(frm)
    lhs_result <- rlang::try_fetch(
      eval(rlang::f_lhs(frm), env),
      error = function(cnd) {
        cli::cli_abort(
          c(
            "Can't convert left-hand-side of formula {.arg {frm_name}} to a function.",
            i = paste(
              "The left-hand-side of {.arg {frm_name}} must be a logical",
              "or integer-like vector."
            )
          ),
          parent = cnd,
          call = error_call,
          class = "vlightr_error"
        )
      }
    )
    if (!(is.logical(lhs_result) || rlang::is_integerish(lhs_result))) {
      cli::cli_abort(
        c(
          paste(
            "The left-hand-side of formula {.arg {frm_name}} must be a logical",
            "or integer-like vector."
          ),
          x = "The left-hand-side of {.arg {frm_name}} is {.obj_type_friendly {lhs_result}}."
        ),
        call = error_call,
        class = "vlightr_error"
      )
    }
    test <- as_index_test(lhs_result)
    formatter <- new_formatter_function(rlang::f_rhs(frm), env)
  }
  list(test = test, formatter = formatter)
}

as_index_test <- function(indices) {
  if (is.logical(indices)) {
    indices[is.na(indices)] <- FALSE
    indices <- which(indices)
  }
  else {
    indices <- unique(indices)
  }
  function(x) {
    seq_along(x) %in% indices
  }
}

