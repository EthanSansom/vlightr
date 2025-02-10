# check ------------------------------------------------------------------------

check_is_highlightable <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_non_highlightable_error"
  ) {
  check_must_not(
    x,
    test = is_highlightable,
    must = "be a non-bare-list and non-dataframe vector",
    x_name = x_name,
    error_bullets = c(
      i = "See {.help vctrs::obj_is_vector} for details on vectors."
    ),
    error_class = error_class,
    error_call = error_call
  )
}

check_is_highlight <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = character()
) {
  check_must_not(
    x,
    test = is_highlight,
    must = "be a {.cls vlightr_highlight} vector",
    x_name = x_name,
    error_call = error_call,
    error_class = error_class
  )
}

check_is_string <- function(
    x,
    nochar_ok = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = character()
  ) {
  if (rlang::is_string(x) && (nochar_ok || x != "")) {
    return(x)
  }
  if (is.character(x) && length(x) > 1) {
    not <- "{a_length_friendly(x)} character vector"
  } else {
    not <- "{.obj_type_friendly {x}}"
  }
  stop_must_not(
    x,
    must = "be a non-empty string",
    not = not,
    x_name = x_name,
    error_call = error_call,
    error_class = error_class
  )
}

check_is_count <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = character()
) {
  if (rlang::is_scalar_integerish(x) && x >= 0) {
    return(x)
  }
  if (length(x) != 1) {
    cli::cli_abort(
      c(
        "{.arg {x_name}} must be a scalar non-negative whole number.",
        x = "{.arg {x_name}} is {length_friendly(length(x))}."
      ),
      call = error_call,
      class = c(error_class, "vlightr_error")
    )
  }
  if (is.na(x) || rlang::is_integerish(x)) {
    not <- if (is.na(x)) "an NA value" else "a negative number"
    stop_must_not(
      x,
      must = "be a scalar non-negative whole number",
      not = not,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  cli::cli_abort(
    c(
      "{.arg {x_name}} must be a scalar non-negative whole number.",
      x = "{.arg {x_name}} is the decimal number {x}."
    ),
    call = error_call,
    class = c(error_class, "vlightr_error")
  )
}

check_must_not <- function(
    x,
    test,
    must,
    not = "{.obj_type_friendly {x}}",
    x_name = rlang::caller_arg(x),
    error_bullets = character(),
    error_call = rlang::caller_env(),
    error_class = character()
  ) {
  if (test(x)) {
    return(x)
  }
  stop_must_not(
    x,
    must = must,
    not = not,
    x_name = x_name,
    error_bullets = error_bullets,
    error_call = error_call,
    error_class = error_class
  )
}

# check list -------------------------------------------------------------------

check_is_list_of_functionish <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_input_type_error"
) {
  out <- check_is_list_of(
    x,
    test = \(x) is.function(x) || is_one_sided_formula(x),
    of_whats = "functions or one-sided formulas",
    x_name = x_name,
    error_call = error_call,
    error_class = error_class
  )
  # Do some extra work to provide specific errors for formulas and coerce
  # one-sided-scoped formulas to functions using `highlight_lambda()`.
  for (i in seq_along(out)) {
    if (is.function(out[[i]])) {
      next
    }
    if (is_one_sided_formula(out[[i]])) {
      out[[i]] <- highlight_lambda(out[[i]])
      next
    }
    x_i <- out[[i]]
    x_i_name <- index_name(x_name, i)
    if (!is.environment(rlang::f_env(x_i))) {
      cli::cli_abort(
        c(
          "{.arg {x_i_name}} must be a function or a one-sided formula carrying an environment.",
          i = "{.arg {x_i_name}} is a formula.",
          x = "{.arg {x_i_name}} is not carrying an environment."
        ),
        call = error_call,
        class = c(error_class, "vlightr_error")
      )
    }
    stop_must_not(
      x_i,
      x_name = x_i_name,
      must = "be a one-sided formula",
      not = "a two-sided formula",
      error_call = error_call,
      error_class = error_class
    )
  }
  out
}

check_is_list_of_index <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_input_type_error"
) {
  check_is_list_of(
    x,
    test = \(x) is.logical(x) || rlang::is_integerish(x),
    of_whats = "logical or integer-like vectors",
    x_name = x_name,
    error_call = error_call,
    error_class = error_class
  )
}

check_is_list_of_highlighter <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_input_type_error"
) {
  check_is_list_of(
    x,
    test = is_highlighter,
    of_whats = "{.cls vlightr_highlighter} functions",
    x_name = x_name,
    error_call = error_call,
    error_class = error_class
  )
}

check_is_list_of <- function(
    x,
    test,
    of_whats,
    not = "{.obj_type_friendly {x}}",
    of_not = "{.obj_type_friendly {x}}",
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = character()
) {
  if (test(x)) {
    return(list(x))
  }
  if (!is.list(x)) {
    stop_must_not(
      x = x,
      must = paste("be a list of", of_whats),
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  mapply(
    x = x,
    x_name = index_name(x_name, seq_along(x)),
    MoreArgs = list(
      test = test,
      must = paste("be a", unplural(of_whats)),
      not = of_not,
      error_call = error_call,
      error_class = error_class
    ),
    FUN = check_must_not,
    SIMPLIFY = FALSE
  )
}

# assert -----------------------------------------------------------------------

assert_same_length <- function(
    x,
    y,
    x_name = rlang::caller_arg(x),
    y_name = rlang::caller_arg(y),
    error_call = rlang::caller_env(),
    error_class = "vlightr_input_size_error"
) {
  if (length(x) != length(y)) {
    cli::cli_abort(
      c(
        "{.arg {x_name}} and {.arg {y_name}} must be the same length.",
        `*` = paste0("{.arg {x_name}} is ", length_friendly(length(x)), "."),
        `*` = paste0("{.arg {y_name}} is ", length_friendly(length(y)), ".")
      ),
      call = error_call,
      class = c(error_class, "vlightr_error")
    )
  }
}

assert_arg_match_internal <- function(
    x,
    values,
    subset = FALSE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env()
  ) {
  if (
    (subset && !is.character(x)) ||
    (!subset && !rlang::is_string(x)) ||
    any(x %notin% values)
  ) {
    values <- encodeString(values, quote = '"')
    not <- if (!subset && rlang::is_string(x)) {
      "the string {.val {x}}."
    } else {
      "{.obj_type_friendly {x}}."
    }
    stop_internal(
      paste("{.arg {x_name}} must be one of {.or {values}}, not", not),
      error_call = error_call
    )
  }
}

# stop -------------------------------------------------------------------------

stop_must_not <- function(
    x,
    must,
    not,
    x_name = rlang::caller_arg(x),
    error_bullets = character(),
    error_call = rlang::caller_env(),
    error_class = character()
  ) {
  if (rlang::is_missing(not)) not <- "{.obj_type_friendly {x}}"
  cli::cli_abort(
    c(
      paste0("{.arg {x_name}} must ", must, ", not ", not, "."),
      error_bullets
    ),
    call = error_call,
    class = c(error_class, "vlightr_error")
  )
}

stop_internal <- function(bullets, error_call = rlang::caller_env()) {
  cli::cli_abort(
    bullets,
    call = error_call,
    class = c("vlightr_internal_error", "vlightr_error"),
    .internal = TRUE,
    .envir = rlang::caller_env()
  )
}

# messaging --------------------------------------------------------------------

a_length_friendly <- function(x) {
  switch(
    as.character(length(x)),
    `0` = "an empty",
    `1` = "a scalar",
    paste("a length", length(x))
  )
}

length_friendly <- function(len) {
  switch(
    as.character(len),
    `0` = "empty",
    paste("length", len)
  )
}

upper1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

commas <- function(x, n = NULL) {
  if (!is.null(n) && length(x) > n) {
    length(x) <- n
    paste0(paste(x, collapse = ", "), ", ...")
  } else {
    paste(x, collapse = ", ")
  }
}

oxford <- function(x, sep = ", ", last = "or", n = NULL) {
  x_len <- length(x)
  if (x_len == 1) {
    return(paste(x))
  }
  if (!is.null(n) && x_len > n) {
    paste0(paste(x, collapse = sep), sep, "...")
  }
  if (x_len == 2) sep <- " "
  paste(paste(x[-x_len], collapse = sep), last, x[[x_len]], sep = sep)
}

index_name <- function(x_name, indices) {
  paste0(x_name, "[[", indices, "]]")
}

dot_name <- function(indices) {
  paste0("..", indices)
}

unplural <- function(x) {
  nchar_x <- nchar(x)
  if (substr(x, nchar_x, nchar_x) == "s") {
    return(substr(x, 1, nchar_x - 1))
  }
  x
}
