# todos ------------------------------------------------------------------------

#### Housekeeping:
#
# You got carried away with these errors... Search for their usage and trim
# down to something more reasonable. Also, see how {rlang} does it's errors,
# their system seems pretty slick.
#
# Also the messaging functions at the bottom!

# check ------------------------------------------------------------------------

check_is_highlightable <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  if (is_highlightable(x)) {
    return(x)
  }
  stop_must_not(
    x,
    must = "be a non-bare-list and non-dataframe vector",
    not = "{.obj_type_friendly {x}}",
    bullets = c(i = "See {.help vctrs::obj_is_vector} for details on vectors."),
    x_name = x_name,
    error_class = error_class,
    error_call = error_call
  )
}

check_is_highlight <- function(
    arg,
    x_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
) {
  if (!is_highlight(arg)) {
    stop_must_not(
      arg,
      must = "be a {.cls vlightr_highlight} vector",
      not = "a {.cls {class(arg)}}",
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  arg
}

check_is_list_of_functionish <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {

  if (is.function(x) || rlang::is_formula(x)) {
    return(list(check_is_functionish(x, x_name, error_call, error_class)))
  }

  if (is.list(x)) {
    return(mapply(
      x = x,
      x_name = paste0(x_name, "[[", seq_along(x), "]]"), # TODO: Make an `index_of()` function
      MoreArgs = list(
        error_call = error_call,
        error_class = error_class
      ),
      FUN = check_is_functionish,
      SIMPLIFY = FALSE
    ))
  }

  stop_must_not(
    x,
    must = "a list of functions or one-sided formulas",
    not = "{.obj_type_friendly {arg}}",
    x_name = x_name,
    error_class = error_class,
    error_call = error_call
  )
}

# TODO: Error message here is weird, `highlight_lambda()` just says you need a
# formula. Clean up this errors script!!!
check_is_functionish <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  if (is.function(x)) {
    return(x)
  }
  rlang::try_fetch(
    highlight_lambda(x, x_name = x_name),
    error = function(cnd) {
      cli::cli_abort(
        c(
          cnd$message,
          i = "{.arg {x_name}} must be a function or a one-sided formula."
        ),
        call = error_call,
        class = error_class
      )
    }
  )
}

check_is_list_of_functionish <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
) {
  if (is.function(x) || rlang::is_formula(x)) {
    return(list(check_is_functionish(x, x_name, error_call, error_class)))
  }
  if (is.list(x)) {
    return(mapply(
      x = x,
      x_name = paste0(x_name, "[[", seq_along(x), "]]"), # TODO: Make an `index_of()` function
      MoreArgs = list(
        error_call = error_call,
        error_class = error_class
      ),
      FUN = check_is_functionish,
      SIMPLIFY = FALSE
    ))
  }
  stop_must_not(
    x,
    must = "a list of functions or one-sided formulas",
    not = "{.obj_type_friendly {x}}",
    x_name = x_name,
    error_class = error_class,
    error_call = error_call
  )
}

check_is_list_of_index <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
) {
  if (is.logical(x) || rlang::is_integerish(x)) {
    return(list(x))
  }
  assert_is_a(
    x,
    is = is.list,
    a = "a logical or integer-like vector (or list thereof)",
    x_name = x_name, error_call = error_call, error_class = error_class
  )
  mapply(
    x = x,
    x_name = paste0(x_name, "[[", seq_along(x), "]]"),
    MoreArgs = list(
      is = \(x) is.logical(x) || rlang::is_integerish(x),
      a = "a logical or integer-like vector",
      error_call = error_call,
      error_class = error_class
    ),
    FUN = check_is_a,
    SIMPLIFY = FALSE
  )
}

check_is_list_of_logical <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
) {

  if (is.logical(x)) {
    return(list(x))
  }

  assert_is_a(
    x,
    is = is.list,
    a = "a logical vector or list thereof",
    x_name = x_name, error_call = error_call, error_class = error_class
  )

  mapply(
    x = x,
    x_name = paste0(x_name, "[[", seq_along(x), "]]"),
    MoreArgs = list(
      is = is.logical,
      a = "a logical vector",
      error_call = error_call,
      error_class = error_class
    ),
    FUN = check_is_a,
    SIMPLIFY = FALSE
  )
}

check_is_a <- function(
    x,
    is,
    a,
    not = "{.obj_type_friendly {x}}",
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  if (is(x)) {
    return(x)
  }
  stop_must_be_a(
    x = x,
    a = a,
    not = not,
    x_name = x_name,
    error_call = error_call,
    error_class = error_class
  )
}

assert_is_a <- function(
    x,
    is,
    a,
    not = NULL,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
) {
  if (!is(x)) {
    stop_must_be_a(
      x = x,
      a = a,
      not = not,
      x_name = x_name,
      error_call = error_call,
      error_class = error_class
    )
  }
}

stop_must_be_a <- function(
    x,
    a,
    not = NULL,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
) {
  cli::cli_abort(
    paste0(
      "{.arg {x_name}} must be ", a,
      ", not ", not %||% "{.obj_type_friendly {x}}", "."
    ),
    call = error_call,
    class = error_class
  )
}

check_is_vector <- function(
    x,
    cls,
    len = NULL,
    nas = TRUE,
    arg_must = NULL,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {

  is_valid_cls <- switch(
    cls,
    character = is.character(x),
    numeric = is.numeric(x),
    integer = is.integer(x),
    complex = is.complex(x),
    logical = is.logical(x),
    raw = is.raw(x),
    list = is.list(x),
    inherits(x, cls)
  )
  if (is_valid_cls && (is.null(len) || length(x) == len) && (nas || !anyNA(x))) {
    return(x)
  }

  if (!inherits(x, cls)) {
    arg_must <- arg_must %||% paste("be", a_vector_friendly(cls, len, nas))
    cli::cli_abort(
      c(
        paste0("{.arg {x_name}} must ", arg_must, "."),
        x = "{.arg {x_name}} is class {.cls {class(x)}}."
      ),
      call = error_call,
      class = error_class
    )
  }
  stop_wrong_len_nas(
    arg = x,
    arg_must = arg_must %||% paste("be", a_vector_friendly(cls, len, nas)),
    len = len,
    nas = nas,
    x_name = x_name,
    error_call = error_call,
    error_class = error_class
  )
}

check_is_count <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "highlightr_error"
  ) {

  if (rlang::is_integerish(x, n = 1) && !is.na(x) && x >= 0) {
    return(x)
  }

  check_is_vector(
    x = x,
    cls = "numeric",
    len = 1,
    nas = FALSE,
    arg_must = "be a count.",
    x_name = x_name,
    error_call = error_call,
    error_class = error_class
  )

  error_header <- "{.arg {x_name}} must be a count."
  if (x < 0) {
    cli::cli_abort(
      c(
        error_header,
        x = "{.arg {x_name}} is the negative number {x}."
      ),
      call = error_call,
      class = error_class
    )
  }

  cli::cli_abort(
    c(
      error_header,
      i = "{.arg {x_name} is the number {x}.",
      x = "Can't coerce {x} to an integer without loss of precision."
    ),
    call = error_call,
    class = error_class
  )
}

check_is_string <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  if (rlang::is_string(x)) {
    return(x)
  }
  check_is_vector(
    x = x,
    cls = "character",
    len = 1,
    nas = FALSE,
    arg_must = "be a string.",
    x_name = x_name,
    error_call = error_call,
    error_class = error_class
  )
}

check_is_bool <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  if (rlang::is_bool(x)) {
    return(x)
  }
  check_is_vector(
    x = x,
    cls = "logical",
    len = 1,
    nas = FALSE,
    arg_must = "be a single TRUE or FALSE value.",
    x_name = x_name,
    error_call = error_call,
    error_class = error_class
  )
}

# assert -----------------------------------------------------------------------

assert_same_length <- function(
    x,
    y,
    x_name = rlang::caller_arg(x),
    y_name = rlang::caller_arg(y),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
) {
  if (length(x) != length(y)) {
    cli::cli_abort(
      c(
        "{.arg {x_name}} and {.arg {y_name}} must be the same length.",
        `*` = paste0("{.arg {x_name}} is ", length_friendly(length(x)), "."),
        `*` = paste0("{.arg {y_name}} is ", length_friendly(length(y)), ".")
      ),
      call = error_call,
      class = error_class
    )
  }
}

# stop -------------------------------------------------------------------------

stop_must_not <- function(
    x,
    must,
    not,
    bullets = NULL,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  if (rlang::is_missing(not)) not <- "{.obj_type_friendly {x}}"
  cli::cli_abort(
    c(
      paste0("{.arg {x_name}} must ", must, ", not ", not, "."),
      bullets
    ),
    call = error_call,
    class = error_class
  )
}

stop_wrong_len_nas <- function(
    x,
    arg_must,
    len = NULL,
    nas = TRUE,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {

  arg_len <- length(x)
  len_bullet <- if (!is.null(len) && arg_len != len) {
    paste0("{.arg {x_name}} is ", length_n_friendly(arg_len), ".")
  }

  arg_nas <- is.na(arg)
  nas_bullet <- if (nas && any(arg_nas)) {
    paste0("{.arg {x_name}} is NA or NaN ", at_loc_friendly(arg_nas), ".")
  }

  if (is.null(arg_must)) {
    non_na <- if (nas) " " else "non-NA/NaN "
    arg_must <- paste0("be ", a_length_n_friendly(len), non_na, "object.")
  }
  cli::cli_abort(
    c(
      paste0("{.arg {x_name}} must ", arg_must, "."),
      x = len_bullet,
      x = nas_bullet
    ),
    call = error_call,
    class = error_class
  )
}

stop_different_length <- function(
    x,
    y,
    x_name = rlang::caller_arg(x),
    y_name = rlang::caller_arg(y),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  if (length(x) != length(y)) {
    cli::cli_abort(
      c(
        "{.arg {x_name}} and {.arg {y_name}} must be the same length.",
        `*` = paste0("{.arg {x_name}} is ", length_n_friendly(length(x)), "."),
        `*` = paste0("{.arg {y_name}} is ", length_n_friendly(length(y)), ".")
      ),
      call = error_call,
      class = error_class
    )
  }
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

at_loc_friendly <- function(loc, n_max = 5) {
  loc <- if (is.logical(loc)) which(loc & !is.na(loc)) else loc
  loc <- as.numeric(loc)
  n <- length(loc)
  at <- ngettext(min(n, n_max), "at location ", "at locations ")
  if (n > n_max) {
    paste0(at, "`", deparse(loc[seq(n_max)]), "` and ", n - n_max, " more")
  } else {
    paste0(at, "`", deparse(loc), "`")
  }
}

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

a_vector_friendly <- function(cls, len = NULL, nas = FALSE) {
  len_n <- if (is.null(len)) "a" else a_length_friendly(len)
  non_na <- if (!nas) "non-NA/NaN" else ""
  paste(len_n, non_na, "{.cls {cls}} vector")
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
