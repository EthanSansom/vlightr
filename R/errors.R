# check ------------------------------------------------------------------------

check_is_highlightable <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  if (is_highlightable(arg)) {
    return(arg)
  }
  stop_must_not(
    arg,
    must = "be a non-bare-list and non-dataframe vector",
    not = "{.obj_type_friendly {arg}}",
    bullets = c(i = "See {.help vctrs::obj_is_vector} for details on vectors."),
    arg_name = arg_name,
    error_class = error_class,
    error_call = error_call
  )
}

check_is_list_of_functionish <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {

  if (is.function(arg) || rlang::is_formula(arg)) {
    out <- list(rlang::as_function(arg, call = error_call))
    return(out)
  }
  if (is.list(arg)) {
    out <- vector("list", length(arg))
    for (i in seq_along(out)) {
      out[[i]] <- check_is_functionish(
        arg = arg[[i]],
        arg_name = paste0(arg_name, "[[", i, "]]"),
        error_call = error_call,
        error_class = error_class
      )
    }
    return(out)
  }
  stop_must_not(
    arg,
    must = "a list of functions or one-sided formulas.",
    not = "{.obj_type_friendly {arg}}",
    arg_name = arg_name,
    error_class = error_class,
    error_call = error_call
  )
}

check_is_functionish <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
) {
  rlang::try_fetch(
    rlang::as_function(x = arg, arg = arg_name, call = error_call),
    error = function(cnd) {
      cli::cli_abort(
        "{.arg {arg_name}} must be a function or one-sided formula.",
        parent = cnd,
        call = error_call,
        class = error_class
      )
    }
  )
}

check_is_highlight <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  if (!is_highlight(arg)) {
    stop_must_not(
      arg,
      must = "be a {.cls vlightr_highlight} vector",
      not = "a {.cls {class(arg)}",
      arg_name = arg_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  arg
}

check_is_vector <- function(
    arg,
    cls,
    len = NULL,
    nas = TRUE,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
) {

  if (!inherits(arg, cls)) {
    len_n <- if (is.null(len)) "a" else length_n_friendly(len, prefix = TRUE)
    non_na <- if (!nas) "non-NA" else ""
    cli::cli_abort(
      c(
        paste("{.arg {arg_name}} must be", len_n, non_na, "{.cls {cls}} vector."),
        x = "{.arg {arg_name}} is class {.cls {class(arg)}}."
      ),
      call = error_call,
      class = error_class
    )
  }

  if ((is.null(len) || length(arg) == len) && (nas || !anyNA(arg))) {
    return(arg)
  }

  arg_len <- length(arg)
  arg_nas <- is.na(arg)
  if (is.null(len)) {
    len_n <- "a"
    len_bullet <- NULL
  } else {
    len_n <- length_n_friendly(len, prefix = TRUE)
    len_bullet <- if (arg_len != len) {
      paste0("{.arg {arg_name}} is ", length_n_friendly(arg_len), ".")
    }
  }
  if (nas) {
    non_na <- ""
    nas_bullet <- NULL
  } else {
    non_na <- "non-NA"
    nas_bullet <- if (any(arg_nas)) {
      paste0("{.arg {arg_name}} is NA or NaN ", at_locations(arg_nas), ".")
    }
  }

  cli::cli_abort(
    c(
      paste("{.arg {arg_name}} must be", len_n, non_na, "{.cls {cls}} vector."),
      i = "{.arg {arg_name}} is class {.cls {cls}}.",
      x = len_bullet,
      x = nas_bullet
    ),
    call = error_call,
    class = error_class
  )
}

check_is_count <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "highlightr_error"
  ) {

  if (rlang::is_integerish(arg, n = 1) && !is.na(arg) && arg >= 0) {
    return(arg)
  }

  header <- "{.arg {arg_name}} must be a count."
  if (!is.numeric(arg)) {
    cli::cli_abort(
      c(header, x = "{.arg {arg_name}} is class {.cls {class(arg)}}."),
      call = error_call,
      class = error_class
    )
  }
  if (!rlang::is_integerish(arg)) {
    cli::cli_abort(
      c(
        header,
        x = "Can't coerce {.arg {arg_name}} to an integer without loss of precision."
      ),
      call = error_call,
      class = error_class
    )
  }
  if (length(arg) == 1 && !is.na(arg) && arg < 0) {
    cli::cli_abort(
      c(header, x = "{.arg {arg_name}} is the negative number {arg}."),
      call = error_call,
      class = error_class
    )
  }

  len_bullet <- if (length(arg) != 1) "{.arg {arg_name}} is length {length(arg)}."
  nas_bullet <- if (any(is.na(arg))) {
    paste0("{.arg {arg_name}} is NA or NaN ", locations_friendly(is.na(arg)), ".")
  }
  cli::cli_abort(
    c(
      header,
      i = "{.arg {arg_name}} is an integerish number.",
      x = len_bullet,
      x = nas_bullet
    ),
    call = error_call,
    class = error_class
  )
}

# stop -------------------------------------------------------------------------

stop_must_not <- function(
    arg,
    must,
    not,
    bullets = NULL,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  cli::cli_abort(
    c(
      paste0("{.arg {arg_name}} must ", must, ", not ", not, "."),
      bullets
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
        `*` = "{.arg {x_name}} is length {length(x)}.",
        `*` = "{.arg {y_name}} is length {length(y)}."
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

locations_friendly <- function(loc, n_max = 5) {
  loc <- if (is.logical(loc)) which(loc & !is.na(loc)) else loc
  loc <- as.numeric(loc)
  n <- length(loc)
  at <- if (min(n, n_max) == 1) "at location " else "at locations "
  if (n > n_max) {
    paste0(at, "`", deparse(loc[seq(n_max)]), "` and ", n - n_max, " more")
  } else {
    paste0(at, "`", deparse(loc), "`")
  }
}

length_n_friendly <- function(len, prefix = FALSE) {
  a <- if (prefix) "a " else ""
  if (length(len) == 2) {
    paste0(a, "length [", len[[1]], "-", len[[2]], "]")
  } else if (len == 1) {
    paste0(a, "scalar")
  } else if (len > 0) {
    paste0(a, "length ", len)
  } else {
    if (prefix) "an empty" else "empty"
  }
}
