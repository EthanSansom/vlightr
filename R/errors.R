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
      not = "a {.cls {class(arg)}}",
      arg_name = arg_name,
      error_call = error_call,
      error_class = error_class
    )
  }
  arg
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
    must = "a list of functions or one-sided formulas",
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

check_is_vector <- function(
    arg,
    cls,
    len = NULL,
    nas = TRUE,
    arg_must = NULL,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {

  is_valid_cls <- switch(
    cls,
    character = is.character(arg),
    numeric = is.numeric(arg),
    integer = is.integer(arg),
    complex = is.complex(arg),
    logical = is.logical(arg),
    raw = is.raw(arg),
    list = is.list(arg),
    inherits(arg, cls)
  )
  if (is_valid_cls && (is.null(len) || length(arg) == len) && (nas || !anyNA(arg))) {
    return(arg)
  }

  if (!inherits(arg, cls)) {
    arg_must <- arg_must %||% paste("be", a_vector_friendly(cls, len, nas))
    cli::cli_abort(
      c(
        paste0("{.arg {arg_name}} must ", arg_must, "."),
        x = "{.arg {arg_name}} is class {.cls {class(arg)}}."
      ),
      call = error_call,
      class = error_class
    )
  }
  stop_wrong_len_nas(
    arg = arg,
    arg_must = arg_must %||% paste("be", a_vector_friendly(cls, len, nas)),
    len = len,
    nas = nas,
    arg_name = arg_name,
    error_call = error_call,
    error_class = error_class
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

  check_is_vector(
    arg = arg,
    cls = "numeric",
    len = 1,
    nas = FALSE,
    arg_must = "be a count.",
    arg_name = arg_name,
    error_call = error_call,
    error_class = error_class
  )

  error_header <- "{.arg {arg_name}} must be a count."
  if (arg < 0) {
    cli::cli_abort(
      c(
        error_header,
        x = "{.arg {arg_name}} is the negative number {arg}."
      ),
      call = error_call,
      class = error_class
    )
  }

  cli::cli_abort(
    c(
      error_header,
      i = "{.arg {arg_name} is the number {arg}.",
      x = "Can't coerce {arg} to an integer without loss of precision."
    ),
    call = error_call,
    class = error_class
  )
}

check_is_string <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  if (rlang::is_string(arg)) {
    return(arg)
  }
  check_is_vector(
    arg = arg,
    cls = "character",
    len = 1,
    nas = FALSE,
    arg_must = "be a string.",
    arg_name = arg_name,
    error_call = error_call,
    error_class = error_class
  )
}

check_is_bool <- function(
    arg,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {
  if (rlang::is_bool(arg)) {
    return(arg)
  }
  check_is_vector(
    arg = arg,
    cls = "logical",
    len = 1,
    nas = FALSE,
    arg_must = "be a single TRUE or FALSE value.",
    arg_name = arg_name,
    error_call = error_call,
    error_class = error_class
  )
}

check_can_slice <- function(
    arg,
    len_to_slice,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {

  arg <- as.integer(check_is_vector(
    arg,
    cls = "numeric",
    nas = FALSE,
    arg_name = arg_name,
    error_call = error_call,
    error_class = error_class
  ))
  if (all(0 < arg & arg < len_to_slice)) {

  }

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

stop_wrong_len_nas <- function(
    arg,
    arg_must,
    len = NULL,
    nas = TRUE,
    arg_name = rlang::caller_arg(arg),
    error_call = rlang::caller_env(),
    error_class = "vlightr_error"
  ) {

  arg_len <- length(arg)
  len_bullet <- if (!is.null(len) && arg_len != len) {
    paste0("{.arg {arg_name}} is ", length_n_friendly(arg_len), ".")
  }

  arg_nas <- is.na(arg)
  nas_bullet <- if (nas && any(arg_nas)) {
    paste0("{.arg {arg_name}} is NA or NaN ", at_loc_friendly(arg_nas), ".")
  }

  if (is.null(arg_must)) {
    non_na <- if (nas) " " else "non-NA/NaN "
    arg_must <- paste0("be ", a_length_n_friendly(len), non_na, "object.")
  }
  cli::cli_abort(
    c(
      paste0("{.arg {arg_name}} must ", arg_must, "."),
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

a_length_n_friendly <- function(len) {
  if (is.null(len)) {
    "a"
  } else if (length(len) == 2) {
    paste0("a length [", len[[1]], "-", len[[2]], "]")
  } else if (len == 1) {
    paste0("a scalar")
  } else if (len > 0) {
    paste0(" a length ", len)
  } else {
    "an empty"
  }
}

length_n_friendly <- function(len) {
  if (is.null(len)) {
    ""
  } else if (length(len) == 2) {
    paste0("length [", len[[1]], "-", len[[2]], "]")
  } else if (len == 1) {
    "length 1"
  } else if (len > 0) {
    paste("length", len)
  } else {
    "empty"
  }
}

a_vector_friendly <- function(cls, len = NULL, nas = FALSE) {
  len_n <- if (is.null(len)) "a" else a_length_n_friendly(len, prefix = TRUE)
  non_na <- if (!nas) "non-NA/NaN" else ""
  paste(len_n, non_na, "{.cls {cls}} vector")
}

upper1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
