.onLoad <- function(libname, pkgname) {
  opts <- options()
  vlightr_opts <- list(
    vlightr.default_test = false,
    vlightr.default_formatter = background("br_yellow")
  )
  set_opts_at <- names(vlightr_opts) %notin% names(opts)
  if (any(set_opts_at)) options(vlightr_opts[set_opts_at])
  invisible()
}
