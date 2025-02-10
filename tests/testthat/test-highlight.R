# highlight --------------------------------------------------------------------

test_that("highlight() returns zero-length vector", {

  hlt <- highlight()
  expect_s3_class(hlt, "vlightr_highlight")
  expect_length(hlt, 0L)

  hlt <- highlight(numeric())
  expect_s3_class(hlt, "vlightr_highlight")
  expect_length(hlt, 0L)

})

test_that("highlight() expects a vector", {

  expect_error(
    highlight(list()),
    class = "vlightr_non_highlightable_error"
  )
  expect_error(
    highlight(list(1, 2, 3, 4)),
    class = "vlightr_non_highlightable_error"
  )
  expect_error(
    highlight(data.frame()),
    class = "vlightr_non_highlightable_error"
  )
  expect_error(
    highlight(data.frame(x = 1:5)),
    class = "vlightr_non_highlightable_error"
  )
  expect_error(
    highlight(NULL),
    class = "vlightr_non_highlightable_error"
  )
  expect_error(
    highlight(base::mean),
    class = "vlightr_non_highlightable_error"
  )

})

test_that("highlight() can recieve base R vectors", {

  # TODO: Maybe make a fixture for all of your desired integer types
  num <- numeric()
  int <- integer()
  byt <- raw()
  cmp <- complex()
  lgl <- logical()
  chr <- character()
  fac <- factor()
  dte <- as.Date(numeric())
  pxc <- as.POSIXct(numeric())
  pxl <- as.POSIXlt(numeric())
  dft <- difftime(numeric(), numeric())
  tms <- ts()

  expect_no_error(highlight(num))
  expect_no_error(highlight(int))
  expect_no_error(highlight(byt))
  expect_no_error(highlight(cmp))
  expect_no_error(highlight(lgl))
  expect_no_error(highlight(chr))
  expect_no_error(highlight(fac))
  expect_no_error(highlight(dte))
  expect_no_error(highlight(pxc))
  expect_no_error(highlight(pxl))
  expect_no_error(highlight(dft))
  expect_no_error(highlight(tms))

  expect_no_error(highlight(numeric(10)))
  expect_no_error(highlight(integer(10)))
  expect_no_error(highlight(raw(10)))
  expect_no_error(highlight(complex(10)))
  expect_no_error(highlight(logical(10)))
  expect_no_error(highlight(character(10)))
  expect_no_error(highlight(factor(1:10)))
  expect_no_error(highlight(factor(1:10, labels = letters[1:10])))
  expect_no_error(highlight(ordered(1:10)))
  expect_no_error(highlight(ordered(1:10, labels = letters[1:10])))
  expect_no_error(highlight(ts(1:10)))

  dates <- paste0("2020-01-0", 1:9)
  datetimes <- paste(dates, "01:00")
  expect_no_error(highlight(as.Date(dates)))
  expect_no_error(highlight(as.POSIXct(datetimes)))
  expect_no_error(highlight(as.POSIXlt(datetimes)))
  expect_no_error(highlight(difftime(dates, datetimes)))

})

test_that("highlight() can recieve external S4 vectors", {

  withr::local_package("lubridate")
  expect_true(isS4(lubridate::interval()))

  starts <- as.Date(paste0("2020-02-0", 1:9))
  ends <- as.Date(paste0("2020-03-0", 1:9))
  expect_no_error(highlight(lubridate::interval()))
  expect_no_error(highlight(lubridate::interval(starts, ends)))

})

test_that("highlight() can recieve external S3 vectors", {

  withr::local_package("ivs")
  expect_no_error(highlight(ivs::iv(numeric(), numeric())))
  expect_no_error(highlight(ivs::iv(1:10, 2:11)))

})

test_that("highlight() `.t` and `.f` expect a function, lamda, or list thereof", {

  num10 <- 1:10
  expect_no_error(highlight(num10, .t = is.integer, .f = paste))
  expect_no_error(highlight(num10, .t = list(is.integer), .f = paste))
  expect_no_error(highlight(num10, .t = is.integer, .f = list(paste)))
  expect_no_error(highlight(num10, .t = ~ .x > 5, .f = paste))
  expect_no_error(highlight(num10, .t = is.integer,  .f = ~ paste0(.x, "!")))
  expect_no_error(highlight(num10, .t = list(is.integer, ~ .x > 5),  .f = list(paste, ~ paste0(.x, "!"))))

  expect_error(
    highlight(num10, .t = 10, .f = paste),
    class = "vlightr_input_type_error"
  )
  expect_error(
    highlight(num10, .t = .x ~ .x, .f = paste),
    class = "vlightr_input_type_error"
  )
  expect_error(
    highlight(num10, .t = is.integer, .f = "A"),
    class = "vlightr_input_type_error"
  )
  expect_error(
    highlight(num10, .t = is.integer, .f = .x ~ .x),
    class = "vlightr_input_type_error"
  )

})

test_that("highlight() accepts empty lists for `.t` and `.f`", {

  expect_no_error(highlight(1:10, .t = list(), .f = list()))
  hlt <- highlight(1:10, .t = list(), .f = list())
  expect_identical(tests(hlt), list())
  expect_identical(formatters(hlt), list())

})

test_that("highlight() errors when `.t` and `.f` are different lengths", {

  expect_error(
    highlight(1:10, .t = list(), .f = paste),
    class = "vlightr_input_size_error"
  )
  expect_error(
    highlight(1:10, .t = is.integer, .f = list()),
    class = "vlightr_input_size_error"
  )
  expect_error(
    highlight(1:10, .t = list(is.integer, is.numeric), .f = list(paste)),
    class = "vlightr_input_size_error"
  )
  expect_error(
    highlight(1:10, .t = list(is.integer), .f = list(paste, paste0)),
    class = "vlightr_input_size_error"
  )

  # Failure should occur even with 0-length data `.x`
  expect_error(
    highlight(.t = list(), .f = paste),
    class = "vlightr_input_size_error"
  )
  expect_error(
    highlight(.t = is.integer, .f = list()),
    class = "vlightr_input_size_error"
  )
  expect_error(
    highlight(.t = list(is.integer, is.numeric), .f = list(paste)),
    class = "vlightr_input_size_error"
  )
  expect_error(
    highlight(.t = list(is.integer), .f = list(paste, paste0)),
    class = "vlightr_input_size_error"
  )

})

# todo -------------------------------------------------------------------------

# - test highlight methods like `c()` and arithmetic
#   - do we add together the formatters and tests correctly?
#   - do we take the unique formatters and tests?
#   - what is the order of formatters and tests?
# - make sure that we follow the casting and coercion rules of underlying data
# - check how attributes are handled (use factors, `c()` diff highlighted factors)
# - check for error types and messages when an `hl()` interacts with a non-highlighted vector

# NOTE: Check for the actual highlighting results separate tests for `format()`
# and `print()`. Don't bother checking these kinds of things for each highlight
# class. This includes things like:
#
# - formats are applied correctly (test format(x_hl) identical to formatter(x_hl_data))
# - formats are applied in the order supplied
# - formatting _case vectors works as expected
# - does formatting work in a tibble()? Do snapshots! This way if {pillar} changes
#   we'll know
#
# For `print()`
# - do snapshots
# - test the alignment and ANSI support
# - check the use of `n`
# - check the use of `quoted`
