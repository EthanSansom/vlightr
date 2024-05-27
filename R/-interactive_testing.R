if (FALSE) {

  load_all()

  # Opening Example ----

  # We can print NA values as red for example
  x <- as.integer(c(NA, 1, NA, 0))
  x_hl <- highlight(x, is.na, cli::col_red)
  print(x)
  print(x_hl)

  # Suppose we're just tired of thinking about NA's
  x_hl <- highlight(x, is.na, \(x) cli::col_green("NOT A PROBLEM"))
  x_hl

  # Un-highlighting, we can see that the underlying data is persistent
  un_hightlight(x_hl)

  # The conditional formatting is persistent. We can index the vector
  # without losing it's formatting
  x_hl[is.na(x_hl)]

  # However, when we attempt to actually brush our NA's under the rug, we hit a snag
  x_hl[is.na(x_hl)] <- 0L

  # Highlights only play well with other highlights. We can highlight `0L` to
  # make this work.
  x_hl[is.na(x_hl)] <- highlight(0L)
  x_hl

  # For cases like these, the `highlight()` shorthand `hl()` is useful
  x_hl[is.na(x)] <- hl(NA)
  x_hl

  # Casting or coercing a highlight is also undefined behavior
  as.logical(x_hl)

  # We can get around this by first un-highlighting and then re-highlighting our vector
  re_highlight(as.logical(un_hightlight(x_hl)), x_hl)

  # Because that's a lot of typing, `re_highlight` and `un_highlight` have
  # shorthands `rl` and `ul` (think "re-light" and "un-light").
  rl(as.logical(ul(x_hl)), x_hl)

  # For operations like this, the highlight pipe `%hl>%` can be used as well. The
  # highlight pipe uses the `magrittr` pipe `%>%` under the hood, but un-highlights
  # it's input and re-highlights it's output.
  x_hl %hl>% as.logical()

  # Highlights will happily be coerced to the underlying type of other highlights,
  # when possible
  y_hl <- hl(c(2.5, 3.5))
  c(x_hl, y_hl)

  # Further, when multiple highlights are combined, so are their formatters
  y_hl <- highlight(c(2.5, 3.5), ~ .x <= 1, ~ paste(.x, "(small)"))
  c(x_hl, y_hl)

  # format_once ----
  x_hl <- highlight(
    1:10,
    list(~ .x > 7, ~ .x > 5),
    list(~paste0("<", .x, ">"), ~paste0("[", .x, "]")),
    format_once = FALSE
  )

  # Both formats applied to > 7
  x_hl

  # Only one format applied
  attr(x_hl, "format_once") <- TRUE
  x_hl

  # combining incompatible highlights ----
  stop_twos <- function(x) if (any(x == 2)) stop("No Twos") else TRUE
  x <- hl(10, stop_twos, cli::bg_br_yellow)
  y <- hl(1:5)
  x
  y
  c(x, y)

  # precedence ----
  x_hl <- highlight(
    1:10,
    list(~ .x > 7, ~ .x > 5),
    list(~paste0("<", .x, ">"), ~paste0("[", .x, "]"))
  )

  x_hl
  attr(x_hl, "precedence") <- rev(attr(x_hl, "precedence"))
  x_hl

  # Test the pipe ----
  x <- highlight(1:10)
  x %hl>% as.character()
  rl(as.character(ul(x)), x)
  re_highlight(as.character(un_hightlight(x)), x)

  # Test with other vectors ----
  library(ivs)
  library(lubridate)
  library(scales)

  # A secretive `ivs` <ivs_iv>
  iv_highlighted <- highlight(iv(1:5, 2:6), ~iv_start(.x) > 3, ~ "SECRET")
  iv_highlighted

  # A green `lubridate` <Interval>
  int_highlighted <- highlight(interval(now(), now() + days(10)), ~TRUE, cli::col_green)
  int_highlighted

  # Some Highlighters ----

  # A Secret String
  secret <- highlighter(~TRUE, ~sapply(nchar(.x), \(n) strrep("x", n)))
  x <- c("This", "is", "a", "secret", "message")
  print(x)
  print(secret(x))

  # Automatically turn emojis
  library(emo)
  emoji_keywords <- unlist(emo::ji_keyword, use.names = FALSE)
  autoemoji <- highlighter(~ .x %in% emoji_keywords, ~ sapply(.x, emo::ji))
  x <- c("I'm", "angry", "because", "my", "hotdog", "caught", "on", "fire")
  print(x)
  print(autoemoji(x))
  print(ul(autoemoji(x)))
  x |> autoemoji() |> format() |> cat()

  x <- autoemoji(emoji_keywords)
  x %hl>% { grep("cart", ., value = TRUE) }

  # Dummy vector labels
  dummy <- highlighter(
    list(~ .x == 1, ~ .x == 0, is.na),
    list(~ "Yes", ~ "No", cli::col_red)
  )
  x <- sample(c(1, 0, NA), 10, TRUE)
  print(x)
  print(dummy(x))

  # Identify the top 1% of observations
  top_percent <- highlighter(
    ~.x >= quantile(.x, 0.99),
    cli::bg_br_yellow,
    ">=99th Percentile"
  )
  x <- runif(20)
  print(x)
  print(top_percent(x))
  describe_highlight(top_percent(x))

  # Use labellers from `scales` as the initial formatter
  dollar <- highlighter(is.na, cli::col_red, init_formatter = scales::label_dollar())
  dollar(c(1, 4, 5, 6, 0.25, NA))

  # `label_number_auto` uses heuristics to label numbers in a readable way
  auto_number_formatter <- scales::label_number_auto()
  nice_number_formatter <- function(x) {
    out <- suppressWarnings(auto_number_formatter(x))
    out[is.na(x)] <- "NA"
    unlist(out)
  }
  nice_number <- highlighter(init_formatter = nice_number_formatter)

  # This returns a list in this case, where NA is `character(0)`. Hence the wrapping
  # in `nice_number_formatter`.
  x <- c(-1, 200000, 56, 12, 0.0001, 2, NA_real_)
  auto_number_formatter(x)
  x_hl <- nice_number(x)
  print(x)
  print(x_hl)

  # `scales::label_number_auto()` adapts to the current contents of `x_hl`
  x[!is.na(x) & x > 100]
  x_hl[!is.na(x_hl) & x_hl > hl(100)]
}
