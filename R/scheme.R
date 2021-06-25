#' @export
#' @rdname scheme
scheme <- function(instructions) {
  if (length(instructions) == 0) {
    rlang::abort("Must provide a valid `instructions` parameter")
  }
  purrr::map(instructions, check_instruction)
  structure(
    instructions,
    class = "chewie_scheme"
  )
}

#' @export
#' @rdname scheme
is.chewie_scheme <- function(x) inherits(x, "chewie_scheme")

#' @export
#' @rdname scheme
print.chewie_scheme <- function(x) {
  cat("<chewie_scheme>", "\n", sep = "")
  for (item in x) {
    cat("  ")
    print.chewie_instruction(item)
  }
  invisible(x)
}

#' @export
#' @rdname as_scheme
as_scheme <- function(x) {
  UseMethod("as_scheme")
}

#' Loads scheme from a dataframe
#'
#' @family scheme
#'
#' @keywords scheme
#'
#' @export
as_scheme.data.frame <- function(x) {
  rows <- purrr::transpose(x)

  instructions <- purrr::map(rows, as_instruction)

  scheme(instructions)
}

#' Chew instructions from scheme
#'
#' Loads scheme and parses a page using the instructions contained by it.
#'
#' @param scheme a `scheme` type object containing scraping instructions
#' @param page a product of `rvest::read_html()`
#' @param url a link to a page
#'
#' @keywords chew
#'
#' @export
chew <- function(scheme, page = NULL, url = NULL, ...) {
  if (
    sum(!inherits(scheme, "chewie_scheme"),
        !inherits(scheme, "data.frame")) != 1
  ) {
    rlang::abort(
      "`scheme` parameter should be of `chewie_scheme` or `data.frame` type"
    )
  }

  if (sum(!missing(page), !missing(url)) != 1) {
    rlang::abort("Must supply exactly one of `page` or `url`")
  }

  if (!missing(url)) {
    page <- httr::GET(url, ...) |>
      httr::content(as = "text") |>
      rvest::read_html()
  }

  check_html_document(page)

  if (inherits(scheme, "data.frame")) {
    scheme <- as_scheme(scheme)
  }

  purrr::map(scheme, ~ execute_instruction(page, .x))
}
