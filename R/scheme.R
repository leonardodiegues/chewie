
# Scheme ------------------------------------------------------------------

scheme <- function(instructions) {
  structure(
    instructions,
    class = "chewie_scheme"
  )
}


#' @export
#' @rdname scheme_from_dataframe
scheme_from_dataframe <- function(x) {
  rows <- purrr::transpose(x)

  instructions <- purrr::map(rows, instruction_from_list)

  scheme(instructions)
}

#' Chew instructions from scheme
#'
#' Loads scheme and parses a page using the instructions contained by it.
#'
#' @param page a `xml_document` element
#' @param scheme the type of extractor to be utilized
#'
#' @keywords chew
#'
#' @export
chew <- function(page, scheme) {
  check_html_document(page)

  if (inherits(scheme, "data.frame")) {
    scheme <- scheme_from_dataframe(scheme)
  }

  purrr::map(scheme, ~ execute_instruction(page, .x))
}


# Instruction -------------------------------------------------------------

instruction <- function(title, path, pattern = NULL, type = NULL) {
  structure(
    list(
      title    = title,
      path     = path,
      pattern  = pattern,
      type     = type,
      result   = NULL
    ),
    class = "chewie_instruction"
  )
}

#' @export
#' @rdname scheme
is.instruction <- function(x) inherits(x, "chewie_instruction")

print.instruction <- function(x) {
  cat("<chewie_instruction>", "\n", sep = "")
  cat("    title: ", x$title, "\n", sep = "")
  cat("    path:  ", x$path, "\n", sep = "")
  cat("    type:  ", x$type, "\n", sep = "")
}

instruction_from_list <- function(l) {
  instruction(
    title = l$title,
    path = l$path,
    pattern = l$pattern,
    type = l$type
  )
}

execute_instruction <- function(page, instruction) {
  check_instruction(instruction)

  result <- page |>
    rvest::html_nodes(css = instruction$path) |>
    parse_field(
      output_type = instruction$type,
      pattern = instruction$pattern
    )

  instruction_set_result(instruction, result)
}

instruction_set_result <- function(x, result) {
  x$result <- result
  x
}

teste <- httr::GET("http://www.olympedia.org/editions/59/") |>
  httr::content(as = "text") |>
  rvest::read_html()


#' Extract texts from `xml_document` or `xml_node` element.
#'
#' Text parser using methods from both `stringr` and `rvest`. Automatically
#'   vectorised by both packages properties, can return a list of text or a
#'   single text
#'
#' @param field a `xml_node` or `xml_nodeset` element
#' @param output_type the type of extractor to be utilized
#' @param pattern a valid pattern pattern
#'
#' @keywords parser
#'
#' @export
parse_field <- function(field, output_type = NULL, pattern = NULL) {
  check_html_nodes(field)

    if (is.null(output_type)) {
    return(field)
    }

  valid_outputs <- c(
    "string",
    "numeric",
    "table",
    "date",
    "datetime",
    "timedelta",
    "price"
  )

  switch(output_type,
    string    = extract_text(field, pattern)     ,
    numeric   = extract_number(field, pattern)   ,
    table     = extract_table(field, pattern)    ,
    date      = extract_date(field, pattern)     ,
    datetime  = extract_datetime(field, pattern) ,
    timedelta = extract_timedelta(field, pattern),
    price     = extract_price(field, pattern),
    rlang::abort(sprintf("Invalid `output_type` value: '%s'", output_type))
  )
}


# Helpers -----------------------------------------------------------------
check_html_document <- function(x) {
  if (!inherits(x, "xml_document")) {
    rlang::abort("`x` must be produced by `rvest::read_html()`")
  }
}

check_html_nodes <- function(x) {
  if (sum(!inherits(x, "xml_node") | !inherits(x, "xml_nodeset")) == 0) {
    rlang::abort("`x` must be produced by `rvest::html_node()` or `rvest::html_nodes()`")
  }
}

check_scheme <- function(x) {
  if (!inherits(x, "chewie_scheme")) {
    rlang::abort("`x` must be produced by `scheme()`")
  }
}

check_instruction <- function(x) {
  if (!inherits(x, "chewie_instruction")) {
    rlang::abort("`x` must be produced by `instruction()`")
  }
}
