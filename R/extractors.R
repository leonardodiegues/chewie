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
parse_field <- function(field, as = NULL, pattern = NULL) {
  check_html_nodes(field)

  if (is.null(as)) {
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

  switch(as,
    string    = extract_text(field, pattern)     ,
    numeric   = extract_number(field, pattern)   ,
    table     = extract_table(field, pattern)    ,
    date      = extract_date(field, pattern)     ,
    datetime  = extract_datetime(field, pattern) ,
    timedelta = extract_timedelta(field, pattern),
    price     = extract_price(field, pattern)    ,
    rlang::abort(sprintf("Invalid `as` value: '%s'", as))
  )
}

#' Extract texts from `xml_document` or `xml_node` element.
#'
#' Text parser using methods from both `stringr` and `rvest`. Automatically
#'   vectorised by both packages properties, can return a list of text or a
#'   single text
#'
#' @param field a `xml_node` or `xml_nodeset` element
#' @param pattern a valid RegEx pattern
#'
#' @family extractors
#'
#' @keywords extractor
#'
#' @export
extract_text <- function(field, pattern = NULL) {
  txt <- rvest::html_text2(field)

  if (!is.null(pattern)) {
    txt <- stringr::str_extract(txt, pattern)
  }

  txt
}

#' Extract texts from `xml_node` object.
#'
#' Wrapper around `extract_text`. Converts extracted texts to numeric.
#'
#' @param field a `xml_node` or `xml_nodeset` element
#' @param pattern a valid RegEx pattern
#'
#' @return
#'
#' @family extractors
#'
#' @keywords extractor
#'
#' @export
extract_numeric <- function(field, pattern = NULL) {
  txt <- extract_text(field, pattern)
  as.numeric(txt)
}

#' Extract prices from `xml_node` elements.
#'
#' Wrapper around `extract_text`. Extracts digits from a string, convert them
#'   into a single integer and divide it by 100, considering that prices won't have
#'   more than 2 decimal digits.
#'
#' @param field a `xml_node` or `xml_nodeset` element
#' @param pattern a valid RegEx pattern
#'
#' @family extractors
#'
#' @keywords extractor
#'
#' @export
extract_price <- function(field, pattern = NULL) {
  txt <- extract_text(field, pattern)

  price <- txt %>%
    stringr::str_replace_all("\\D", "") %>%
    as.numeric() %>%
    magrittr::divide_by(100)

  price
}

#' Extract datetime from `xml_node` elements.
#'
#' @param field
#' @param pattern
#' @export
extract_datetime <- function(field, pattern = NULL, ...) {
  txt <- extract_text(field, pattern)
  lubridate::as_datetime(txt, ...)
}

#' Extract table from `xml_node` elements.
#'
#' @param field
#' @param pattern
#' @export
extract_table <- function(field, ...) {
  tbl <- rvest::html_table(field, ...)

  first_row <- field |>
    rvest::html_element(css = "tr") |>

}

