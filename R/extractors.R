#' Extract texts from `xml_document` or `xml_node` element.
#'
#' Text parser using methods from both `stringr` and `rvest`. Automatically
#'   vectorised by both packages properties, can return a list of text or a
#'   single text
#'
#' @param field a `xml_document` or `xml_node` element
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
#' @param field a `xml_document` or `xml_node` object
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
#' @param field a `xml_node` element
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

# extract_datetime <- function(field, pattern = NULL, ...) {
#   txt <- extract_text(field, pattern)
#   lubridate::as_datetime(txt, ...)
# }
#
# extract_table <- function(field, ...) {
#   rvest::html_table(field, ...)
# }
#
# extract_table(response, )
#
#
# response <- httr::GET("https://webscraper.io/test-sites/tables") %>%
#   httr::content(as = "text") %>%
#   rvest::read_html()
#
#
# response %>%
#   rvest::html_elements("table:nth-of-type(1)") %>%
#   extract_table()

