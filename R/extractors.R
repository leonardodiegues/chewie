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
    "text",
    "numeric",
    "table",
    "date",
    "datetime",
    "timedelta",
    "price"
  )

  switch(as,
    text      = extract_text(field, pattern),
    numeric   = extract_number(field, pattern),
    table     = extract_table(field, pattern),
    date      = extract_date(field, pattern),
    datetime  = extract_datetime(field, pattern),
    timedelta = extract_timedelta(field, pattern),
    price     = extract_price(field, pattern),
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
#' @param field a `xml_node` or `xml_nodeset` element
#' @param pattern a valid RegEx pattern
#'
#' @export
extract_datetime <- function(field, pattern = NULL, ...) {
  txt <- extract_text(field, pattern)
  lubridate::as_datetime(txt, ...)
}

#' Extract table from `xml_node` elements.
#'
#' `extract_table` retrieves a table as `data.frame` from a parsed HTML page.
#'
#' Users should expect unstable results when `parse_url_cols` is TRUE due to
#'   uncovered cases. This function is an ongoing process and shall be modified
#'   in the future.
#'
#' @param field a `xml_node` or `xml_nodeset` element
#' @param parse_url_cols whether table should include `href` attributes from
#'   columns that have `<a>` tags attached to them, defaults to FALSE.
#'
#' @return a `data.frame` containing the extracted table.

#' @export
extract_table <- function(field, parse_url_cols = FALSE, ...) {
  tbl <- rvest::html_table(field, ...)

  if (!inherits(tbl, "data.frame")) {
    if (length(tbl) > 1) {
      rlang::abort("`field` parameter should aim one table at a time.")
    } else {
      tbl <- tbl[[1]]
    }
  }

  if (parse_url_cols) {
    url_cols <- detect_url_cols(field)

    rows_ <- rvest::html_elements(field, "tr")

    if (first_row_is_header(field)) {
      rows_ <- rows_[2:length(rows_)]
    }

    new_cols <- list()
    new_cols <- lapply(seq_along(url_cols), \(i) new_cols[[i]] <- list())
    names(new_cols) <- lapply(url_cols, \(i) paste0(colnames(tbl)[i], "_url"))

    for (row_ in rows_) {
      cols_ <- rvest::html_elements(row_, "td")

      for (i in seq_along(url_cols)) {
        col_ <- cols_[url_cols[i]]

        url_ <- col_ |>
          rvest::html_element("a") |>
          rvest::html_attr("href")

        new_cols[[i]] <- append(new_cols[[i]], url_)
      }
    }

    new_cols <- data.frame(lapply(new_cols, function(x) Reduce(c, x)))

    tbl <- dplyr::bind_cols(tbl, new_cols)
  }

  tbl
}

#' @export
detect_url_cols <- function(field) {
  cols_ <- c()

  all_rows <- rvest::html_elements(field, "tr")

  if (first_row_is_header(field)) {
    all_rows <- all_rows[2:length(all_rows)]
  }

  for (row_ in all_rows) {
    row_cols <- rvest::html_elements(row_, "td")

    for (i in seq_along(row_cols)) {
      col_ <- row_cols[i]

      if (i %in% cols_) next

      check_for_link <- rvest::html_element(col_, "a")

      if (!is.na(check_for_link)) {
        cols_ <- append(cols_, i)
      }
    }
  }

  sort(cols_)
}

first_row_is_header <- function(field) {
  !is.na(rvest::html_element(field, "tr > th"))
}
