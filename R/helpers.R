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
