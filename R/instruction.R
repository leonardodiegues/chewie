#' @export
#' @rdname instruction
instruction <- function(
  title, path, alternative_path = NULL, path_type = "css",
  parse_as = NULL, pattern = NULL
) {
  structure(
    list(
      title            = title,
      path             = path,
      alternative_path = alternative_path,
      path_type        = path_type,
      pattern          = pattern,
      parse_as         = parse_as,
      result           = NULL
    ),
    class = "chewie_instruction"
  )
}

#' @export
#' @rdname instruction
is.chewie_instruction <- function(x) inherits(x, "chewie_instruction")

#' @export
#' @rdname instruction
print.chewie_instruction <- function(x) {
  cat("<chewie_instruction>", "\n", sep = "")
  cat("    * title:     ", x$title, "\n", sep = "")
  cat("    * path:      ", x$path, "\n", sep = "")
  cat("    * path type: ", x$path_type, "\n", sep = "")
  cat("    * parse as:  ", x$parse_as, "\n", sep = "")
  cat("    * pattern:   ", x$pattern, "\n", sep = "")
  invisible(x)
}

#' @export
#' @rdname as_instruction
as_instruction <- function(x) {
  UseMethod("as_instruction")
}

#' @export
as_instruction.list <- function(x) {
  instruction(
    title = x$title,
    path = x$path,
    path_type = ifelse(is.null(x$path_type), "css", x$path_type),
    pattern = x$pattern,
    parse_as = x$parse_as
  )
}

#' @export
#' @rdname instruction
execute_instruction <- function(page, instruction) {
  check_instruction(instruction)

  result <- page |>
    find_elements(instruction) |>
    parse_field(as = instruction$parse_as, pattern = instruction$pattern)

  instruction_set_result(instruction, result)
}

#' @export
#' @rdname instruction
instruction_set_result <- function(x, result) {
  x$result <- result
  x
}

#' @export
#' @rdname instruction
find_elements <- function(page, instruction) {
  path_type <- instruction$path_type
  path <- instruction$path

  path_args <- list()
  path_args$x <- page
  path_args[path_type[1]] <- path

  results <- rlang::invoke(rvest::html_elements, path_args)

  if (is.null(results) | length(results) == 0) {
    path_args[path_type[1]] <- instruction$alternative_path

    results <- rlang::invoke(rvest::html_elements, path_args)
  }

  results
}
