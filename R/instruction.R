#' @export
#' @rdname instruction
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
#' @rdname instruction
is.instruction <- function(x) inherits(x, "chewie_instruction")

#' @export
#' @rdname instruction
print.instruction <- function(x) {
  cat("<chewie_instruction>", "\n", sep = "")
  cat("    Title: ", x$title, "\n", sep = "")
  cat("    Path:  ", x$path, "\n", sep = "")
  cat("    Type:  ", x$type, "\n", sep = "")
  cat("    Pattern:  ", x$pattern, "\n", sep = "")
  invisible(x)
}

#' @export
#' @rdname instruction
instruction_from_list <- function(l) {
  instruction(
    title = l$title,
    path = l$path,
    pattern = l$pattern,
    type = l$type
  )
}

#' @export
#' @rdname instruction
execute_instruction <- function(page, instruction) {
  check_instruction(instruction)

  result <- page |>
    rvest::html_elements(css = instruction$path) |>
    parse_field(
      output_type = instruction$type,
      pattern = instruction$pattern
    )

  instruction_set_result(instruction, result)
}

#' @export
#' @rdname instruction
instruction_set_result <- function(x, result) {
  x$result <- result
  x
}
