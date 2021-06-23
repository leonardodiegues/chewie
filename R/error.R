error_handler <- function(error_type = NULL) {
  switch(error_type,
    missing_required_param = stop("", call. = FALSE),
    unexistent_param = stop("", call. = FALSE),
    missing_parameter = stop("", call. = FALSE),
    missing_parameter = stop("", call. = FALSE),
  )
}


missing_required_param_error <- function() {
  print(ls())
}


