#' Checks if input is null
#'
#' @param input The input to check
#'
#' @export
is_null <- function(input) {
    is.null(input)
}

#' Checks if input is a single string
#'
#' @param input The input to check
#'
#' @export
is_string <- function(input) {
    is_character(input) & length(input) == 1
}

#' Checks if input is character
#'
#' @param input The input to check
#' @param len The assumed length of input
#'
#' @export
is_character <- function(input, len=length(input)) {
    !is_null(input) & is.character(input) & length(input) == len
}

#' Checks if input is logical
#'
#' @param input The input to check
#' @param len The assumed length of input
#'
#' @export
is_logical <- function(input, len=length(input)) {
    !is_null(input) & is.logical(input) & length(input) == len
}

#' Checks if input is factor
#'
#' @param input The input to check
#' @param len The assumed length of input
#'
#' @export
is_factor <- function(input, len=length(input)) {
    !is_null(input) & is.factor(input) & length(input) == len
}

#' Checks if input is numeric
#'
#' @param input The input to check
#' @param len The assumed length of input
#'
#' @export
is_numeric <- function(input, len=length(input), lower=-Inf, upper=Inf) {
    if (is_null(input) | !is.numeric(input) | length(input) != len) return(FALSE)
    all(input >= min(lower, upper)) & all(input <= max(lower, upper))
}

#' Checks if input is numeric
#'
#' @param input The input to check
#' @param len The assumed length of input
#' @param lower Lower bound
#' @param upper Upper bound
#'
#' @export
is_integer <- function(input, len=length(input), lower=-Inf, upper=Inf) {
    if (!is_numeric(input, lower=lower, upper=upper) | length(input) != len) return(FALSE)
    all(input %% 1 == 0)
}

#' Checks if input is  a subset of the set
#'
#' @param input The input to check
#' @param set The full set
#'
#' @export
is_subset <- function(input, set) {
    if (length(input) <= 1) return(any(input == set))
    for (i in seq_along(input)) {
        if (!any(input[i] == set)) return(FALSE)
    }

    TRUE
}

#' Checks if input is a \code{\link[tibble]{tibble}}
#'
#' @param input The input to check
#'
#' @export
is_tidy_table <- function(input) {
    tibble::is_tibble(input) & dplyr::is.tbl(input)
}

#' Checks if input is a \code{\link[tibble]{tibble}}
#' having columns 'doc', 'type', 'topic'
#'
#' @param The input to check
#'
#' @export
is_tidy_topic_state <- function(state) {
    is_tidy_table(state) &
        is_subset(c("doc", "type", "topic"), names(state))
}

#' Checks if input is an existing file
#'
#' @param input The input to check
#'
#' @export
file_exists <- function(input) {
    if (!is_string(input)) return(FALSE)
    file.exists(input)
}

#' Checks if input is of filetype \code{type}
#'
#' @param input The input to check
#' @param type The filetype
#'
#' @export
is_file_type <- function(input, type) {
    if (!is_string(type)) return(FALSE)
    if (!startsWith(type, "."))
        type <- paste0(".", type, collapse="")

    if (!file_exists(input)) return(FALSE)
    contain_regexp(type, "(\\.)\\w+") & endsWith(input, type)
}

#' Checks if input contains regexp \code{pattern}
#'
#' @param input The input to check
#' @param pattern The pattern
#'
#' @export
contain_regexp <- function(input, pattern) {
    if (!is_string(input) | !is_string(pattern)) return(FALSE)
    grepl(pattern, input)
}
