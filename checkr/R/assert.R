#' Asserts that the input is string.
#'
#' @param input Input that is checked for being a string
#'
#' @export
assert_string <- function(input, null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    if (!is_string(input))
        stop(.msg("Input is not a string.", call))
}

#' Assets that the input is character.
#'
#' @param input Input that is checked for being a character
#' @param len The assumed length of the input
#' @param null_ok If true, will not assert null value
#'
#' @export
assert_character <- function(input, len=length(input), null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    if (!is_character(input, len))
        stop(.msg("Input is not a character.", call))
}

#' Asserts that input is integer
#'
#' @param input Input that is checked for being an integer
#' @param len The assumed length of the input
#' @param lower Lower bound
#' @param upper Upper bound
#'
#' @export
assert_integer <- function(input, len=length(input),
                           lower=-Inf, upper=Inf, null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    if (!is_integer(input, len, lower, upper))
        stop(.msg("Input is not integer within bounds.",))
}

#' Asserts that input is factor
#'
#' @param input Input that is being checked for being a factor
#'
#' @export
assert_factor <- function(input, len=length(input), null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    if (!is_factor(input, len))
        stop(.msg("Input is not a factor.", call))
}

#' Asserts that input is logical
#'
#' @param input Input that is being checked for being a logical
#'
#' @export
assert_logical <- function(input, len=length(input), null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    if (!is_logical(input, len))
        stop(.msg("Input is not a logical.", call))
}

#' Asserts that input contains pattern
#'
#' @param input Input that is being checked for pattern
#' @param pattern Pattern to check for
#'
#' @export
assert_regexp <- function(input, pattern) {
    assert_string(input)
    assert_string(pattern)
    call <- match.call()
    if (!contain_regexp(input, pattern))
        stop(.msg(paste("Input do not contain the pattern ", pattern, ".", sep=""), call))
}

#' Asserts that input is of specified type
#'
#' @param input Input that is being checked for type
#' @param type Type to check for
#'
#' @export
assert_type <- function(input, type) {
    call <- match.call()
    assert_string(type)

    if (type == "tbl_df") {
        if (!is_tidy_table(input))
            stop(.msg("Input is not a tbl_df.", call))
    } else if (typeof(input) != type) {
        stop(.msg(paste("Input is not of type ", type, ".", sep=""), call))
    }
}

#' Asserts that input is subset of a set
#'
#' @param input The input to check for being a subset
#' @param set The full set
#' @param null_ok If set to TRUE, then null subset is fine
#'
#' @export
assert_subset <- function(input, set, null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    if (!is_subset(input, set))
        stop(.msg("Input is not a subset.", call))
}

#' Asserts that input is a single element of a set
#'
#' @param input The input to check
#' @param set The full set
#' @param null_ok If set to TRUE, then null element is fine
#'
#' @export
assert_choice <- function(input, set, null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    if (!(length(input) == 1) | (length(input) == 1 & !(is_subset(input, set))))
        stop(.msg("Input is not a present in the set of choices"))
}

#' Asserts that the input is an existing file.
#'
#' @param input Input that is checked for being an existing file.
#'
#' @export
assert_file_exists <- function(input) {
    call <- match.call()
    if (!file_exists(input))
        stop(.msg(paste("File", input, "does not exist.", sep=" "), call))
}

#' Asserts that the input is of specified filetype
#'
#' @param input Input that is checked for being specified filetype.
#' @param type Filetype to check for
#'
#' @export
assert_filetype <- function(input, type) {
    call <- match.call()
    if (!is_file_type(input, type))
        stop(.msg(paste("File ", input, " does not end with ", type, ".", sep=""), call))
}
