#' Asserts that the input is string.
#'
#' @param input Input that is checked for being a string
#' @param null_ok If \code{TRUE}, will not assert null value
#'
#' @export
assert_string <- function(input, null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    .check_type(is_string, input, "string", call)
}

#' Assets that the input is character.
#'
#' @param input Input that is checked for being a character
#' @param len The assumed length of the input
#' @param null_ok If \code{TRUE}, will not assert null value
#'
#' @export
assert_character <- function(input, len=length(input), null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    .check_type(is_character, input, "character", call)
    .check_len(len, input, call)
}

#' Asserts that input is integer
#'
#' @param input Input that is checked for being an integer
#' @param len The assumed length of the input
#' @param lower Lower bound
#' @param upper Upper bound
#' @param null_ok If \CODE{TRUE}, will not assert null value
#'
#' @export
assert_integer <- function(input, len=length(input),
                           lower=-Inf, upper=Inf, null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    .check_type(is_integer, input, "integer", call)
    .check_len(len, input, call)
    .check_lower_bound(min(lower, upper), input, call)
    .check_upper_bound(max(lower, upper), input, call)
}

#' Asserts that input is factor
#'
#' @param input Input that is being checked for being a factor
#' @param len The assumed length of the input
#' @param null_ok If \code{TRUE}, will not assert null value
#'
#' @export
assert_factor <- function(input, len=length(input), null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    .check_type(is_factor, input, "factor", call)
    .check_len(len, input)
}

#' Asserts that input is logical
#'
#' @param input Input that is being checked for being a logical
#' @param len The assumed length of the input
#' @param null_ok If \code{TRUE}, will not assert null value
#'
#' @export
assert_logical <- function(input, len=length(input), null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    .check_type(is_logical, input, "logical", call)
    .check_len(len, input, call)
}

#' Asserts that input is a \code{\link[tibble]{data_frame}}
#'
#' @param input The input to check
#' @param cols Character vector of expected columns. Defaults to NULL, i.e., no preferenses.
#'
#' @export
assert_tidy_table <- function(input, cols=NULL) {
    call <- match.call()
    .check_type(is_tidy_table, input, "tidy table", call)
    .check_tblcol_subset(cols, names(input), call)
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
    .check_cond(contains_regexp(input, pattern), "regexp", call)
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
#' @param null_ok If set to \CODE{TRUE}, then null subset is fine
#'
#' @export
assert_subset <- function(input, set, null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    .check_subset(input, set, call)
}

#' Asserts that input is a single element of a set
#'
#' @param input The input to check
#' @param set The full set
#' @param null_ok If set to \CODE{TRUE}, then null element is fine
#'
#' @export
assert_choice <- function(input, set, null_ok=FALSE) {
    if (is_null(input) & null_ok) return()
    call <- match.call()
    .check_choice(input, set, call)
}

#' Asserts that the input is an existing file.
#'
#' @param input Input that is checked for being an existing file.
#'
#' @export
assert_file_exists <- function(input) {
    call <- match.call()
    .check_file_exists(input, call)
}

#' Asserts that the input is of specified file type
#'
#' @param input Input that is checked for being specified filetype.
#' @param type File type to check for
#'
#' @export
assert_file_type <- function(input, type) {
    call <- match.call()
    .check_file_type(input, type, call)
}
