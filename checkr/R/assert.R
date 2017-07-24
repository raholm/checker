#' Asserts that the input is string.
#'
#' @param input Input that is checked for being a string
#'
#' @export
assert_string <- function(input, null.ok=FALSE) {
    call <- match.call()

    if (is.null(input) & null.ok) return()

    if (!(is.character(input) & length(input) == 1)) {
        stop(.msg("Input is not a string.", call))
    }
}

#' Assets that the input is character.
#'
#' @param input Input that is checked for being a character
#' @param null.ok If true, will not assert null value
#'
#' @export
assert_character <- function(input, null.ok=FALSE) {
    call <- match.call()

    if (!null.ok & is.null(input)) {
        stop(.msg("Input is null.", call))
    }

    if (!is.character(input) & !is.null(input)) {
        stop(.msg("Input is not a character.", call))
    }
}

#' Asserts that input is integer
#'
#' @param input Input that is checked for being an integer
#' @param lower Lower bound
#' @param upper Upper bound
#'
#' @export
assert_integer <- function(input, lower=-Inf, upper=Inf, null.ok=FALSE) {
    call <- match.call()

    if (is.null(input) & null.ok) return()

    if (!(length(input) == 1)) {
        stop(.msg("Input is not an integer."), call)
    }

    if (is.numeric(input) & !(input %% 1 == 0 )) {
        stop(.msg("Input is not an integer."), call)
    }

    lower <- as.numeric(lower)
    upper <- as.numeric(upper)

    if (!is.null(lower)) {
        if (input < lower) {
            stop(.msg(paste("Input is less than ", lower, ".", sep=""), call))
        }
    }

    if (!is.null(upper)) {
        if (input > upper) {
            stop(.msg(paste("Input is greater than ", upper, ".", sep=""), call))
        }
    }
}

#' Asserts that input is factor
#'
#' @param input Input that is being checked for being a factor
#'
#' @export
assert_factor <- function(input, null.ok=FALSE) {
    call <- match.call()

    if (is.null(input) & null.ok) return()

    if (!is.factor(input)) {
        stop(.msg("Input is not a factor.", call))
    }
}

#' Asserts that input is logical
#'
#' @param inpit Input that is being checked for being a logical
#'
#' @export
assert_logical <- function(input, null.ok=FALSE) {
    call <- match.call()

    if (is.null(input) & null.ok) return()

    if (!is.logical(input)) {
        stop(.msg("Input is not a logical.", call))
    }
}

#' Asserts that input contains pattern
#'
#' @param input Input that is being checked for pattern
#' @param pattern Pattern to check for
#'
#' @export
assert_regexp <- function(input, pattern) {
    call <- match.call()

    assert_string(input)
    assert_string(pattern)

    if (!grepl(pattern, input)) {
        stop(.msg(paste("Input do not contain the pattern ", pattern, ".", sep=""), call))
    }
}

#' Asserts that input is of specified type
#'
#' @param input Input that is being checked for type
#' @param type Type to check for
#'
#' @export
assert_type <- function(input, type) {
    call <- match.call()

    if (type == "tbl_df") {
        if (!dplyr::is.tbl(input)) {
            stop(.msg("Input is not a tbl_df.", call))
        }
    } else if (typeof(input) != type) {
        stop(.msg(paste("Input is not of type ", type, ".", sep=""), call))
    }
}

#' Asserts that input is subset of a set
#'
#' @param input The input to check for being a subset
#' @param set The full set
#' @param null.ok If set to TRUE, then null subset is fine
#'
#' @export
assert_subset <- function(input, set, null.ok=FALSE) {
    call <- match.call()

    if (!null.ok & is.null(input)) {
        stop(.msg("Input is null.", call))
    }

    for (i in seq_along(input)) {
        if (!(input[i] %in% set)) {
            stop(.msg(paste(input[i], " is not in present in the set.", sep=""), call))
        }
    }
}

#' Asserts that the input is an existing file.
#'
#' @param input Input that is checked for being an existing file.
#'
#' @export
assert_file_exists <- function(input) {
    call <- match.call()

    assert_string(input)

    if (!file.exists(input)) {
        stop(.msg(paste("File", input, "does not exist.", sep=" "), call))
    }
}

#' Asserts that the input is of specified filetype
#'
#' @param input Input that is checked for being specified filetype.
#' @param type Filetype to check for
#'
#' @export
assert_filetype <- function(input, type) {
    call <- match.call()

    assert_file_exists(input)
    assert_regexp(type, "(\\.)\\w+")

    if (!endsWith(input, type)) {
        stop(.msg(paste("File ", input, " does not end with ", type, ".", sep=""), call))
    }
}
