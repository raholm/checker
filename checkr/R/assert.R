#' Asserts that the input is string.
#'
#' @param input Input that is checked for being a string
#'
#' @export
assert_string <- function(input) {
    if (!(is.character(input) & length(input) == 1)) {
        stop(paste("Input is not a string.", sep=" "))
    }
}

#' Assets that the input is character.
#'
#' @param input Input that is checked for being a character
#' @param null.ok If true, will not assert null value
#'
#' @export
assert_character <- function(input, null.ok=FALSE) {
    if (!null.ok & is.null(input)) {
        stop("Input is null.")
    }

    if (!is.character(input) & !is.null(input)) {
        stop("Input is not a character.")
    }
}

#' Asserts that input is integer
#'
#' @param input Input that is checked for being an integer
#' @param lower Lower bound
#' @param upper Upper bound
#'
#' @export
assert_integer <- function(input, lower=-Inf, upper=Inf) {
    if (!(length(input) == 1)) {
        stop("Input is not an integer.")
    }

    if (!(input %% 1 == 0 )) {
        stop("Input is not an integer.")
    }

    lower <- as.numeric(lower)
    upper <- as.numeric(upper)

    if (!is.null(lower)) {
        if (input < lower) {
            stop(paste("Input is less than ", lower, ".", sep=""))
        }
    }

    if (!is.null(upper)) {
        if (input > upper) {
            stop(paste("Input is greater than ", upper, ".", sep=""))
        }
    }
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

    if (!grepl(pattern, input)) {
        stop(paste("Input do not contain the pattern ", pattern, ".", sep=""))
    }
}

#' Asserts that input is of specified type
#'
#' @param input Input that is being checked for type
#' @param type Type to check for
#'
#' @export
assert_type <- function(input, type) {
    if (type == "tbl_df" & !dplyr::is.tbl(input)) {
        stop("Input is not a tbl_df.")
    } else if (typeof(input) != type) {
        stop(paste("Input is not of type ", type, ".", sep=""))
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
    if (!null.ok & is.null(input)) {
        stop("Input is null.")
    }

    for (i in seq_along(input)) {
        if (!(input[i] %in% set)) {
            stop(paste(input[i], " is not in present in the set.", sep=""))
        }
    }
}

#' Asserts that the input is an existing file.
#'
#' @param input Input that is checked for being an existing file.
#'
#' @export
assert_file_exists <- function(input) {
    assert_string(input)

    if (!file.exists(input)) {
        stop(paste("File", input, "does not exist.", sep=" "))
    }
}

#' Asserts that the input is of specified filetype
#'
#' @param input Input that is checked for being specified filetype.
#' @param type Filetype to check for
#'
#' @export
assert_filetype <- function(input, type) {
    assert_file_exists(input)
    assert_regexp(type, "(\\.)\\w+")

    if (!endsWith(input, type)) {
        stop(paste("File ", input, " does not end with ", type, ".", sep=""))
    }
}
