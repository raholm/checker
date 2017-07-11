#' Asserts that the input is string.
#'
#' @param input Input that is checked for being a string
assert_string <- function(input) {
    if (!(is.character(input) & length(input) == 1)) {
        stop(paste("Input is not a string.", sep=" "))
    }
}

#' Assets that the input is character.
#'
#' @param input Input that is checked for being a character
#' @param null.ok If true, will not assert null value
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
#' @param string Input text that is being checked for pattern
#' @param pattern Pattern to check for
assert_regexp <- function(input, pattern) {
    assert_string(input)
    assert_string(pattern)

    if (!grepl(pattern, input)) {
        stop(paste("Input do not contain the pattern ", pattern, ".", sep=""))
    }
}

#' Asserts that input is of specified type
assert_type <- function(input, type) {
    if (type == "tbl_df" & !dplyr::is.tbl(input)) {
        stop("Input is not a tbl_df.")
    } else if (typeof(input) != type) {
        stop(paste("Input is not of type ", type, ".", sep=""))
    }
}

#' Asserts that input is subset of a set
#'
#' @param set The full set
#' @param subset The subset
#' @param null.ok If set to TRUE, then null subset is fine
assert_subset <- function(set, subset, null.ok=FALSE) {
    if (!null.ok & is.null(subset)) {
        stop("Input subset is null.")
    }

    for (i in seq_along(subset)) {
        if (!(subset[i] %in% set)) {
            stop(paste(subset[i], " is not in present in the set.", sep=""))
        }
    }
}

#' Asserts that the input is an existing file.
#'
#' @param filename Input that is checked for being an existing file.
assert_file_exists <- function(filename) {
    assert_string(filename)

    if (!file.exists(filename)) {
        stop(paste("File", filename, "does not exist.", sep=" "))
    }
}

#' Asserts that the input is of specified filetype
#'
#' @param filename Input that is checked for being specified filetype.
#' @param type Filetype to check for
assert_filetype <- function(filename, type) {
    assert_file_exists(filename)
    assert_regexp(type, "(\\.)\\w+")

    if (!endsWith(filename, type)) {
        stop(paste("File ", filename, " does not end with ", type, ".", sep=""))
    }
}
