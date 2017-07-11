#' Asserts that the input is string.
#'
#' @param text Input that is checked for being a string.
assert_string <- function(text) {
    if (!(is.character(text) & length(text) == 1)) {
        stop(paste("Input", text, "is not a string.", sep=" "))
    }
}

#' Assets that the input is character.
#'
#' @param character Input that is checked for being a string
#' @param null.ok If true, will not assert null value
assert_character <- function(character, null.ok=FALSE) {
    if (!null.ok & is.null(character)) {
        stop("Input character is null.")
    }

    if (!is.character(character) & !is.null(character)) {
        stop(paste("Input", character, "is not a character.", sep=" "))
    }
}

#' Asserts that input is integer
#'
#' @param number Input that is checked for being an integer.
#' @param lower Lower bound
#' @param upper Upper bound
assert_integer <- function(number, lower=-Inf, upper=Inf) {
    if (!(length(number) == 1)) {
        stop(paste("Input", number, "is not a integer.", sep=" "))
    }

    if (!(number %% 1 == 0 )) {
        stop(paste("Input", number, "is not a integer.", sep=" "))
    }

    lower <- as.numeric(lower)
    upper <- as.numeric(upper)

    if (!is.null(lower)) {
        if (number < lower) {
            stop(paste("Input", number, "is less than.", lower, sep=" "))
        }
    }

    if (!is.null(upper)) {
        if (number > upper) {
            stop(paste("Input", number, "is greater than.", upper, sep=" "))
        }
    }
}

assert_regexp <- function(string, pattern) {
    assert_string(string)
    assert_string(pattern)

    if (!grepl(pattern, string)) {
        stop(paste(string, " do not contain the pattern ", pattern, ".", sep=""))
    }
}

#' Asserts that input is of specified type
assert_type <- function(object, type) {
    if (type == "tbl_df" & !dplyr::is.tbl(object)) {
        stop("Object is not a tbl_df.")
    } else if (typeof(object) != type) {
        stop(paste("Object is not of type ", type, ".", sep=""))
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
            stop(paste(subset[i], " is not in ", set, ".", sep=""))
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

#' Asserts that the input is an existing json file
#'
#' @param filename Input that is chcked for being an existing json file.
assert_filetype <- function(filename, type) {
    assert_file_exists(filename)
    assert_regexp(type, "(\\.)\\w+")

    if (!endsWith(filename, type)) {
        stop(paste("File ", filename, " does not end with ", type, ".", sep=""))
    }
}
