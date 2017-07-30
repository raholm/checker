.msg <- function(msg, call) {
    paste(deparse(call), ":", msg, sep=" ")
}

.check_null <- function(input, null_ok, call) {
    if (!null_ok & is_null(input))
        stop(.msg("Input is null."), call)
}

.check_type <- function(func, input, type, call) {
    if (!func(input))
        stop(.msg(paste0("Input is not a ", type, "."), call))
}

.check_cond <- function(cond, condition, call) {
    if (!cond)
        stop(.msg(paste0("Input does not satisfy ", condition, " condition.", call)))
}

.check_len <- function(len, input, call) {
    if (!(length(input) == len))
        stop(.msg("Input does not satisfy the length constraint.", call))
}

.check_lower_bound <- function(lower, input, call) {
    if (!(all(input >= lower)))
        stop(.msg("Input does not satisfy the lower bound constraint.", call))
}

.check_upper_bound <- function(upper, input, call) {
    if (!(all(input <= upper)))
        stop(.msg("Input does not satisfy the upper bound constraint.", call))
}

.check_tblcol_subset <- function(subset, set, call) {
    if (!is_null(subset) & !(is_subset(subset, set)))
        stop(.msg("Input does not contain column constraint.", call))
}

.check_subset <- function(subset, set, call) {
    if (!is_subset(subset, set))
        stop(.msg("Input is not a subset.", call))
}

.check_choice <- function(subset, set, call) {
    if (!length(subset) == 1)
        stop(.msg("Input is not a single choice.", call))
    if (!is_subset(subset, set))
        stop(.msg("Input is not a valid choice.", call))
}

.check_file_exists <- function(input, call) {
    if (!file_exists(input))
        stop(.msg("Input is not an existing file.", call))
}

.check_file_type <- function(input, type, call) {
    .check_file_exists(input)
    if (!is_file_type(input, type))
        stop(.msg("Input does not satisfy file type constraint.", call))
}
