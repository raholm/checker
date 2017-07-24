.msg <- function(msg, call) {
    paste(deparse(call), ":", msg, sep=" ")
}
