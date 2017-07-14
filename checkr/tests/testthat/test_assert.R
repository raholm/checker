test_that("assert_is_string raises an error for invalid input", {
    expect_error(assert_string(123))
    expect_error(assert_string(list(a="test string")))
    expect_error(assert_string(c("a", "b", "c")))
    expect_error(assert_string(NULL, null.ok=FALSE))
})

test_that("assert_is_string does not raise an error for valid input", {
    expect_message(assert_string("test string"), NA)
    expect_message(assert_string(NULL, null.ok=TRUE), NA)
})

test_that("assert_character raises an error for invalid input", {
    expect_error(assert_character(123))
    expect_error(assert_character(list(a="test_string")))
    expect_error(assert_character(NULL, null.ok=FALSE))
})

test_that("assert_character does not raise an error for valid input", {
    expect_message(assert_character("test string"), NA)
    expect_message(assert_character(c("a", "b", "c")), NA)
    expect_message(assert_character(NULL, null.ok=TRUE), NA)
})

test_that("assert_integer raises an error for invalid input", {
    expect_error(assert_integer(c(1, 2, 3)))
    expect_error(assert_integer(list(a=123)))
    expect_error(assert_integer("test string"))
    expect_error(assert_integer(1.5))
    expect_error(assert_integer(1, lower=2))
    expect_error(assert_integer(1, upper=0))
    expect_error(assert_integer(NULL, null.ok=FALSE))
})

test_that("assert_integer does not raise an error for valid input", {
    expect_message(assert_integer(123), NA)
    expect_message(assert_integer(-123), NA)
    expect_message(assert_integer(0), NA)
    expect_message(assert_integer(0, lower=0, upper=1), NA)
    expect_message(assert_integer(1, lower=0, upper=1), NA)
    expect_message(assert_integer(NULL, null.ok=TRUE), NA)
})

test_that("assert_factor raises an error for invalid input", {
    expect_error(assert_factor(123))
    expect_error(assert_factor("123"))
    expect_error(assert_factor(list(a="test_string")))
    expect_error(assert_factor(NULL, null.ok=FALSE))
})

test_that("assert_factor does not raise an error for valid input", {
    expect_message(assert_factor(as.factor(c("a", "b", "c"))), NA)
    expect_message(assert_factor(NULL, null.ok=TRUE), NA)
})


test_that("assert_regexp raises an error for invalid input", {
    expect_error(assert_regexp("R", "\\."))
    expect_error(assert_regexp("abc", "(abd)"))
})

test_that("assert_regexp does not raise an error for valid input", {
    expect_message(assert_regexp(".json", "\\."), NA)
    expect_message(assert_regexp(".json", "(\\.)\\w+"), NA)
    expect_message(assert_regexp("abc", "(abc)"), NA)
})


test_that("assert_subset raises an error for invalid input", {
    expect_error(assert_subset(NULL, c("a", "b", "c"), null.ok=FALSE))
    expect_error(assert_subset(c("a", "d"), c("a", "b", "c")))
    expect_error(assert_subset(c(1, 4), c(1, 2, 3)))
})

test_that("assert_subset does not raise an error for valid input", {
    expect_message(assert_subset(NULL, c("a", "b", "c"), null.ok=TRUE), NA)
    expect_message(assert_subset(c("a", "c"), c("a", "b", "c")), NA)
    expect_message(assert_subset(c(1, 3), c(1, 2, 3)), NA)
})

test_that("assert_file_exists raises an error for invalid file", {
    expect_error(assert_file_exists("./files/empty_file.invalid"))
})

test_that("assert_file_exists does not an error for valid file", {
    expect_message(assert_file_exists("./files/empty_file.valid"), NA)
})

test_that("assert_filetype raises an error for incorrect filetype", {
    expect_message(assert_file_exists("./files/empty_file.valid"), NA)
    expect_error(assert_filetype("./files/empty_file.valid", ".json"))
})

test_that("assert_filetype does not raise an error for correct filetype", {
    expect_message(assert_filetype("./files/empty_file.json", ".json"), NA)
})
