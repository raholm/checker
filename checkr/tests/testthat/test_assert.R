test_that("assert_string raises an error for invalid input", {
    expect_error(assert_string(123))
    expect_error(assert_string(list(a="test string")))
    expect_error(assert_string(c("a", "b", "c")))
    expect_error(assert_string(NULL, null_ok=FALSE))
})

test_that("assert_string does not raise an error for valid input", {
    expect_message(assert_string("test string"), NA)
    expect_message(assert_string(NULL, null_ok=TRUE), NA)
})

test_that("assert_character raises an error for invalid input", {
    expect_error(assert_character(123))
    expect_error(assert_character(list(a="test_string")))
    expect_error(assert_character(NULL, null_ok=FALSE))
    expect_error(assert_character("a", len=2))
})

test_that("assert_character does not raise an error for valid input", {
    expect_message(assert_character("test string"), NA)
    expect_message(assert_character(c("a", "b", "c")), NA)
    expect_message(assert_character(NULL, null_ok=TRUE), NA)
})

test_that("assert_integer raises an error for invalid input", {
    expect_error(assert_integer(list(a=123)))
    expect_error(assert_integer("test string"))
    expect_error(assert_integer(1.5))
    expect_error(assert_integer(1, lower=2))
    expect_error(assert_integer(1, upper=0))
    expect_error(assert_integer(1, len=2))
    expect_error(assert_integer(NULL, null_ok=FALSE))
})

test_that("assert_integer does not raise an error for valid input", {
    expect_message(assert_integer(c(1, 2, 3)), NA)
    expect_message(assert_integer(123), NA)
    expect_message(assert_integer(-123), NA)
    expect_message(assert_integer(0), NA)
    expect_message(assert_integer(0, lower=0, upper=1), NA)
    expect_message(assert_integer(1, lower=0, upper=1), NA)
    expect_message(assert_integer(NULL, null_ok=TRUE), NA)
})

test_that("assert_numeric raises an error for invalid input", {
    expect_error(assert_numeric(list(a=123)))
    expect_error(assert_numeric("test string"))
    expect_error(assert_numeric(1, lower=2))
    expect_error(assert_numeric(1, upper=0))
    expect_error(assert_numeric(1, len=2))
    expect_error(assert_numeric(NULL, null_ok=FALSE))
})

test_that("assert_numeric does not raise an error for valid input", {
    expect_message(assert_numeric(c(-1.1, 2.253, 3.14)), NA)
    expect_message(assert_numeric(123.123), NA)
    expect_message(assert_numeric(-123.123), NA)
    expect_message(assert_numeric(0), NA)
    expect_message(assert_numeric(0.5, lower=0.5, upper=1), NA)
    expect_message(assert_numeric(0.5, lower=0, upper=.5), NA)
    expect_message(assert_numeric(NULL, null_ok=TRUE), NA)
})


test_that("assert_logical raises an error for invalid input", {
    expect_error(assert_logical(1))
    expect_error(assert_logical(0))
    expect_error(assert_logical("1"))
    expect_error(assert_logical("TRUE"))
    expect_error(assert_logical("FALSE"))
    expect_error(assert_logical(NULL, null_ok=FALSE))
    expect_error(assert_logical(TRUE, len=2))
})

test_that("assert_logical does not raise an error for valid input", {
    expect_message(assert_logical(TRUE), NA)
    expect_message(assert_logical(FALSE), NA)
    expect_message(assert_logical(NULL, null_ok=TRUE), NA)
})

test_that("assert_factor raises an error for invalid input", {
    expect_error(assert_factor(123))
    expect_error(assert_factor("123"))
    expect_error(assert_factor(list(a="test_string")))
    expect_error(assert_factor(NULL, null_ok=FALSE))
    expect_error(assert_factor(as.factor(123), len=2))
})

test_that("assert_factor does not raise an error for valid input", {
    expect_message(assert_factor(as.factor(c("a", "b", "c"))), NA)
    expect_message(assert_factor(NULL, null_ok=TRUE), NA)
})

test_that("assert_tidy_table raises an error for invalid input", {
    expect_error(assert_tidy_table(data.frame()))
    expect_error(assert_tidy_table("hello"))
})

test_that("assert_tidy_table raises an error for missing columns", {
    expect_error(assert_tidy_table(dplyr::data_frame(b="b"), "a"))
    expect_error(assert_tidy_table(dplyr::data_frame(a="a"), c("a", "b")))
})

test_that("assert_tidy_table does not raise an error for valid input", {
    expect_message(assert_tidy_table(tibble::data_frame()), NA)
    expect_message(assert_tidy_table(dplyr::data_frame()), NA)
    expect_message(assert_tidy_table(dplyr::data_frame(a="a", b="b"), "a"), NA)
    expect_message(assert_tidy_table(dplyr::data_frame(a="a", b="b"), c("a", "b")), NA)
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
    expect_error(assert_subset(NULL, c("a", "b", "c"), null_ok=FALSE))
    expect_error(assert_subset(c("a", "d"), c("a", "b", "c")))
    expect_error(assert_subset(c(1, 4), c(1, 2, 3)))
})

test_that("assert_subset does not raise an error for valid input", {
    expect_message(assert_subset(NULL, c("a", "b", "c"), null_ok=TRUE), NA)
    expect_message(assert_subset(c("a", "c"), c("a", "b", "c")), NA)
    expect_message(assert_subset(c(1, 3), c(1, 2, 3)), NA)
})

test_that("assert_choice raises an error for invalid input", {
    expect_error(assert_choice(NULL, c("a", "b", "c"), null_ok=FALSE))
    expect_error(assert_choice(c("a", "b"), c("a", "b", "c")))
    expect_error(assert_choice("a", c("b", "c")))
})

test_that("assert_choice does not raise an error for valid input", {
    expect_message(assert_choice(NULL, c("a", "b", "c"), null_ok=TRUE), NA)
    expect_message(assert_choice("a", c("a", "b", "c")), NA)
    expect_message(assert_choice(1, c(1, 2, 3)), NA)
})

test_that("assert_file_exists raises an error for invalid file", {
    expect_error(assert_file_exists("./files/empty_file.invalid"))
})

test_that("assert_file_exists does not an error for valid file", {
    expect_message(assert_file_exists("./files/empty_file.valid"), NA)
})

test_that("assert_file_type raises an error for incorrect filetype", {
    expect_message(assert_file_exists("./files/empty_file.valid"), NA)
    expect_error(assert_file_type("./files/empty_file.valid", ".json"))
})

test_that("assert_file_type does not raise an error for correct filetype", {
    expect_message(assert_file_type("./files/empty_file.json", ".json"), NA)
})
