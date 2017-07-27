test_that("is_null returns false for non-null input", {
    expect_false(is_null(1))
    expect_false(is_null("a"))
    expect_false(is_null(list()))
    expect_false(is_null(data.frame()))
})

test_that("is_null returns true for null input", {
    expect_true(is_null(NULL))
})

test_that("is_string returns false for non-string input", {
    expect_false(is_string(NULL))
    expect_false(is_string(1))
    expect_false(is_string(c("a", "b")))
    expect_false(is_string(list(a="hello world")))
    expect_false(is_string(data.frame(a="hello world")))
})

test_that("is_string returns true for string input", {
    expect_true(is_string("abc"))
})

test_that("is_character returns false for non-character input", {
    expect_false(is_character(NULL))
    expect_false(is_character(1))
    expect_false(is_character(c(1, 2)))
    expect_false(is_character(list(a="hello world")))
    expect_false(is_character(data.frame(a="hello world")))
})

test_that("is_character returns false for character input of incorrect length", {
    expect_false(is_character(c("a", "b", "c"), 2))
})

test_that("is_character returns true for character input", {
    expect_true(is_character("abc"))
    expect_true(is_character(c("abc", "b")))
    expect_true(is_character(c("abc", "b"), 2))
})

test_that("is_logical returns false for non-logical input", {
    expect_false(is_logical(NULL))
    expect_false(is_logical(1))
    expect_false(is_logical("a"))
    expect_false(is_logical(c(1, 2, 3)))
})

test_that("is_logical returns false for logical input of incorrect length", {
    expect_false(is_logical(c(TRUE, FALSE, TRUE), 2))
})

test_that("is_factor returns false for non-factor input", {
    expect_false(is_factor(NULL))
    expect_false(is_factor(1))
    expect_false(is_factor("a"))
    expect_false(is_factor(c(1, 2, 3)))
})

test_that("is_factor returns false for factor input of incorrect length", {
    expect_false(is_factor(as.factor(c(1, 1, 2)), 2))
})

test_that("is_factor returns true for factor input", {
    expect_true(is_factor(as.factor(1)))
    expect_true(is_factor(as.factor(c(1, 1, 2))))
    expect_true(is_factor(as.factor(c("a", "a", "b"))))
})

test_that("is_numeric returns false for non-numeric input", {
    expect_false(is_numeric(NULL))
    expect_false(is_numeric("a"))
    expect_false(is_numeric(c("a", "b")))
    expect_false(is_numeric(list(a="a")))
    expect_false(is_numeric(data.frame(a="b")))
})

test_that("is_numeric returns false for numeric input of incorrect length", {
    expect_false(is_numeric(c(1, 2, 3.14), 2))
})

test_that("is_numeric returns false for numeric input of incorrect bounds", {
    expect_false(is_numeric(1.01, upper=1))
    expect_false(is_numeric(-1.01, lower=-1))
    expect_false(is_numeric(c(-1.01, 1.01), lower=-1, upper=2))
    expect_false(is_numeric(c(-1.01, 1.01), lower=-2, upper=1))
})

test_that("is_numeric returns true for numeric input", {
    expect_true(is_numeric(c(-1.5, -1, 0, 1, 1.5)))
    expect_true(is_numeric(-1.5))
    expect_true(is_numeric(1.5))
})

test_that("is_numeric returns true for numeric input within the bounds", {
    expect_true(is_numeric(c(-1.5, -1, 0, 1, 1.5), lower=-1.5, upper=1.5))
    expect_true(is_numeric(c(-1.5, -1, 0, 1, 1.5), lower=-2, upper=2))
})


test_that("is_integer returns false for non-integer input", {
    expect_false(is_integer(NULL))
    expect_false(is_integer("a"))
    expect_false(is_integer(c("a", "b")))
    expect_false(is_integer(list()))
    expect_false(is_integer(data.frame()))
    expect_false(is_integer(1.5))
    expect_false(is_integer(-1.5))
})

test_that("is_integer returns false for integer input of incorrect length", {
    expect_false(is_integer(c(1, 2, 3), 2))
})

test_that("is_integer returns false for integer input of incorrect bounds", {
    expect_false(is_integer(2, upper=1))
    expect_false(is_integer(-2, lower=-1))
    expect_false(is_integer(c(-2, 1), lower=-1, upper=2))
    expect_false(is_integer(c(-1, 2), lower=-2, upper=1))
})

test_that("is_integer returns true for integer input", {
    expect_true(is_integer(c(-2, -1, 0, 1, 2)))
    expect_true(is_integer(-1))
    expect_true(is_integer(1))
})

test_that("is_integer returns true for integer input within the bounds", {
    expect_true(is_integer(c(-2, -1, 0, 1, 2), lower=-2, upper=2))
})

test_that("is_subset returns false for non-subset input", {
    expect_false(is_subset(NULL, c(1, 2, 3)))
    expect_false(is_subset("a", c(1, 2, 3)))
    expect_false(is_subset(1, c("a", "b", "c")))
    expect_false(is_subset(c("a", "d"), c("a", "b", "c")))
})

test_that("is_subset returns true for subset input", {
    expect_true(is_subset(1, c(1, 2, 3)))
    expect_true(is_subset("a", c("a", "b", "c")))
    expect_true(is_subset(c("a", "c"), c("a", "b", "c")))
})

test_that("is_tidy_table returns false for non-tibble table input", {
    expect_false(is_tidy_table(data.frame(a="hello world")))
    expect_false(is_tidy_table(list(a="hello world")))
    expect_false(is_tidy_table(c("hello world")))
    expect_false(is_tidy_table(tibble::lst(a="hello world")))
    expect_false(is_tidy_table(dplyr::lst(a="hello world")))
})

test_that("is_tidy_table returns true for tibble input", {
    expect_true(is_tidy_table(tibble::data_frame(a="hello world")))
    expect_true(is_tidy_table(dplyr::data_frame(a="hello world")))
})

test_that("is_tidy_topic_state returns false for non-tibble table input", {
    expect_false(is_tidy_topic_state(data.frame(doc="a", type="b", topic="c")))
})

test_that("is_tidy_topic_state returns false for tibble table input with missing columns", {
    expect_false(is_tidy_topic_state(dplyr::data_frame(doc="a", type="b")))
    expect_false(is_tidy_topic_state(dplyr::data_frame(doc="a", topic="c")))
    expect_false(is_tidy_topic_state(dplyr::data_frame(type="b", topic="c")))
})

test_that("is_tidy_topic_state returns true for tibble table input with expected columns", {
    expect_true(is_tidy_topic_state(tibble::data_frame(doc="a", type="b", topic="c")))
    expect_true(is_tidy_topic_state(dplyr::data_frame(doc="a", type="b", topic="c")))
})

test_that("file_exists returns false for non-string input", {
    expect_false(file_exists(1))
    expect_false(file_exists(c("a", "b")))
    expect_false(file_exists(data.frame()))
    expect_false(file_exists(list()))
})

test_that("file_exists returns false for non-existing file input", {
    expect_false(file_exists("./files/missing_file.txt"))
})

test_that("file_exists returns true for existing file input", {
    expect_true(file_exists("./files/empty_file.valid"))
})

test_that("is_file_type returns false for non-string input", {
    expect_false(is_file_type(1, ".txt"))
    expect_false(is_file_type(c("a", "b"), ".txt"))
    expect_false(is_file_type(data.frame(), ".txt"))
    expect_false(is_file_type(list(), ".txt"))
})

test_that("is_file_type returns false for non-existing file input", {
    expect_false(is_file_type("./files/missing_file.txt", ".txt"))
})

test_that("is_file_type returns false for invalid type", {
    expect_false(is_file_type("./files/empty_file.valid", data.frame(a=".txt")))
    expect_false(is_file_type("./files/empty_file.valid", list(a=".txt")))
    expect_false(is_file_type("./files/empty_file.valid", 123))
    expect_false(is_file_type("./files/empty_file.valid", c(".valid", ".json")))
})

test_that("is_file_type returns true for existing file input", {
    expect_true(is_file_type("./files/empty_file.valid", ".valid"))
    expect_true(is_file_type("./files/empty_file.valid", "valid"))
    expect_true(is_file_type("./files/empty_file.json", ".json"))
    expect_true(is_file_type("./files/empty_file.json", "json"))
})

test_that("contain_regexp returns false for non-string input", {
    expect_false(contain_regexp(data.frame(a="hello world"), "hello"))
})

test_that("contain_regexp returns false for non-string pattern", {
    expect_false(contain_regexp("hello world", 123))
})

test_that("contain_regexp returns false for non-existing pattern", {
    expect_false(contain_regexp("json", "(\\.)\\w+"))
})

test_that("contain_regexp returns true for existing pattern", {
    expect_true(contain_regexp(".json", "(\\.)\\w+"))
})
