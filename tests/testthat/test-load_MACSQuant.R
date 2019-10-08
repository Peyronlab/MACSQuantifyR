# Tests for functions dependent on import data
library(tools)
set.seed(42)

filepath <- system.file("extdata", "drugs.xlsx", package = "MACSQuantifyR")


# Tests for object import
# --------------------------------------------------------------------------------
context("Object creation")
MACSQuant <- load_MACSQuant(filepath)


test_that("data is imported correctly", {
    expect_equal(dim(MACSQuant@my_data)[2], 4)
    expect_true(dim(MACSQuant@my_data)[1] > 0)
    expect_equal(colnames(MACSQuant@my_data), c("Full path", "WID", "%-#", "Count/mL"))
})

test_that("filename is correctly formated", {
    expect_match(file_ext(filepath), "^xls$|^xlsx$")
})

