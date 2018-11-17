# Tests for functions dependent on import data
set.seed(42)

filepath <- system.file("extdata", "drugs.xlsx", package = "MACSQuantifyR")


# Tests for object import
# --------------------------------------------------------------------------------
context("Object creation")
MACSQuant <- load_MACSQuant(filepath)


test_that("data is imported correctly", {
    expect_equal(dim(MACSQuant@my_data), c(110, 4))
    expect_equal(colnames(MACSQuant@my_data), c("Full path", "WID", "%-#", "Count/mL"))
})

test_that("filename is correctly formated", {
    expect_match(file_ext(filepath), "^xls$|^xlsx$")
})
