# Tests for functions dependent on replicate sorting
set.seed(43)

filepath <- system.file("extdata", "drugs.xlsx", package = "MACSQuantifyR")

number_of_replicates <- 3
number_of_conditions <- 18


filepath <- system.file("extdata", "drugs.xlsx", package = "MACSQuantifyR")
# Tests for object import
# --------------------------------------------------------------------------------
context("Object integrity before running function")
MACSQuant <- load_MACSQuant(filepath)
MACSQuant@param.output$c_names <- c(paste("cond", rep(1:18), sep = " "))

test_that("required fiels correct", {
    expect_true(unique(dim(MACSQuant@my_data) != c(0, 0)))
    expect_true(unique(dim(MACSQuant@my_data_sorted) == c(0, 0)))
    expect_true(unique(dim(MACSQuant@statistics) == c(0, 0)))
    expect_true(length(MACSQuant@param.experiment$number_of_replicates) != 0)
    expect_true(length(MACSQuant@param.experiment$number_of_conditions) != 0)
    expect_equal(length(MACSQuant@param.output$c_names), number_of_conditions)
})
