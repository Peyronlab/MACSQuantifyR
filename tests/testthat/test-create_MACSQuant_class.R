# Tests for functions dependent on a MACSQuant object
set.seed(42)

# load a minimal example data set (subset of nbt dataset)
filepath <- system.file("extdata", "drugs.xlsx",
    package = "MACSQuantifyR")


# Tests for object creation (via new/Setup)
# --------------------------------------------------------------------------------

# Generate Seurat object

object.test <- new(Class = "MACSQuant",
    my_data = data.frame(),
    my_data_sorted = data.frame(),
    statistics = data.frame(),
    param.experiment = list(),
    param.output = list())
object.test@param.experiment$number_of_replicates <- 0
object.test@param.experiment$number_of_conditions <- 0
object.test@param.experiment$doses <- numeric()
object.test@param.experiment$control <- logical()
object.test@param.experiment$c_names <- character()
object.test@param.output$save.files <- logical()

context("Object creation")

test_that("object initialization creates MACSQuant object", {
    expect_is(object.test, "MACSQuant")
})

context("Object creation: subtypes")

test_that("entered parameters set correctly", {
    expect_true(is.numeric(object.test@param.experiment$number_of_replicates))
    expect_true(is.numeric(object.test@param.experiment$number_of_conditions))
    expect_true(is.data.frame(object.test@my_data))
    expect_true(is.logical(object.test@param.experiment$control))
    expect_true(is.character(object.test@param.experiment$c_names))
    expect_true(is.logical(object.test@param.output$save.files))
    expect_true(is.data.frame(object.test@my_data_sorted))
})

context("Object creation: slot names")
test_that("slots names matches", {
    expect_equal(names(object.test@param.experiment),
        c("number_of_replicates",
            "number_of_conditions",
            "doses",
            "control",
            "c_names"))
    expect_equal(names(object.test@param.output),
        c("save.files"))
})
