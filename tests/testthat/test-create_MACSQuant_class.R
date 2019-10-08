# Tests for functions dependent on a MACSQuant object
set.seed(42)

# load a minimal example data set (subset of nbt dataset)
filepath <- system.file("extdata", "drugs.xlsx",
    package = "MACSQuantifyR")


# Tests for object creation (via new/Setup)
# --------------------------------------------------------------------------------

# Generate Seurat object

MACSQuant <- new(Class = "MACSQuant",
    my_data = data.frame(),
    my_data_sorted = data.frame(),
    statistics = data.frame(),
    param.experiment = list(),
    param.output = list())
MACSQuant@param.experiment$number_of_replicates <- 0
MACSQuant@param.experiment$number_of_conditions <- 0
MACSQuant@param.experiment$doses <- numeric()
MACSQuant@param.experiment$control <- logical()
MACSQuant@param.experiment$c_names <- character()
MACSQuant@param.output$save.files <- logical()

context("Object creation")

test_that("object initialization creates MACSQuant object", {
    expect_is(MACSQuant, "MACSQuant")
})

context("Object creation: subtypes")

test_that("entered parameters set correctly", {
    expect_true(is.data.frame(MACSQuant@my_data))
    expect_true(is.data.frame(MACSQuant@my_data_sorted))
    expect_true(is.matrix(MACSQuant@my_replicates_sorted))
    expect_true(is.data.frame(MACSQuant@statistics))
    expect_true(is.character(MACSQuant@experiment_name))
    expect_true(is.data.frame(MACSQuant@combination.index.df))
})

MACSQuant=new_class_MQ()
context("object initalization")
test_that("object parameters init correctly",{
    expect_true(is.numeric(MACSQuant@param.experiment$number_of_replicates))
    expect_true(is.numeric(MACSQuant@param.experiment$number_of_conditions))
    expect_true(is.logical(MACSQuant@param.experiment$control))
    expect_true(is.character(MACSQuant@param.experiment$c_names))
    expect_true(is.numeric(MACSQuant@param.experiment$doses))
    expect_true(is.numeric(MACSQuant@param.experiment$doses.alt))
    expect_true(is.character(MACSQuant@param.output$plt.title))
    expect_true(is.character(MACSQuant@param.output$plt.labels))
    expect_true(is.character(MACSQuant@param.output$path))
    expect_true(is.logical(MACSQuant@param.output$save.files))
})


context("Object creation: slot names")
test_that("slots names matches", {
    expect_equal(names(MACSQuant@param.experiment),
        c("number_of_replicates",
            "number_of_conditions",
            "doses",
            "doses.alt",
            "c_names",
            "control"))
    expect_equal(names(MACSQuant@param.output),
        c("plt.title",
         "plt.labels",
         "save.files",
         "path"))
})

