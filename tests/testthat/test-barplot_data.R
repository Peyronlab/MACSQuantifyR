# Tests for output folder creation
# --------------------------------------------------------------------------------
filepath <- system.file("extdata", "drugs.RDS",
                        package = "MACSQuantifyR")
MACSQuant=readRDS(filepath)

plt.col = NULL
plt.conditions = NULL
plt.flavour = 'counts'
plt.labels = NULL
plt.combo = FALSE
plt.3D.only = NULL

plt.labels=MACSQuant@param.output$plt.labels
plt.col=MACSQuant@param.output$plt.col

context("argument validity; barplot_data()")
test_that("arguments are correctly specified", {
  expect_true(is.numeric(plt.col)|is.character(plt.col))
  expect_true(plt.flavour%in%c('percent','counts'))
  expect_true(is.numeric(plt.labels)|is.character(plt.labels))
  expect_is(plt.combo,"logical")
  expect_true(is.null(plt.3D.only)|is.logical(plt.3D.only))
 expect_identical(length(plt.col),length(plt.labels))
})




context("argument validity, plot.combo is true; barplot_data()")

filepath <- system.file("extdata", "drugs.Rdata",
                        package = "MACSQuantifyR")
load(filepath)
plt.combo = TRUE
plt.col <-
  c(
    heat.colors(length(slot(MACSQuant, "param.experiment")$c_names)), 1)

plt.labels <- c(slot(MACSQuant, "param.experiment")$c_names, "Control")
number_of_conditions=slot(MACSQuant, "param.experiment")$number_of_conditions

test_that("arguments are correctly specified", {
  expect_true(length(plt.labels)==number_of_conditions+1)
  expect_true(length(plt.col)==number_of_conditions+1)
  
})
