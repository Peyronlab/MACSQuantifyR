# Tests for functions in toolbox script
set.seed(42)

# load a minimal example data set (subset of nbt dataset)
filepath <- system.file("extdata", "drugs.xlsx",
    package = "MACSQuantifyR")
load_MACSQuant(filepath)

# Tests for output folder creation
# --------------------------------------------------------------------------------

MACSQuant <- load_MACSQuant(filepath)
MACSQuant@param.output$c_names <- c(paste("cond", rep(1:18), sep = " "))

context("path validity; create_output_folder()")
test_that("path exists", {
    expect_true(dir.exists(MACSQuant@param.output$path))
})

# Tests for GUI init
# --------------------------------------------------------------------------------

context("GUI format; init_GUI()")
well_letter=init_GUI()
test_that("GUI formated correclty", {
    expect_identical(well_letter,
                    c("H",
                     "G",
                     "F",
                     "E",
                     "D",
                     "C",
                     "B",
                     "A")
                )
})

# Tests for GUI interpretation
# --------------------------------------------------------------------------------
number_of_replicates <- 3
number_of_conditions <- 1
col <- rep(2, number_of_conditions)
point_matrix=list(x=c(2,2,2),y=c(7,6,5))
coordinates_names=to_well_names(point_matrix,col,well_letter)

context("GUI reading; to_well_names()")
test_that("GUI selection succeded", {
    expect_true(length(coordinates_names)==
                    number_of_replicates &
                is.null(dim(coordinates_names)))

    expect_true(length(unique(coordinates_names))==
                    number_of_replicates)

    expect_true((length(point_matrix) == 2 &
                     sum(point_matrix$y < 8.5) == 3 &
                     sum(point_matrix$y > 0.5) == 3 &
                     sum(point_matrix$y < 12.5) == 3 &
                     sum(point_matrix$x > 0.5) == 3))
})

# Tests for excel correspondance
# --------------------------------------------------------------------------------

matched <- match_id_line(MACSQuant, coordinates_names)
context("excel corresp; match_id_line()")
test_that("selected wells matched ", {
    expect_true(length(matched)==
                number_of_replicates*2)

    expect_true(sum(is.na(matched))==0)
    })


# Tests for statistical results
# --------------------------------------------------------------------------------
MACSQuant@param.experiment$number_of_replicates=3
MACSQuant@param.experiment$number_of_conditions=1

statistics <- data.frame(
    Full.path.first = NA,
    WID.first = NA,
    Fluo.percent.plus = NA,
    Fluo.percent.minus = NA,
    sd_percent = NA,
    Cell.count.minus = NA,
    sd_count = NA, stringsAsFactors = FALSE
)

statistics[1,]=compute_statistics(MACSQuant,
                   matched,
                   stats = "mean"
)

statistics[, seq(3, 7)] <- vapply(statistics[, seq(3, 7)], function(x) {
    as.numeric(x)
}, numeric(number_of_conditions))


context("statistics computation; compute_statistics()")
test_that("statistics dataframe is processed correctly", {
    expect_true(dim(statistics)[2]==7)
    expect_true(is.character(statistics$Full.path.first))
    expect_true(is.character(statistics$WID.first))
    expect_true(is.numeric(statistics$Fluo.percent.plus))
    expect_true(is.numeric(statistics$Fluo.percent.minus))
    expect_true(is.numeric(statistics$sd_percent))
    expect_true(is.numeric(statistics$Cell.count.minus))
    expect_true(is.numeric(statistics$sd_count))
    })

test_that("statistics are rendered correctly", {
    expect_equal(statistics$Fluo.percent.plus,1,tolerance=.99)
    expect_equal(statistics$Fluo.percent.minus,1,tolerance=.99)
    expect_gt(statistics$sd_percent,0)
    expect_gt(statistics$sd_count,0)
    expect_gt(statistics$Cell.count.minus,0)
})



