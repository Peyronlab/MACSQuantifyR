### MACSQuant

#' The MACSQuant Class
#'
#' MACSQuant object contains all the data and the user parameters
#'
#' @slot my_data Contains the raw data
#' @slot my_data_sorted  Contains the sorted data
#' (according to replicates order)
#' @slot my_replicates_sorted  Contains the sorted matrix containing
#' replicates names
#' @slot experiment_name Contains the experiment name
#' given by the user
#' @slot statistics Contains the statistics for each condition
#' @slot combination.index.df data.frame that contains the outputs of
#' the combination_index function
#' @slot param.experiment list that contains the parameters relative
#' to the experiment (i.e, number of replicates,
#' number of conditions...)
#' @slot param.output  List that contains the parameters relative
#' to the report generation and outputs (i.e, save intermediary
#' results, generate full report..)
#'
#' @name MACSQuant
#' @rdname MACSQuant
#' @aliases MACSQuant-class
#' @exportClass MACSQuant
#'

MACSQuant <- setClass("MACSQuant",
        slots = c(my_data = "data.frame",
        my_data_sorted = "data.frame",
        my_replicates_sorted = "matrix",
        experiment_name = "character",
        statistics = "data.frame",
        combination.index.df = "data.frame",
        param.experiment = "list",
        param.output = "list"))

