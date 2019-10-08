#' @export
#'
#' @title pipeline with report generation
#' @description
#' Loads user's data, prompt a graphical representation of a 96
#' well plate and let the user select where the duplicates of each
#' condition were placed. Use for reordering excel file.
#' Plate image with selection can also be saved in the form of a png
#' file in the output_MQ directory.
#'
#' Also generates a complete report with the graphics
#' @import prettydoc
#'
#' @name pipeline
#' @rdname pipeline
#' @aliases pipeline
#' @usage
#' pipeline(filepath,sheet_name=NULL,number_of_replicates,number_of_conditions,
#' control=FALSE,save.files=TRUE,MACSQuant.obj=NULL)
#' @param filepath path of the excel file
#' @param sheet_name Name of the sheet to load
#' (optional, first sheet is default)
#' @param number_of_replicates For each condition, the number of duplicates
#' (must be the same for all conditions)
#' @param number_of_conditions The number of conditions tested
#' (eg: Drug 1 alone, Drug 2 alone)
#' @param control Is there a control in this experiment (eg: Staurosporin)
#' @param save.files Used to save the image in the output folder
#' @param MACSQuant.obj object of class MACSQuant
#'
#' @examples
#' print("run manually, requires user input")
#' filepath <- system.file("extdata", "SingleDrugs.xlsx",
#'     package = "MACSQuantifyR")
#' # pipeline(filepath,3,5)
#' # let you select 5 conditions of 3 replicates each
#' @return A formatted report file along with intermediates results

source("./R/load_MACSQuant.R")

source("./R/on_plate_selection.r")


pipeline <- function(filepath, sheet_name = NULL,
                        number_of_replicates,
                        number_of_conditions,
                        control = FALSE,
                        save.files = TRUE,
                        MACSQuant.obj = NULL) {

    MACSQuant <- load_MACSQuant(filepath, sheet_name, MACSQuant.obj)

    MACSQuant <- on_plate_selection(MACSQuant,
        number_of_replicates,
        number_of_conditions,
        control,
        save.files)

    if (save.files == TRUE) {
        generate_report(MACSQuant)
    }
    return(MACSQuant)


}
