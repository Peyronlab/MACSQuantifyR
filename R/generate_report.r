#' @export
#' @importFrom rmarkdown render
#' @importFrom png readPNG
#' @importFrom grid grid.raster
#' @importFrom gridExtra grid.arrange
#'
#' @title Word report generation
#' @description
#'
#' Allows the user to generate a report at every
#' steps of the analysis
#'
#' Also generates a complete report with the graphics

#' @name generate_report
#' @rdname generate_report
#' @aliases generate_report
#' @usage
#' generate_report(MACSQuant)
#' @param MACSQuant object of class MACSQuant resulting of the function
#' load_maxQuant().
#' Contains the original data table
#' @examples
#' drugs_R_image <- system.file("extdata",
#'     "drugs.RDS",
#'     package = "MACSQuantifyR")
#' MACSQuant <- readRDS(drugs_R_image)
#' generate_report(MACSQuant)
#' @return A formatted report


generate_report <- function(MACSQuant) {
    dir_path <- paste(MACSQuant@param.output$path, "/outputMQ/", sep = "")

    if (dir.exists(dir_path)) {

        saveRDS(MACSQuant, paste(dir_path, "MACSQuant.RDS", sep = ""))

        filepath <- system.file("rmd", "generate_report.rmd",
            package = "MACSQuantifyR")

        render(filepath, "word_document", output_file =
            paste(dir_path, "results.docx", sep = ""),
        params = list(exp = MACSQuant@experiment_name,
            path = MACSQuant@param.output$path))
    } else {
        warning("Report not generated!")
        warning("./outputMQ not found. Please be sure to be in the
                correct directory")
    }
}
