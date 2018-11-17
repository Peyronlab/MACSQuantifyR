#' @export
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#' @importFrom methods new
#'
#' @title xls file from maxQuant 96-well-plate device
#' @name load_MACSQuant
#' @rdname load_MACSQuant
#' @aliases load_MACSQuant
#' @usage
#' load_MACSQuant(filepath,sheet_name)
#' @param filepath path of the excel file
#' @param sheet_name Name of the sheet to load
#' (optional, first sheet is default)
#' @examples
#' filepath=system.file('extdata', 'SingleDrugs.xlsx',
#' package = 'MACSQuantifyR')
#' # load_MACSQuant(filepath)
#' @return An object called MACSQuant of class MACSQuant containing
#' variable my_data that corresponds to the data of the excel file in R

source("./R/function_toolbox.R")

load_MACSQuant <- function(filepath, sheet_name = NULL) {
    ext <- c("xls", "xlsx")
    # check for correct extention type and file
    if (!file.exists(filepath)) {
        stop("File or variable not found")
    } else if (!file_ext(filepath) %in% ext) {
        stop("File compatibility error, try xls or xlsx files")
    } else {
        my_data <- read_excel(filepath, sheet_name)
        data <- as.data.frame(my_data)
        if (dim(data)[2] != 4) {
            # is the data a MQ file?
            stop(paste(
                "Check your data file, column missing.",
                "Columns shoud be 'Full path','WID','%-#','Count/mL'"
            ),
            sep = " "
            )
        }
        if (sum((names(data) == c("Full path", "WID", "%-#", "Count/mL"))) !=
            4) {
            stop(paste("Check your data file,",
                "column names shoud be 'Full path','WID','%-#','Count/mL'",
                sep = " "
            ))
        }
        message("--> Done: data loaded")
        message("--> Done: data stored in variable MACSQuant@my_data")
        message(paste("...You can now run",
            "on_plate_selection(MACSQuant,num_replicates,number_of_conditions)",
            "with your replicates and conditions numbers...",
            sep = " "
        ))
        MACSQuant <- new_class_MQ()
        MACSQuant@my_data <- my_data

        # assign("my_data", data, envir = .GlobalEnv)
        # asssigning data to my_data
    }
    return(MACSQuant)
}
