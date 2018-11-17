#' @export
#' @importFrom methods new
#' @title Create a new MACSQuant object
#' @description
#' The user can create a new custom MACSQuant object
#' This function is also used in internal code for
#' MACSQuant initialization
#' @name new_class_MQ
#' @aliases new_class_MQ
#' @usage
#' #initialize empty MACSQuant object
#' new_class_MQ(my_data=NULL,my_data_sorted=NULL,my_replicates_sorted=NULL,
#' number_of_replicates=NULL,number_of_conditions=NULL,doses=NULL,c_names=NULL,
#' control=NULL,save.files=NULL)
#' #initialize custom MACSQuant object
#' new_class_MQ(my_data,my_data_sorted=NULL,my_replicates_sorted=NULL,
#' number_of_replicates=NULL,number_of_conditions=NULL,doses=NULL,
#' c_names=NULL,control=NULL,save.files=NULL)
#' @param my_data Contains the raw data
#' @param my_data_sorted  Contains the sorted data (according to replicates order)
#' @param my_replicates_sorted  Contains the sorted matrix containing replicates names
#' @param number_of_replicates For each condition, the number of duplicates
#' (must be the same for all conditions)
#' @param number_of_conditions The number of conditions tested
#' (eg: Drug 1 alone, Drug 2 alone)
#' @param doses  Numeric vector representing doses for each conditions
#' @param c_names Vector containing experiment names
#' @param control logical: is there a control in this experiment (eg: Staurosporin)
#' @param save.files Used to save the image in the output folder
#'
#' @examples
#' #initialize empty MACSQuant object
#' new_class_MQ()
#' #initialize custom MACSQuant object
#' my_data=data.frame(character(length=10), character(length=10),
#' numeric(length=10), numeric(length=10))
#' names(my_data)=c("Full path", "WID", "\%-#", "Count/mL")
#' new_class_MQ(my_data)
#'
#' @return A formatted report file along with intermediates results


new_class_MQ <- function(my_data = NULL,
                         my_data_sorted = NULL,
                         my_replicates_sorted = NULL,
                         number_of_replicates = NULL,
                         number_of_conditions = NULL,
                         doses = NULL,
                         c_names = NULL,
                         control = NULL,
                         save.files = NULL) {
    if (is.null(my_data)) {
        my_data <- data.frame()
    }
    if (is.null(my_data_sorted)) {
        my_data_sorted <- data.frame()
    }
    if (is.null(my_replicates_sorted)) {
        my_replicates_sorted <- matrix()
    }
    if (is.null(number_of_replicates)) {
        number_of_replicates <- 0
    }
    if (is.null(number_of_conditions)) {
        number_of_conditions <- 0
    }
    if (is.null(doses)) {
        doses <- numeric()
    }
    if (is.null(c_names)) {
        c_names <- character()
    }
    if (is.null(control)) {
        control <- FALSE
    }
    if (is.null(save.files)) {
        save.files <- FALSE
    }

    MACSQuant <- new(Class = "MACSQuant",
        my_data = my_data,
        my_data_sorted = my_data_sorted,
        my_replicates_sorted = my_replicates_sorted,
        param.experiment = list(
            number_of_replicates = number_of_replicates,
            number_of_conditions = number_of_conditions,
            doses = doses,
            c_names = c_names,
            control = control),
        param.output = list(
            save.files = save.files
    ))

    return(MACSQuant)
}
