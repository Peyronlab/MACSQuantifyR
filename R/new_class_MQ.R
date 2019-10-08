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
#' experiment_name=NULL,statistics=NULL,combination.index.df=NULL,
#' number_of_replicates=NULL,number_of_conditions=NULL,doses=NULL,
#' doses.alt=NULL,c_names=NULL, control=NULL,plt.title=NULL,
#' plt.labels=NULL,plt.col=NULL,save.files=NULL,path=NULL)
#' #initialize custom MACSQuant object
#' new_class_MQ(my_data,my_data_sorted=NULL,my_replicates_sorted=NULL,
#' experiment_name=NULL,statistics=NULL,combination.index.df=NULL,
#' number_of_replicates=NULL,number_of_conditions=NULL,
#' doses=NULL,doses.alt=NULL, c_names=NULL,control=NULL,plt.title=NULL,
#' plt.labels=NULL,plt.col=NULL, save.files=NULL,path=NULL)
#' @param my_data Contains the raw data
#' @param my_data_sorted  Contains the sorted data
#' (according to replicates order)
#' @param my_replicates_sorted  Contains the sorted matrix containing
#' replicates names
#' @param experiment_name User defined experiment named. Used for the
#' report generation
#' @param statistics Contains the statistics for each condition
#' @param combination.index.df data.frame that contains the outputs of
#' the combination_index function
#' @param number_of_replicates For each condition, the number of duplicates
#' (must be the same for all conditions)
#' @param number_of_conditions The number of conditions tested
#' (eg: Drug 1 alone, Drug 2 alone)
#' @param doses  Numeric vector representing doses for each conditions
#' @param doses.alt In case of interaction test second dose vector
#' @param c_names Vector containing experiment names
#' @param control logical: is there a control in this experiment
#' (eg: Staurosporin)
#' @param plt.title Title of the experiment to add to the barplot
#' @param plt.labels legend labels for the bar plot
#' @param plt.col color vector for the graphical representations
#' @param save.files Used to save the image in the output folder
#' @param path path of the experiment output folder (default '.')
#'
#' @examples
#' # initialize empty MACSQuant object
#' new_class_MQ()
#' # initialize custom MACSQuant object
#' my_data <- data.frame(character(length = 10), character(length = 10),
#'     numeric(length = 10), numeric(length = 10))
#' names(my_data) <- c("Full path", "WID", "%-#", "Count/mL")
#' new_class_MQ(my_data)
#' @return A formatted report file along with intermediates results


new_class_MQ <- function(my_data = NULL,
                        my_data_sorted = NULL,
                        my_replicates_sorted = NULL,
                        experiment_name = NULL,
                        statistics = NULL,
                        combination.index.df = NULL,
                        number_of_replicates = NULL,
                        number_of_conditions = NULL,
                        doses = NULL,
                        doses.alt = NULL,
                        c_names = NULL,
                        control = NULL,
                        plt.title = NULL,
                        plt.labels = NULL,
                        plt.col = NULL,
                        save.files = NULL,
                        path = NULL) {
    if (is.null(my_data)) {
        my_data <- data.frame()
    }
    if (is.null(my_data_sorted)) {
        my_data_sorted <- data.frame()
    }
    if (is.null(my_replicates_sorted)) {
        my_replicates_sorted <- matrix()
    }
    if (is.null(experiment_name)) {
        experiment_name <- character()
    }
    if (is.null(statistics)) {
        statistics <- data.frame()
    }
    if (is.null(combination.index.df)) {
        combination.index.df <- data.frame()
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
    if (is.null(doses.alt)) {
        doses.alt <- numeric()
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
    if (is.null(plt.title)) {
        plt.title <- character()
    }
    if (is.null(plt.labels)) {
        plt.labels <- character()
    }
    if (is.null(plt.col)) {
        plt.col <- character()
    }
    if (is.null(path)) {
        path <- getwd()
    }

    MACSQuant <- new(Class = "MACSQuant",
        my_data = my_data,
        my_data_sorted = my_data_sorted,
        my_replicates_sorted = my_replicates_sorted,
        experiment_name = experiment_name,
        statistics = statistics,
        combination.index.df = combination.index.df,
        param.experiment = list(
                number_of_replicates = number_of_replicates,
                number_of_conditions = number_of_conditions,
                doses = doses,
                doses.alt = doses.alt,
                c_names = c_names,
                control = control),
                param.output = list(
                plt.title = plt.title,
                plt.labels = plt.labels,
                save.files = save.files,
                path = path
    ))

    return(MACSQuant)
}
