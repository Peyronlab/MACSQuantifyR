#' @export
#' @importFrom graphics axis locator plot points text title legend par
#' @importFrom grDevices dev.copy dev.off png
#' @importFrom utils write.table
#' @importFrom stats median sd
#' @importFrom grDevices heat.colors
#' @importFrom ggplot2 aes element_text geom_bar
#' geom_errorbar ggplot ggtitle position_dodge
#' scale_fill_hue theme xlab ylab
#' @importFrom rlang .data
#' @importFrom methods new
#'
#' @title Sort experimental design on graphical 96-well-plate
#' @description
#' Loads user's data, prompt a graphical representation of a 96
#' well plate and let the user select where the duplicates of each
#' condition were placed. Use for reordering excel file.
#' Plate image with selection can also be save in the form of a png
#' file in the output_MQ directory.

#' @name on_plate_selection
#' @rdname on_plate_selection
#' @aliases on_plate_selection
#' @usage
#' on_plate_selection(MACSQuant,number_of_replicates,number_of_conditions,
#' control=FALSE,save.files=FALSE)
#' @param MACSQuant object of class MACSQuant resulting of the function load_maxQuant().
#' Contains the original data table
#' @param number_of_replicates For each condition, the number of duplicates
#' (must be the same for all conditions)
#' @param number_of_conditions The number of conditions tested
#' (eg: Drug 1 alone, Drug 2 alone)
#' @param control Is there a control in this experiment (eg: Staurosporin)
#' @param save.files Used to save the image in the output folder
#' @examples
#' print("run manually, requires user input")
#' #on_plate_selection(MACSQuant,3,5)
#' #let you select 5 conditions of 3 replicates each
#' @return A formatted report file along with intermediates results

source("./R/function_toolbox.R")


on_plate_selection <- function(MACSQuant,
                               number_of_replicates,
                               number_of_conditions,
                               control = FALSE,
                               save.files = FALSE) {

    # check if data is imported or not
    if (is.null(MACSQuant@my_data)) {
        stop(paste("Data missing: Please run load_MACSQuant or a",
            "new_class_MQ to create MACSQuant object", sep = " "))
    }

    # checking if arguments are correct
    if (!is.numeric(number_of_conditions) | !is.numeric(number_of_replicates)) {
        stop(paste("Both number_of_conditions and number_of_replicates",
            "must be numeric",
            sep = " "
        ))
    } else if (number_of_conditions <= 0 | number_of_replicates <= 0) {
        stop(paste("Both number_of_conditions and number_of_replicates",
            "must be positive",
            sep = " "
        ))
    } else {
        MACSQuant@param.experiment$number_of_replicates <- number_of_replicates
        MACSQuant@param.experiment$number_of_conditions <- number_of_conditions
    }

    # checking if plot options (xlabs) are present
    # and correctly formatted
    if (length(MACSQuant@param.experiment$c_names) == 0) {
        col <- rep(2, number_of_conditions)
    } else if (length(MACSQuant@param.experiment$c_names) ==
        number_of_conditions) {
        col <- heat.colors(length(MACSQuant@param.experiment$c_names))
    } else if (length(MACSQuant@param.experiment$c_names) !=
        number_of_conditions) {
        stop(paste("If you give a name to your conditions:",
            "all conditions except controls must be named",
            "(length(c_names)==number_of_conditions)\n", "
                   else rm(c_names) to remove condition names",
            sep = " "
        ))
    }

    # starting messages
    message("...To quit press ESC...")
    message(paste("...You can now select your conditions replicates",
        "(without control condition replicates)...",
        sep = " "
    ))

    # GUI plate plot init
    well_letter <- init_GUI()

    # initialization of dataframes sorted matrix of replicates
    # and statistics
    sorted_matrix <- matrix(
        data = NA,
        ncol = number_of_replicates,
        nrow = number_of_conditions
    )
    statistics <- data.frame(
        Full.path.first = NA,
        WID.first = NA,
        Fluo.percent.plus = NA,
        Fluo.percent.minus = NA,
        sd_percent = NA,
        Cell.count.minus = NA,
        sd_count = NA, stringsAsFactors = FALSE
    )

    # converting xy plot selecction to well name
    cat(paste("--> ", number_of_conditions, " conditions: ...", sep = ""))
    for (i in seq(1, number_of_conditions)) {
        # every condition compute statistics on replicates
        # locate replicates for condition i
        loc <- locator(type = "n", n = number_of_replicates)
        # ensure proper data selection
        if (length(loc) == 2) {
            sorted_matrix[i, ] <- to_well_names(loc, col[i], well_letter)
            matched <- match_id_line(MACSQuant, sorted_matrix[i, ])
            if (length(matched) != number_of_replicates * 2) {
                warning(c(
                    paste(
                        "Your file may not contains 2 gates",
                        "for one or more replicates in: ",
                        sep = " "
                    ),
                    paste(sorted_matrix[i, ], collapse = ","), "\nPlease",
                    "be sure to only have P1 and P1/P2 gates in your file"
                ))
            }
            statistics[i, ] <- compute_statistics(MACSQuant,
                matched,
                stats = "mean"
            )

            cat(paste(i, "...", sep = ""))
        } else {
            warning("No data selected")
        }
    }
    cat("OK")
    sorted_matrix_final <- sorted_matrix
    if (i != number_of_conditions) {
        stop("Process interrupted please start again")
    }
    message("\n--> Done: replicates identified")
    message("--> Done: statistics on each condition replicates")


    if (control == TRUE) {
        MACSQuant@param.experiment$control <- TRUE
        message("...You can now select your control replicates...")
        cat("--> 1 control: ...")

        # check with users
        sorted_matrix_ctrl <- matrix(
            data = NA, ncol = number_of_replicates,
            nrow = 1
        )

        loc2 <- locator(type = "n", n = number_of_replicates)
        if (length(loc2) == 2) {
            sorted_matrix_ctrl[1, ] <- to_well_names(loc2, 1, well_letter)
            matched <- match_id_line(MACSQuant, sorted_matrix_ctrl[1, ])
            if (length(matched) != number_of_replicates * 2) {
                warning(c(
                    paste(
                        "Your file may not contains 2 gates",
                        "for one or more replicates in: ",
                        sep = " "
                    ),
                    paste(sorted_matrix_ctrl[1, ], collapse = ","),
                    "\nPlease",
                    "be sure to only have P1 and P1/P2 gates in your file"
                ))
            }
            statistics[i + 1, ] <- compute_statistics(MACSQuant, matched,
                stats = "mean"
            )
            cat("OK...")

            # print(paste('selection',paste(sorted_matrix_ctrl[1,],collapse =
            # ','),sep=' '))
            message("\n--> Done: statistics on each control replicates")
            sorted_matrix_final <- rbind(sorted_matrix_final,
                sorted_matrix_ctrl)
            print(sorted_matrix_final)
        } else {
            warning("No data selected")
        }
    }
    MACSQuant@my_replicates_sorted <- sorted_matrix_final
    if (control == FALSE &
        (length(MACSQuant@param.experiment$c_names) == 0)) {
        legend("topright",
            inset = c(-0.3, 0.05), legend = c("Conditions"),
            pch = c(1), col = col[1]
        )
        statistics[, seq(3, 7)] <- vapply(statistics[, seq(3, 7)], function(x) {
            as.numeric(x)
        }, numeric(number_of_conditions))
    } else if (control == TRUE &
        (length(MACSQuant@param.experiment$c_names) == 0)) {
        legend("topright", inset = c(-0.3, 0.05), legend = c(
            "Conditions",
            "Control"
        ), pch = c(1), col = c(col[1], 1))
        statistics[, seq(3, 7)] <- vapply(statistics[, seq(3, 7)], function(x) {
            as.numeric(x)
        }, numeric(number_of_conditions + 1))
    } else if (control == FALSE &
        (length(MACSQuant@param.experiment$c_names) != 0)) {
        legend("topright",
            inset = c(-0.3, 0.05), legend = .data$c_names,
            pch = c(1), col = col
        )
        statistics[, seq(3, 7)] <- vapply(statistics[, seq(3, 7)], function(x) {
            as.numeric(x)
        }, numeric(number_of_conditions))
    } else if (control == TRUE &
        (length(MACSQuant@param.experiment$c_names) != 0)) {
        legend("topright", inset = c(-0.3, 0.05), legend = c(
            MACSQuant@param.experiment$c_names,
            "Control"
        ), pch = c(1), col = c(col, 1))
        statistics[, seq(3, 7)] <- vapply(statistics[, seq(3, 7)], function(x) {
            as.numeric(x)
        }, numeric(number_of_conditions + 1))
    }


    if (save.files == TRUE) {
        MACSQuant@param.output$save.files <- TRUE
        create_output_folder()
        dev.copy(png, "./outputMQ/plate_template.png",
            width = 600, height = 600,
            units = "px"
        )
        dev.off()
        message("--> Done: image saved in ./outputMQ")
        write.table(statistics, "./outputMQ/statistics.txt ", sep = "\t")
        message("--> Done: statistics table saved in ./outputMQ")
    }


    message("--> Done: replicates stored in variable my_replicates_sorted")
    MACSQuant@statistics <- statistics

    indices <- order_data(MACSQuant)
    MACSQuant@my_data_sorted <- MACSQuant@my_data[indices, ]
    message("...You can now run plot_data(...)...")
        if(MACSQuant@param.experiment$control==T){
            MACSQuant@param.experiment$doses=seq_len(number_of_conditions+1)
            col=c(col,1)
        } else {
            MACSQuant@param.experiment$doses=seq_len(number_of_conditions)}
    plot_data(MACSQuant, col,
        plt.title = "Barplot test", flavour = "percent"
    )
    return(MACSQuant)
}
