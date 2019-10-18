create_output_folder <- function(MACSQuant) {
    # Create folder containing various output files
    # and variables

    if (length(MACSQuant@param.output$path) > 0) {
        if (!dir.exists(MACSQuant@param.output$path))
        {stop("Provided path does not exists, please sepecify a valid path")}
        path <- paste(MACSQuant@param.output$path, "/outputMQ", sep = "")
    } else {
        path <- "./outputMQ"
    }
    if (!dir.exists(file.path(path))) {
        dir.create(file.path(path))
        message("--> Done: Folder ./outputMQ created")
    }
    return(MACSQuant)
}

init_GUI <- function()
# intialize GUI plate plot for user to select replicates on
{
    well_letter <- sort(toupper(letters[seq_len(8)]), decreasing = TRUE)
    df <- data.frame(df2 = seq_len(12), df1 = c(
        rep(1, 12), rep(2, 12),
        rep(3, 12), rep(4, 12), rep(5, 12), rep(6, 12), rep(7, 12), rep(
                8,
                12
        )
    ))
    rownames(df) <- vapply(toupper(letters[seq(8, 1)]), function(x) {
        paste(x, seq(12:1), sep = "")
    }, character(12))
    # TODO external X11()
    par(mar = c(3.1, 3.1, 3.1, 9), xpd = TRUE)
    plot(df,
        yaxt = "n", xaxt = "n", xlab = "", ylab = "", axes = FALSE,
        ylim = c(-0.49, 8.49), lwd = 3, pch = 21, cex = 4.5, col = "lightblue"
    )
    title("Well plate")
    axis(2, at = seq_len(8), labels = well_letter, las = 2, line = "1")
    axis(1, at = seq_len(12), labels = seq_len(12), line = "-2.5")
    text(df, labels = rownames(df), cex = 0.8, font = 1)
    return(well_letter)
}

to_well_names <- function(point_matrix, col, well_letter) {
    # interprete locations plot and return vector containing
    # the replicates names
    coordinates_raw <- unlist(point_matrix)
    coordinates_mat <- matrix(coordinates_raw, ncol = 2)
    coordinates_mat <- round(coordinates_mat)
    points(coordinates_mat, lwd = 1, pch = 21, cex = 3.2, col = col)
    coordinates_mat[, 2] <- well_letter[coordinates_mat[, 2]]
    coordinates_names <- paste(coordinates_mat[, 2], coordinates_mat[, 1],
        sep = ""
    )
    return(coordinates_names)
}

match_id_line <- function(MACSQuant, wid_vector) {
    # match replicate id in raw data table (my_data)
    # returns indices in raw data
    l.indices=lapply(wid_vector,
                FUN = function(j){which(MACSQuant@my_data$WID == j)})
    matched=unlist(l.indices)
    return(matched)
}

compute_statistics <- function(MACSQuant,
                                lines,
                                stats = "mean") {
    data_selected <- MACSQuant@my_data[lines, ]
    Counts <- c()
    Percent <- c()
    rep.indices=seq(1, MACSQuant@param.experiment$number_of_replicates * 2,
                2)
    c.list=lapply(rep.indices,
        function(rep){data_selected$`Count/mL`[rep] -
        data_selected$`Count/mL`[rep + 1]}
        )
    Counts=unlist(c.list)

    p.list=lapply(rep.indices,
        function(rep){(data_selected$`%-#`[rep + 1]) / 100}
    )
    Percent=unlist(p.list)

    Full.path.first <- unlist(strsplit(
        data_selected$`Full path`[1],
        "\\\\P1"
    ))
    WID.first <- sort(data_selected$WID)[1]

    if (stats == "mean") {
        Count.minus <- mean(Counts)
        Percent.plus <- mean(Percent)
    } else if (stats == "median") {
        Count.minus <- median(Counts)
        Percent.plus <- median(Percent)
    } else {
        stop(paste("Values for argument stats",
                "should be c('mean','median'). Other not supported",
                sep = " "
        ))
    }
    sd_count <- sd(Counts)
    Percent.minus <- 1 - Percent.plus
    sd_percent <- sd(Percent)

    results <- c(
        Full.path.first, WID.first, Percent.plus, Percent.minus,
        sd_percent, Count.minus, sd_count
    )
    return(results)
}



order_data <- function(MACSQuant) {

    # is_variable_missing=(check_variables()) if
    # (length(is_variable_missing)>0){stop(is_variable_missing)}

    # pre allocate indice matrix
    indice_matrix <- matrix(nrow = 1)
    # turn sorted matrix into vector
    sorted_vector <- as.vector(t(MACSQuant@my_replicates_sorted))

    for (i in sorted_vector) {
        matched <- t(which(MACSQuant@my_data$WID == i))
        if (length(matched) != 2) {
                warning(paste("Your file do not contains 2 gates",
                    " for replicate: ",
                    i, "\nPlease be sure to only have",
                    "P1 and P1/P2 gates in your file",
                sep = ""
                ))
        }
        indice_matrix <- cbind(indice_matrix, matched)
    }
    indices <- indice_matrix[, -c(1)]
    ignored <- which(!(seq_len(dim(MACSQuant@my_data)[1]) %in% indices))
    ignored_print <- paste(unique(MACSQuant@my_data$WID[ignored]),
        collapse = ",")

    if (length(indices) != dim(MACSQuant@my_data)[1]) {
        warning(paste("!!! ",
                ignored_print,
                " not selected and will be ignored",
                sep = "", collapse = ""
        ))
    }
    if (MACSQuant@param.output$save.files == TRUE) {
        write.table(MACSQuant@my_data[indices, ],
                paste(MACSQuant@param.output$path,
                        "/outputMQ/sorted_table.txt", sep = ""),
                sep = "\t"
        )
        message("--> Done: New data table saved table in ./outputMQ")
    }
    message("--> Done: sorted data stored in variable my_data_sorted")

    return(indices)
}

#
# check_variables <- function() {
#     warn <- NULL
#     if (!exists("my_data_sorted")) {
#         warn <- paste("Variables 'my_data_sorted'",
#                 "missing: Please run order_data() first",
#                 sep = " "
#         )
#     }
#     if (!exists("my_replicates_sorted")) {
#         warn <- paste("Variables 'my_replicates_sorted'",
#                 "missing: Please run on_plate_selection() first",
#                 sep = " "
#         )
#     }
#     if (!(exists("my_data"))) {
#         warn <- paste("Variable 'my_data'",
#                 "missing: Please run load_maxQuant() first",
#                 sep = " "
#         )
#     }
#     return(warn)
# }
