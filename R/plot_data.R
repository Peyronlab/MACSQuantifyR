plot_data <- function(MACSQuant,
                      plt.col,
                      plt.conditions = NULL,
                      flavour = NULL,
                      plt.labels = NULL
) {
    if (length(MACSQuant@param.output$plt.labels) > 0 &
        is.null(plt.labels)) {
        plt.labels <- MACSQuant@param.output$plt.labels
    } else if (length(MACSQuant@param.output$plt.labels) == 0 &
        is.null(plt.labels)) {
        stop(paste("specify a character vector plt.labels",
            "or define labels in MACSQuant@param.output$plt.labels",
            sep = " "))
    }

    if (is.null(flavour)) {
        stop(paste("Argument flavour incorrect",
            "please choose between 'counts' and 'percent'", sep = " "))
    } else if (flavour != "counts" & flavour != "percent") {
        stop(paste("Argument flavour incorrect",
            "please choose between 'counts' and 'percent'", sep = " "))
    } else if (flavour == "counts") {
        plt.y <- MACSQuant@statistics$Cell.count.minus
        plt.y.sd <- MACSQuant@statistics$sd_count
        plt.y.lab <- "Cell count"
        plt.title <- "Barplot cell count"
    } else {
        plt.y <- MACSQuant@statistics$Fluo.percent.minus
        plt.y.sd <- MACSQuant@statistics$sd_percent
        plt.y.lab <- "Percent fluo minus"
        plt.title <- "Barplot percentage"
    }
    if (length(MACSQuant@param.output$plt.title) != 0) {
        plt.title <- MACSQuant@param.output$plt.title
    }
    # in case of user specification on which conditions to plot
    if (!is.null(plt.conditions)) {
        if (length(plt.col) != length(plt.conditions)) {
            warning(paste("Number of condition selected doesn't",
                "match length(plt.color)", sep = " "))
            plt.col <- heat.colors(length(plt.conditions))
        }
        plt.statistics <- MACSQuant@statistics[plt.conditions, ]
        plt.y <- plt.y[plt.conditions]
        plt.y.sd <- plt.y.sd[plt.conditions]
    } else {
        plt.statistics <- MACSQuant@statistics
    }
    plt.doses <- MACSQuant@param.experiment$doses
    plt.labels <- factor(plt.labels, levels = c(unique(plt.labels)))

    plt.condition <- factor(plt.col, levels = c(unique(plt.col)))
    p <- ggplot(plt.statistics, aes(x = plt.doses, y = plt.y)) +

        geom_bar(position = position_dodge(), stat = "identity", colour = "black",
            size = 0.1, aes(fill = plt.labels), alpha = .7) +

        geom_errorbar(aes(ymin = plt.y - plt.y.sd, ymax = plt.y + plt.y.sd),
            size = 0.3, width = 0.2, position = position_dodge(0.9)) +

        xlab("Dose") + ylab("Cell.count") +

        scale_fill_manual(
            values = as.character(unique(plt.condition)),
            name = "Conditions",
            labels = unique(plt.labels)
        ) +

        ggtitle(plt.title) +

        theme(axis.title.x = element_text(
            face = "bold", colour = "#990000", size = 20),
        axis.text.x = element_text(
            angle = 45,
            vjust = 0.5, size = 15)) +

        theme_minimal()

    return(p)
}
