plot_data <- function(MACSQuant,
                      plt.col,
                      plt.title = "Barplot",
                      plt.conditions = NULL,
                      flavour = NULL) {
    if (is.null(flavour) | (flavour != "counts" & flavour != "percent")) {
        stop(paste("Argument flavour incorrect",
            "please choose between 'counts' and 'percent'", sep = " "))
    } else if (flavour == "counts") {
        plt.y <- MACSQuant@statistics$Cell.count.minus
        plt.y.sd <- MACSQuant@statistics$sd_count
        plt.y.lab <- "Cell count"
    } else {
        plt.y <- MACSQuant@statistics$Fluo.percent.minus
        plt.y.sd <- MACSQuant@statistics$sd_percent
        plt.y.lab <- "Percent fluo minus"
    }

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

    ggplot(plt.statistics, aes(x = MACSQuant@param.experiment$doses, y = plt.y)) + geom_bar(
        position = position_dodge(),
        stat = "identity", colour = "black", size = 0.3, fill = plt.col
    ) +
        geom_errorbar(aes(ymin = plt.y - plt.y.sd, ymax = plt.y + plt.y.sd),
            size = 0.3, width = 0.2, position = position_dodge(0.9)
        ) +
        xlab("Dose") + ylab("Cell.count") + scale_fill_hue(
            name = "Doses",
            labels = seq_len(9)
        ) + ggtitle(plt.title) + theme(axis.title.x = element_text(
            face = "bold",
            colour = "#990000", size = 20
        ), axis.text.x = element_text(
            angle = 45,
            vjust = 0.5, size = 15
        )) #+
}
# theme_bw()
