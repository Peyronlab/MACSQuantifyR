#' @export
#' @importFrom ggplot2 aes element_text geom_bar
#' geom_errorbar ggplot ggtitle position_dodge
#' scale_fill_manual theme theme_minimal xlab ylab
#' ggsave facet_grid labs unit element_blank
#' @importFrom lattice cloud
#' @importFrom latticeExtra panel.3dbars
#'
#' @title Generate barplots
#' @description
#' Uses the informations stored in the statistic tables of
#' the MACSQuant object to generate barplots for the specified
#' conditions. Plots can also be saved in the form of a png
#' file in the output_MQ directory.

#' @name barplot_data
#' @rdname barplot_data
#' @aliases barplot_data
#' @usage
#' barplot_data(MACSQuant,plt.col=NULL,plt.conditions=NULL, plt.flavour=NULL,
#' plt.labels = NULL, plt.combo=FALSE,plt.3D.only = NULL,...)
#' @param MACSQuant object of class MACSQuant resulting of the function
#' load_maxQuant().
#' Contains the original data table
#' @param plt.col In case of manual run is used to define the bar colors.
#' (must be the same for all conditions)
#' @param plt.conditions user defined vector of conditions to plot
#' (example: plt.conditions=c(1:9))
#' @param plt.flavour Which barplot to produce, cell count or percentage of
#' fluorochrome
#' (i.e: "counts","percent")
#' @param plt.labels xlabels of the plot
#' @param plt.combo Does the experiment involves multiple variables
#' (i.e some condition are a drug combination screening)
#' @param plt.3D.only Whether to plot 3D barplot alone
#' @param ... params for lattice cloud namely z and x for parameter screen
#' default for z and x are c(-110,-70) and argument xlab and ylab
#' @examples
#' drugs_R_image <- system.file("extdata",
#'     "drugs.RDS",
#'     package = "MACSQuantifyR")
#' MACSQuant <- readRDS(drugs_R_image)
#' flavour <- "counts"
#' number_of_conditions <-
#'     slot(MACSQuant, "param.experiment")$number_of_conditions
#' plt.col <- c(rep(2, number_of_conditions), 1)
#' barplot_data(MACSQuant,
#'     plt.col = plt.col,
#'     plt.conditions = NULL,
#'     plt.flavour = flavour,
#'     plt.labels = NULL,
#'     plt.combo = FALSE)
#' @return returns a barplot of the data
#'
barplot_data <- function(MACSQuant,
                        plt.col = NULL,
                        plt.conditions = NULL,
                        plt.flavour = NULL,
                        plt.labels = NULL,
                        plt.combo = FALSE,
                        plt.3D.only = NULL,
                        ...) {
    if (!is.null(plt.3D.only) & !is.logical(plt.3D.only))
    {
        stop("plt.3D.only: logical argument expected")
    }

    l <- list(...)
    e <- new.env()
    if (length(l) > 0) {
        for (i in seq(length(l)))
        {
            assign(names(l)[[i]], l[[i]], envir = e)
        }
    }

    if (!"z" %in% names(e)) {
        e$z <- -110
    }
    if (!"x" %in% names(e)) {
        e$x <- -70
    }
    if (!"xlab" %in% names(e)) {
        e$xlab <- "Drug1"
    }
    if (!"ylab" %in% names(e)) {
        e$ylab <- "Drug2"
    }
    # check wich labels to apply to the plot
    # label exists not defined
    if (length(MACSQuant@param.output$plt.labels) > 0 &
        is.null(plt.labels)) {
        plt.labels <- MACSQuant@param.output$plt.labels
        # labels not diffined
    } else if (length(MACSQuant@param.output$plt.labels) == 0 &
        is.null(plt.labels)) {
        stop(paste("specify a character vector plt.labels",
            "OR define labels in MACSQuant@param.output$plt.labels",
            sep = " "))
    }

    # which plot to compute count or percent
    # check if arguments are correct
    if (is.null(plt.flavour)) {
        stop(paste("Argument plt.flavour incorrect",
            "please choose between 'counts' and 'percent'", sep = " "))
    } else if (plt.flavour != "counts" & plt.flavour != "percent") {
        stop(paste("Argument plt.flavour incorrect",
            "please choose between 'counts' and 'percent'", sep = " "))
        # apply choice to local environement variables
    } else if (plt.flavour == "counts") {
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
    # check if title is defined in MACSQuant object
    if (length(MACSQuant@param.output$plt.title) != 0) {
        plt.title <- MACSQuant@param.output$plt.title
    }

    # subset conditions in case of user specification
    # apply to local env variables
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

    # define plot variables
    plt.doses <- factor(MACSQuant@param.experiment$doses)
    plt.labels <- factor(plt.labels, levels = c(unique(plt.labels)))
    plt.condition <- factor(plt.col, levels = c(unique(plt.col)))

    # prepare ggplot data.f
    if (plt.combo == FALSE & is.null(plt.3D.only)) {

        data.f <- cbind(plt.statistics, plt.doses)
        p <- ggplot(data.f, aes(x = plt.doses, y = plt.y)) +

            geom_bar(position = position_dodge(), stat = "identity",
                colour = "black", size = 0.1, aes(fill = plt.labels),
                alpha = .7) +

            geom_errorbar(aes(ymin = plt.y - plt.y.sd, ymax = plt.y + plt.y.sd),
                size = 0.3, width = 0.2, position = position_dodge(0.9)) +

            xlab("Dose") + ylab(plt.y.lab) +

            scale_fill_manual(
                values = as.character(unique(plt.condition)),
                name = "Conditions",
                labels = unique(plt.labels)
            ) +

            ggtitle(plt.title) +
            theme_minimal() +
            theme(axis.title.x = element_text(
                face = "bold", colour = "#990000", size = 15),
            axis.title.y = element_text(
                face = "bold", colour = "#990000", size = 15),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
        #     angle = 45,
        #     vjust = 0.5,
    } else if (plt.combo == TRUE & is.null(plt.3D.only)) {
        plt.doses.alt <- factor(MACSQuant@param.experiment$doses.alt)
        plt.doses <- factor(MACSQuant@param.experiment$doses)
        if (MACSQuant@param.experiment$control) {
            plt.statistics <- plt.statistics[-nrow(plt.statistics), ]
            plt.y <- plt.y[-length(plt.y)]
        }
        plt.labels <- factor(plt.labels[-length(plt.labels)])
        plt.title <- plt.title

        data.f <- cbind(plt.statistics, plt.doses, plt.doses.alt)
        p1 <- ggplot(data.f, aes(x = plt.doses, y = plt.y)) +
            geom_bar(position = position_dodge(), stat = "identity",
                colour = "black",
                size = 0.1, aes(fill = plt.labels), alpha = .7) +
            facet_grid(. ~ plt.doses.alt, space = "free_x", scales = "free_x",
                switch = "x") +

            labs(x = "") + theme(panel.spacing = unit(0, "lines"),
                strip.background = element_blank(),
                strip.placement = "outside") +
            scale_fill_manual(
                values = heat.colors(dim(plt.statistics)[1]),
                name = "Conditions" # ,
                # labels = unique(plt.labels)
            ) +
            xlab("Dose") + ylab(plt.y.lab) +
            ggtitle(plt.title) +
            theme_minimal() +
            theme(axis.title.x = element_text(
                face = "bold", colour = "#990000", size = 15),
            axis.title.y = element_text(
                face = "bold", colour = "#990000", size = 15),
            axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
            axis.text.y = element_text(size = 12)
            )


        p2 <- cloud((plt.y ~ data.f$plt.doses + plt.doses.alt),
            data.f, panel.3d.cloud = panel.3dbars,
            col.facet =  plt.col, xbase = .8, ybase = .3,
            scales = list(arrows = FALSE, col = 1,
                distance = 1),
            screen = list(z = e$z, x = e$x),
            par.settings = list(box.3d = list(col = NA)),
            xlab = e$xlab, ylab = e$ylab, zlab = plt.flavour)


        p <- gridExtra::arrangeGrob(p1, p2, ncol = 2)

    } else {
        plt.doses.alt <- factor(MACSQuant@param.experiment$doses.alt)
        plt.doses <- factor(MACSQuant@param.experiment$doses)
        if (MACSQuant@param.experiment$control) {
            plt.statistics <- plt.statistics[-nrow(plt.statistics), ]
            plt.y <- plt.y[-length(plt.y)]
        }
        data.f <- cbind(plt.statistics, plt.doses, plt.doses.alt)
        p <- cloud((plt.y ~ data.f$plt.doses + plt.doses.alt),
            data.f, panel.3d.cloud = panel.3dbars,
            col.facet =  plt.col, xbase = .8, ybase = .3,
            scales = list(arrows = FALSE, col = 1,
                distance = 1),
            screen = list(z = e$z, x = e$x),
            par.settings = list(box.3d = list(col = NA)),
            xlab = e$xlab, ylab = e$ylab, zlab = plt.flavour)
    }

    return(p)
}
