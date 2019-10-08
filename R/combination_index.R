#' @export
#'
#' @importFrom ggplot2 aes element_text
#' ggplot ggtitle
#' theme theme_minimal xlab ylab
#' element_line geom_abline geom_hline geom_point
#' geom_vline scale_color_manual scale_size_manual
#' stat_smooth xlim ylim ggsave
#' @importFrom stats lm
#' @importFrom ggrepel geom_text_repel
#' @importFrom lattice cloud
#' @importFrom latticeExtra panel.3dbars
#'
#' @title compute combination index
#' @description
#' This function allows the user to compute combination index
#' on the drug combinations. This function also generates intermediary
#' plots and tables.
#' @name combination_index
#' @rdname combination_index
#' @aliases combination_index
#' @usage
#' combination_index(MACSQuant, ...)
#' @param MACSQuant object of class MACSQuant resulting of the function
#' load_maxQuant().
#' Contains the original data table
#' @param ... params for lattice cloud namely z and x for parameter screen
#' default for z and x are c(-110,-70) and argument xlab and ylab
#'
#' @examples
#' filepath <- system.file("extdata", "drugs.Rdata",
#'     package = "MACSQuantifyR")
#' load(filepath)
#' combination_index(MACSQuant)
#' @references
#' Chou, T. C. (2006). Theoretical basis, experimental design, and
#' computerized simulation of synergism and antagonism in drug
#' combination studies. \emph{Pharmacological reviews}, 58(3), 621-681.
#'
#' @return Several plots and combination index


combination_index <- function(MACSQuant, ...) {
    #parsing options
    save.files <- MACSQuant@param.output$save.files
    #parsing specific arguments
    l <- list(...)
    e <- new.env()
    if (length(l) > 0) {
        for (i in seq(length(l)))
        {
            assign(names(l)[[i]], l[[i]], envir = e)
        }
    }
    if (!"xlab" %in% names(e)) {
        e$xlab <- "Drug1"
    }
    if (!"ylab" %in% names(e)) {
        e$ylab <- "Drug2"
    }

    #indices of single drug conditions
    ind_d1 <- which(MACSQuant@param.experiment$doses != 0 &
        MACSQuant@param.experiment$doses.alt == 0)
    ind_d2 <- which(MACSQuant@param.experiment$doses == 0 &
        MACSQuant@param.experiment$doses.alt != 0)

    #doses and response (fa) of single drug conditions
    d <- MACSQuant@param.experiment$doses[ind_d1]
    fa <- MACSQuant@statistics$Fluo.percent.plus[ind_d1]
    d2 <- MACSQuant@param.experiment$doses.alt[ind_d2]
    fa2 <- MACSQuant@statistics$Fluo.percent.plus[ind_d2]

    #preparing dose-response plot
    dose_rep_df <- as.data.frame(rbind(cbind(d, fa), cbind(d2, fa2)))
    dose_rep_df <- data.frame(dose_rep_df)
    dose_rep_df$dataset <- c(rep("A", length(d)),
        rep("B", length(d2)))

    #doses for combo conditions
    doses1_c <- MACSQuant@param.experiment$doses[-c(ind_d1, ind_d2)]
    doses2_c <- MACSQuant@param.experiment$doses.alt[-c(ind_d1, ind_d2)]

    #response for combo conditions
    fa_c <- MACSQuant@statistics$Fluo.percent.plus[-c(ind_d1, ind_d2)]

    #adapting data to previously set options
    if (MACSQuant@param.experiment$control) {
        fa_c <- fa_c[-length(fa_c)]
    }


    combo_df <- data.frame(doses1_c, doses2_c, fa_c)

    #dose response plot
    p1 <- ggplot(data = dose_rep_df,
        aes(x = d, y = fa, col = dose_rep_df$dataset)) +
        geom_point(size = 3) +
        scale_color_manual(name = "Dataset", labels = c(e$xlab, e$xlab),
            values = c("lightblue", "orange")) +
        scale_size_manual(name = "Dataset", labels = c(e$xlab, e$xlab)) +
        xlab("Dose") + ylab("Response") +
        ggtitle("Dose-response curve") +
        theme_minimal() +
        theme(axis.title.x = element_text(
            face = "bold", colour = "#990000", size = 15),
        axis.title.y = element_text(
            face = "bold", colour = "#990000", size = 15),
        axis.text.x = element_text(size = 12, vjust = 0.5),
        axis.text.y = element_text(size = 12)
        )

    if (save.files == TRUE) {
        ggsave(paste(MACSQuant@param.output$path,
            "/outputMQ/dose_response_curve.png", sep = ""),
        width = 15.875, height = 15.875,
        units = "cm", plot = p1)
    }

    #median effect plot
    p2 <- ggplot(data = dose_rep_df,
        aes(x = log10(d), y = log10(fa / (1 - fa)),
            col = dose_rep_df$dataset)) + geom_point(size = 3) +
        scale_color_manual(name = "Dataset", labels = c("Drug1", "Drug2"),
            values = c("lightblue", "orange")) +
        scale_size_manual(name = "Dataset", labels = c("Drug1", "Drug2")) +
        xlab("Dose") + ylab("Response") +
        ggtitle("Median effect plot (log dose-response)") +
        theme_minimal() +
        theme(axis.title.x = element_text(
            face = "bold", colour = "#990000", size = 15),
        axis.title.y = element_text(
            face = "bold", colour = "#990000", size = 15),
        axis.text.x = element_text(size = 12, vjust = 0.5),
        axis.text.y = element_text(size = 12)
        ) +
        stat_smooth(method = "lm", se = FALSE, n = 80,
            fullrange = TRUE, span = 50) +
        geom_hline(aes(yintercept = 0), size = 0.2, linetype = 2) +
        geom_vline(aes(xintercept = 0), size = 0.2, linetype = 2)

    if (save.files == TRUE) {
        ggsave(paste(MACSQuant@param.output$path,
            "/outputMQ/median_effect.png", sep = ""),
        width = 15.875, height = 15.875,
        units = "cm", plot = p2)
    }

    #linear regression on median effect plot data
    fit1 <- lm(log10(fa / (1 - fa)) ~ log10(d))
    fit2 <- lm(log10(fa2 / (1 - fa2)) ~ log10(d2))

    #extracting coefficients from regression
    m1 <- as.numeric(fit1$coefficients[2])
    b1 <- as.numeric(fit1$coefficients[1])
    m2 <- as.numeric(fit2$coefficients[2])
    b2 <- as.numeric(fit2$coefficients[1])

    #IC50 computation
    IC50_1 <- 10^(-b1 / m1)
    IC50_2 <- 10^(-b2 / m2)

    #Combination index computation
    DX1 <- IC50_1 * (fa_c / (1 - fa_c))^(1 / m1)
    DX2 <- IC50_2 * (fa_c / (1 - fa_c))^(1 / m1)
    CI <- (doses1_c / DX1) + (doses2_c / DX2)

    #normalization for isobologram plot
    doses1_c_norm <- (doses1_c / DX1)
    doses2_c_norm <- (doses2_c / DX2)
    DRI_1 <- (DX1 / doses1_c)
    DRI_2 <- (DX2 / doses2_c)

    #decision regarding synergism
    case_cond <- character(length(CI))
    col <- character(length(CI))

    for (i in seq(length(CI)))
    {
        if (CI[i] <= 0.1) {
            case_cond[i] <- "Very Strong Synergism"
            col[i] <- "darkred"
        } else if (CI[i] > 3.3) {
            case_cond[i] <- "Strong Antagonism"
            col[i] <- "black"
        } else if (CI[i] > 0.1 & CI[i] <= 0.3) {
            case_cond[i] <- "Strong Synergism"
            col[i] <- "red"
        } else if (CI[i] > 0.3 & CI[i] <= 0.7) {
            case_cond[i] <- "Synergism"
            col[i] <- "orange"
        } else if (CI[i] > 0.7 & CI[i] <= 0.9) {
            case_cond[i] <- "Moderate Synergism"
            col[i] <- "yellow"
        } else if (CI[i] > 1.1 & CI[i] <= 1.45) {
            case_cond[i] <- "Moderate Antagonism"
            col[i] <- "lightgrey"
        } else if (CI[i] > 1.45 & CI[i] <= 3.3) {
            case_cond[i] <- "Antagonism"
            col[i] <- "darkgrey"
        } else if (CI[i] > 0.9 & CI[i] <= 1.1) {
            case_cond[i] <- "Aditivity"
            col[i] <- "green"
        }
    }

    #isobologram plot prep
        #possible ploted situations
    cases <- c("Very Strong Synergism", "Strong Synergism",
        "Synergism", "Moderate Synergism", "Aditivity",
        "Moderate Antagonism", "Antagonism",
        "Strong Antagonism")

        #corresponding colors
    cases_col <- c("darkred", "red",
        "orange", "yellow", "green",
        "lightgrey", "darkgrey", "black")

        #preparing data for plot
    CI_df <- data.frame("DX1" = doses1_c_norm, "DX2" = doses2_c_norm,
        "Decision" = case_cond, "Color" = col, stringsAsFactors = FALSE)
    labels_ci <- factor(cases[!is.na(match(cases, CI_df$Decision))])
    labels_col <- factor(cases_col[!is.na(match(cases,
        CI_df$Decision))])

    #isobologram plot
    p3 <- ggplot(data = CI_df, aes(x = doses2_c_norm, y = doses1_c_norm)) +
        geom_point(size = 3, colour = col) +
        xlim(0, 1) +
        ylim(0, 1) +
        theme_minimal() +
        ggtitle("Normalized Isobologram") +
        xlab("Normalized Dose 1") + ylab("Normalized Dose 2") +
        theme(axis.title.x = element_text(
            face = "bold", colour = "#990000", size = 15),
        axis.title.y = element_text(
            face = "bold", colour = "#990000", size = 15),
        axis.text.x = element_text(size = 12, vjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black",
            size = 0.5, linetype = "solid")) +
        geom_abline(intercept = 1, slope = -1, color = "red") +
        geom_text_repel(aes(label = CI_df$Decision),
            size = 2, col = CI_df$Color)
    if (save.files == TRUE) {
        ggsave(paste(MACSQuant@param.output$path,
            "/outputMQ/isobologram.png", sep = ""),
        width = 15.875, height = 15.875,
        units = "cm", plot = p3)

    }

    #preparing arguments for 3D combinations barplots
        #combo conditions index
    Condition <-
        seq_len(MACSQuant@param.experiment$number_of_conditions)[-c(ind_d1,
            ind_d2)]

    #results under df from
    MACSQuant@combination.index.df <- data.frame(combo_df, CI, doses1_c_norm,
        doses2_c_norm, CI_df, Condition, stringsAsFactors = FALSE)

    #preparing arguments for 3D combinations barplots
        #color correspondence
    col <- rep("white", MACSQuant@param.experiment$number_of_conditions)
    col[MACSQuant@combination.index.df$Condition] <-
        MACSQuant@combination.index.df$Color

    if (save.files == TRUE) {
        write.table(MACSQuant@combination.index.df,
            paste(MACSQuant@param.output$path,
                "/outputMQ/combination_index.txt ", sep = ""),
            sep = "\t")
    }

    # 3D combination barplots
    flav <- "percent"
    plt.labels <- MACSQuant@param.output$plt.labels
    p4 <- grid.arrange(barplot_data(MACSQuant,
        plt.col = col,
        plt.flavour = flav,
        plt.labels = plt.labels,
        plt.combo = TRUE,
        plt.3D.only = TRUE,
        ...))

    # p <- gridExtra::arrangeGrob(p3, p4, ncol = 2)

    if (save.files == TRUE) {
        ggsave(paste(MACSQuant@param.output$path,
            "/outputMQ/barplot_combination_index.png", sep = ""),
        width = 15.875, height = 15.875,
        units = "cm", plot = p4)
    }

    plot(p1)
    plot(p2)
    plot(p3)
    grid.arrange(p4)
    return(MACSQuant)
}
