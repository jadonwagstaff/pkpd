# Jadon Wagstaff
# University of Utah Health
# Pediatrics
# Clinical Pharmacology
# 2018

#' Individual prediction plots.
#'
#' Plots individual observed vs predicted values as a diagnostic
#' tool for pk/pd modeling.
#'
#' If any dataframe input column has a different name, define
#' that columns name in the input parameters (See Examples).
#' See http://r4ds.had.co.nz chapters 11.4, 11.5 and 16 for creating
#' date or datetime R objects.
#'
#' @param df Data frame containing ID, height, the time weight and
#' height were collected, sex, and eighter birth date or age in months.
#'
#' @param filename Name of file to save pdf output.
#'
#' @param ID Column name for unique individual identifier.
#'
#' @param TIME Column name for datetime or time when concentration
#' values were collected.
#'
#' @param DV Column name for observed concentration values.
#'
#' @param PRED Column name for predicted concentration values.
#'
#' @return A pdf containing observed vs predicted and time vs observed
#' and predicted plots for each ID. The pdf will be saved
#' as 'filename' in the working directory.
#'
#' @keywords prediction, plot, individual
#' @examples
#' individual_plots(pred_example, "pred_example_plots.pdf")
#' # pred_example_plots.pdf should now be in your working directory
#' @export
individual_plots <- function(df, filename, ID = ID, TIME = TIME, DV = DV, PRED = PRED) {

  #--------------------------------------------------------------------------------
  # PROCESS INPUT
  #--------------------------------------------------------------------------------

  # prepare column names
  ID <- deparse(substitute(ID))
  TIME <- deparse(substitute(TIME))
  DV <- deparse(substitute(DV))
  PRED <- deparse(substitute(PRED))

  # define variables
  first_lims <- c(min(df$PRED, df$DV, 0), max(df$PRED, df$DV))
  second_xlims <- c(min(df$TIME, 0), max(df$TIME))
  second_ylims <- c(min(df$PRED, df$DV, 0), max(df$PRED, df$DV))
  indices <- dplyr::group_indices(df, ID)
  plots <- list()

  # open File
  pdf(filename)

  #--------------------------------------------------------------------------------
  # GENERATE PLOTS
  #--------------------------------------------------------------------------------
  for (i in seq(1, length(unique(indices)), 2)) {
    for (j in 0:1) {
      # Prediction vs Observed
      plots[[j*2 + 1]] <- ggplot2::ggplot(df[indices == i + j,], ggplot2::aes(PRED, DV)) +
        ggthemes::theme_tufte() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5), plot.margin = grid::unit(c(.5, .5, .5, .5), 'cm')) +
        ggplot2::ggtitle(unique(df[indices == i + j,]$ID)) +
        ggplot2::xlab("Predicted Response") +
        ggplot2::ylab("Observed Response") +
        ggplot2::xlim(first_lims) +
        ggplot2::ylim(first_lims) +
        ggplot2::annotate('segment', x = first_lims[1], y = first_lims[1], xend = first_lims[2], yend = first_lims[2], color = '#4682B4',alpha = .7) +
        ggplot2::geom_point(alpha = .6, size = 1.25)
      # Response vs Time
      plots[[j*2 + 2]] <- ggplot2::ggplot(df[indices == i + j,]) +
        ggthemes::theme_tufte() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5), plot.margin = grid::unit(c(.5, .5, .5, .5), 'cm')) +
        ggplot2::ggtitle(unique(df[indices == i + j,]$ID)) +
        ggplot2::xlab("Time") +
        ggplot2::ylab("Response (Predicted: Red)") +
        ggplot2::geom_point(ggplot2::aes(TIME, PRED), size = 1.25, color = '#b22222') +
        ggplot2::geom_point(ggplot2::aes(TIME, DV), alpha = .6, size = 1.25) +
        ggplot2::xlim(second_xlims) +
        ggplot2::ylim(second_ylims)
    }

    if (i ==  length(unique(indices))) {
      plots <- plots[1:2]
    }

    # Combine into one page
    gridExtra::grid.arrange(
      grobs = plots,
      nrow = 2,
      ncol = 2,
      vp = grid::viewport(width=.93, height=.93))
  }

  # Close File
  dev.off()

}


