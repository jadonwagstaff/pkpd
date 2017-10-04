#' Height and Weight graphing function.
#'
#' Uses CDC height and weight expected values to add percentile lines
#' to height/age or weight/age graphs.
#'
#' See \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}
#' for information on the data used to make percentile lines.
#'
#' @param p a ggplot object.
#' @param category "WT" for a weight/age graph and "HT" for a
#' height/age graph.
#' @param sex 0 for female, 1 for male
#' @param inf_only TRUE if all ages are less than three years old.
#' Default is FALSE.
#' @param percentiles Which percentiles to include. Vector values
#' containing any of "P3", "P5", "P10", "P25", "P50", "P75", "P90",
#' "P95", or "P97". Default is c("P3", "P50", "P97")
#' percentile, upper percentile will automatically be calculated.
#' @return Returns the ggplot object with lines representing the
#' cdc growth-chart percentiles chosen.
#' @keywords height, weight, ggplot, ggplot2, plot, graph, percentiles
#' @examples
#' library(tidyverse)
#' # p1 plots age and weight for females
#' p1 <- ggplot(data = filter(htwt_example, SEX == 0)) +
#'   aes(y = WT, x = AGE_Y) +
#'   geom_point(alpha = .7) +
#'   theme_minimal()
#' p1 <- viz_htwt(p = p1,
#'                category = "WT",
#'                sex = 0)
#' p1
#' # p2 plots age and weight for male infants
#' p2 <- ggplot(data = filter(htwt_example, SEX == 1 & AGE_Y < 3)) +
#'   aes(y = WT, x = AGE_Y) +
#'   geom_point(alpha = .7) +
#'   theme_minimal()
#' p2 <- viz_htwt(p = p2,
#'                category = "WT",
#'                sex = 1,
#'                inf_only = TRUE,
#'                percentiles = c("P10", "P90"))
#' p2
#' @export
viz_htwt <- function(p, category, sex, inf_only = FALSE, percentiles = c("P3", "P50", "P97")) {

  #--------------------------------------------------------------------------------
  # ESSENTIAL FUNCTIONS
  #--------------------------------------------------------------------------------

  # get_spline_data
  #
  # used to obtain data required to build spline
  get_spline_data <- function(d, sex, percentile) {
    if (sex == 0) {
      d <- dplyr::filter(d, Sex == 2)
    } else {
      d <- dplyr::filter(d, Sex == 1)
    }
    d <- dplyr::select(d,
                       x = Agemos,
                       y = percentile)
    d$x <- d$x / 12
    return(d)
  }

  #--------------------------------------------------------------------------------
  # PROCESS INPUT
  #--------------------------------------------------------------------------------

  # pick color
  if (sex == 0) {
    sex_color <- "#ef8a62"
  } else {
    sex_color <- "#67a9cf"
  }

  # pick data
  if (category == "HT") {
    child <- statage
    infant <- lenageinf
  } else if (category == "WT") {
    child <- wtage
    infant <- wtageinf
  }

  #--------------------------------------------------------------------------------
  # ADD SPLINE LAYERS
  #--------------------------------------------------------------------------------
  for (i in seq_along(percentiles)) {
    spline_data <- get_spline_data(infant, sex, percentiles[i])
    spline <- splinefun(x = spline_data$x,
                        y = spline_data$y,
                        method = "natural")

    p <- p + ggplot2::stat_function(fun = spline,
                                    xlim = c(0,3),
                                    color = sex_color,
                                    size = 1)

    if (inf_only == TRUE) {
      p <- p + ggplot2::annotate("text",
                                 label = percentiles[i],
                                 x = 3,
                                 y = spline(3),
                                 color = sex_color,
                                 hjust = 0)
    } else {
      spline_data <- get_spline_data(child, sex, percentiles[i])
      spline <- splinefun(x = spline_data$x,
                          y = spline_data$y,
                          method = "natural")

      p <- p + ggplot2::stat_function(fun = spline,
                                      xlim = c(2,20),
                                      color = sex_color,
                                      size = 1) +
               ggplot2::annotate("text",
                                 label = percentiles[i],
                                 x = 20,
                                 y = spline(20),
                                 color = sex_color,
                                 hjust = 0)
    }
  }

  # set x boundaries
  if (inf_only == TRUE) {
    p <- p + ggplot2::xlim(0, 4)
  } else {
    p <- p + ggplot2::xlim(0, 23)
  }
  return(p)
}


