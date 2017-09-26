#' Height and Weight anomaly detection.
#'
#' Uses CDC height and weight expected values to determine anomalies
#' in height and weight values for cleaning purposes.
#'
#' See \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}
#' for data sources used to detect anomalies.
#'
#' Datasets are included in package as statage, lenageinf, wtage,
#' and wtageinf.
#'
#' @param df Data frame containing specific columns:
#'
#' \strong{HT:} Patient height (cm).
#'
#' \strong{WT:} Patient weight (kg).
#'
#' \strong{SEX:} Patient sex 0 = female, 1 = female.
#'
#' either
#'
#' \strong{TIME:} Dates when corresponding height and weight values
#' were collected. Must be R datetime object.
#'
#' \strong{BIRTH_DT:} Birth date of patient. Must be R date or
#' datetime object.
#'
#' or
#'
#' \strong{AGE_M:} Age of patient in months.
#'
#' @param lower_percentile Integer value representing the lower
#' percentile, upper percentile will automatically be calculated.
#'
#' Options: 3,5,10,25
#'
#' Example: lower_percentile = 3. Normal will be defined as inbetween
#' fith and ninety fifth percentile (3-97).
#'
#' lower_percentile = 5, (5-95).
#'
#' lower_percentile = 10, (10-90).
#'
#' lower_percentile = 25, (25-75).
#'
#' @return Returns a data frame identifying height weight anomalies
#' with specific colums:
#'
#' \strong{HT_FLAG:} Numerical value. >0 signifying distance from
#' normal if height is abnormal. 0 if there is no abnormality in
#' height detected.
#'
#' \strong{WT_FLAG:} Numerical value. >0 signifying distance from
#' normal if weight is abnormal. 0 if there is no abnormality in
#' weight detected.
#'
#' \strong{AGE_Y:} Age of patient in years.
#'
#' @keywords height, weight, cleaning, clean
#' @examples
#' clean_htwt(htwt_example, 3)
#' @export
clean_htwt <- function(df, lower_percentile) {
  #--------------------------------------------------------------------------------
  # ESSENTIAL FUNCTIONS
  #--------------------------------------------------------------------------------

  # find_set_anomalies
  #
  # find anomalies for a specific sex/age group
  # input htwt_set = set to find abnormalities
  # input sex = 0 female, 1 male
  # input age_set = "infant" or "child"
  find_set_anomalies <- function(htwt_set, sex, age_set) {
    if (nrow(htwt_set) > 0) {
      ha <- switch(age_set,
                   "infant" = lenageinf,
                   "child" = statage)
      wa <- switch(age_set,
                   "infant" = wtageinf,
                   "child" = wtage)
      htwt_set$HT_FLAG <- compare_spline(x = htwt_set$AGE,
                                         y = htwt_set$HT,
                                         d = get_spline_data(ha, sex))
      htwt_set$WT_FLAG <- compare_spline(x = htwt_set$AGE,
                                         y = htwt_set$WT,
                                         d = get_spline_data(wa, sex))
    }
    return(htwt_set)
  }

  # get_spline_data
  #
  # function to prepare information to find cubic spline
  # input d = dataset to pull from
  # input sex = 0 female, 1 male
  # input x = column to use for x values
  get_spline_data <- function(d, sex) {
    if (sex == 0) {
      d <- dplyr::filter(d, Sex == 2)
    } else {
      d <- dplyr::filter(d, Sex == 1)
    }
    d <- dplyr::select(d,
                       x = Agemos,
                       low = percentile["low"],
                       high = percentile["high"])
    return(d)
  }

  # compare_spline
  #
  # find anomalies for a specific spline set
  # input x = vector values to use finding percentile
  # input y = vector values to compare to find abnormality
  # input spline_data = data frame, and spline data frame
  #    with x, low, high values
  compare_spline <- function(x, y, d) {
    low_spline <- splinefun(x = d$x,
                            y = d$low,
                            method = "natural")
    high_spline <- splinefun(x = d$x,
                             y = d$high,
                             method = "natural")
    low <- y - low_spline(x)
    low <- low * (low < 0)
    high <- y - high_spline(x)
    high <- high * (high > 0)
    return(minmax(low, high))
  }

  # minmax
  #
  # finds minimum or maximum pairwise values for two vectors and combines
  # them. Only use if corresponding values are expected to be either both
  #   positive or both negative
  minmax <- function(v1, v2) {
    return(pmin(v1 * (v1 < 0),
                v2 * (v2 < 0)) +
             pmax(v1 * (v1 > 0),
                  v2 * (v2 > 0)))
  }


  #--------------------------------------------------------------------------------
  # PROCESS INPUT
  #--------------------------------------------------------------------------------
  df <- dplyr::mutate(df, ROW = row_number())

  # prepare percentile information
  percentile <- switch(as.character(lower_percentile),
                       "3" = c(low = "P3", high = "P97"),
                       "5" = c(low = "P5", high = "P95"),
                       "10" = c(low = "P10", high = "P90"),
                       "25" = c(low = "P25", high = "P75"))

  # prepare htwt tibble which will be used for calculations
  if ("AGE_M" %in% colnames(df)) {
    age <- df$AGE_M
  } else {
    time <- as.numeric(readr::parse_datetime(df$TIME)) / (24 * 3600 * 30.41667)
    birth_dt <- as.numeric(readr::parse_datetime(df$BIRTH_DT)) / (24 * 3600 * 30.41667)
    age <- time - birth_dt
  }
  htwt <- tibble::tibble(AGE = age,
                         HT = df$HT,
                         WT = df$WT,
                         SEX = df$SEX,
                         ROW = df$ROW
                         )
  htwt <- dplyr::mutate(htwt,
                        HT_FLAG = 0,
                        WT_FLAG = 0)


  #--------------------------------------------------------------------------------
  # FIND ANOMALIES
  #--------------------------------------------------------------------------------
  for (i in 0:1) {
    # 0-36 months
    htwt_set <- dplyr::filter(htwt, AGE <= 36 & SEX == i)
    htwt_set <- find_set_anomalies(htwt_set, i, "infant")
    htwt$HT_FLAG[htwt_set$ROW] <- minmax(htwt$HT_FLAG[htwt_set$ROW], htwt_set$HT_FLAG)
    htwt$WT_FLAG[htwt_set$ROW] <- minmax(htwt$WT_FLAG[htwt_set$ROW], htwt_set$WT_FLAG)

    # 24-240 months
    htwt_set <- dplyr::filter(htwt, AGE > 24 & SEX == i)
    htwt_set <- find_set_anomalies(htwt_set, i, "child")
    htwt$HT_FLAG[htwt_set$ROW] <- minmax(htwt$HT_FLAG[htwt_set$ROW], htwt_set$HT_FLAG)
    htwt$WT_FLAG[htwt_set$ROW] <- minmax(htwt$WT_FLAG[htwt_set$ROW], htwt_set$WT_FLAG)
  }

  # > 240 months
  htwt_set <- dplyr::filter(htwt, AGE > 240)
  htwt$HT_FLAG[htwt_set$ROW] <- NA
  htwt$WT_FLAG[htwt_set$ROW] <- NA

  #--------------------------------------------------------------------------------
  # GENERATE OUTPUT
  #--------------------------------------------------------------------------------
  df <- dplyr::mutate(df,
              HT_FLAG = htwt$HT_FLAG,
              WT_FLAG = htwt$WT_FLAG,
              AGE_Y = htwt$AGE / 12)
  df <- dplyr::select(df, -ROW)

  return(df)

}


