# Jadon Wagstaff
# University of Utah Health
# Pediatrics
# Clinical Pharmacology
# 2017

#' Height and Weight anomaly detection.
#'
#' Uses CDC height and weight expected values to determine anomalies
#' in height and weight values for cleaning purposes.
#'
#' See \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}
#' for data sources used to detect anomalies. Z-scores are calculated
#' based on LMS parameters as described in [1]. If subject is greater
#' than 20 years old, height and weight will be compared to the distribution
#' for a 20 year old.
#'
#' If any dataframe input column has a different name, define
#' that columns name in the input parameters (See Examples).
#' See http://r4ds.had.co.nz chapters 11.4, 11.5 and 16 for creating
#' date or datetime R objects.
#'
#' 1. Flegal KM, Cole TJ. Construction of LMS parameters for the Centers
#' for Disease Control and prevention 2000 growth chart. National health
#' statistics reports; no 63. Hyattsville, MD: National Center for Health
#' Statistics. 2013.
#'
#' @param df Data frame containing weight, height, the time weight and
#' height were collected, sex, and eighter birth date or age in months.
#'
#' @param HEIGHT Column name for patient height (cm).
#'
#' @param WEIGHT Column name for patient weight (kg).
#'
#' @param SEX Column name for patient sex 0 = female, 1 = male.
#'
#' @param TIME Column name for dates when corresponding height and
#' weight values were collected. Must be R datetime object.
#'
#' @param BIRTH_DT Column name for birth date of patient. Must be R date or
#' datetime object.
#'
#' @param AGE_M Column name for age of patient in months.
#'
#' @return Returns a data frame identifying height weight anomalies
#' with specific colums:
#'
#' \strong{HT_Z:} Z score on expected distribution of height for
#' age and sex.
#'
#' \strong{WT_Z:} Z score on expected distribution of Weight for
#' age and sex.
#'
#' @keywords height, weight, cleaning, clean
#' @examples
#' htwt <- clean_htwt(htwt_example, HEIGHT = HT, WEIGHT = WT)
#' htwt
#' # filter out results for outside of 5th and 95th percentile
#' htwt <- dplyr::filter(htwt, abs(HT_Z) > 1.96 | abs(WT_Z) > 1.96)
#' htwt
#' @export
clean_htwt <- function(df, HEIGHT = HEIGHT, WEIGHT = WEIGHT, SEX = SEX,
                       TIME = TIME, BIRTH_DT = BIRTH_DT, AGE_M = AGE_M) {
  #--------------------------------------------------------------------------------
  # ESSENTIAL FUNCTIONS
  #--------------------------------------------------------------------------------

  # score
  #
  # find z-score based on ht/wt, sex, and age group
  score <- function(x, age, set) {
    l <- splines[[set]][["L"]](age)
    m <- splines[[set]][["M"]](age)
    s <- splines[[set]][["S"]](age)

    output <- rep(0, length(x))
    output[l != 0] <- (x^l - m^l) / (m^l * l * s)
    output[l == 0] <- log(x / m) / s
    return(output)
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

  # prepare column names
  HEIGHT <- deparse(substitute(HEIGHT))
  WEIGHT <- deparse(substitute(WEIGHT))
  SEX <- deparse(substitute(SEX))
  TIME <- deparse(substitute(TIME))
  BIRTH_DT <- deparse(substitute(BIRTH_DT))
  AGE_M <- deparse(substitute(AGE_M))

  # prepare htwt tibble which will be used for calculations
  if (AGE_M %in% colnames(df)) {
    age <- df[[AGE_M]]
  } else {
    time <- as.numeric(readr::parse_datetime(df[[TIME]])) / (24 * 3600 * 30.41667)
    birth_dt <- as.numeric(readr::parse_datetime(df[[BIRTH_DT]])) / (24 * 3600 * 30.41667)
    age <- time - birth_dt
  }
  age[age > 240] <- 240
  htwt <- tibble::tibble(AGE = age,
                         HEIGHT = df[[HEIGHT]],
                         WEIGHT = df[[WEIGHT]],
                         SEX = df[[SEX]]
  )
  htwt <- dplyr::mutate(htwt,
                        ROW = row_number(),
                        HT_Z = 0,
                        WT_Z = 0)


  #--------------------------------------------------------------------------------
  # FIND ANOMALIES (Z-SCORES)
  #--------------------------------------------------------------------------------
  # 0-36 months female
  htwt_set <- dplyr::filter(htwt, AGE <= 36 & SEX == 0)
  htwt_set$WT_Z <- score(htwt_set$WEIGHT, htwt_set$AGE, "WFI")
  htwt_set$HT_Z <- score(htwt_set$HEIGHT, htwt_set$AGE, "HFI")
  htwt$HT_Z[htwt_set$ROW] <- minmax(htwt$HT_Z[htwt_set$ROW], htwt_set$HT_Z)
  htwt$WT_Z[htwt_set$ROW] <- minmax(htwt$WT_Z[htwt_set$ROW], htwt_set$WT_Z)

  # 0-36 months male
  htwt_set <- dplyr::filter(htwt, AGE <= 36 & SEX == 1)
  htwt_set$WT_Z <- score(htwt_set$WEIGHT, htwt_set$AGE, "WMI")
  htwt_set$HT_Z <- score(htwt_set$HEIGHT, htwt_set$AGE, "HMI")
  htwt$HT_Z[htwt_set$ROW] <- minmax(htwt$HT_Z[htwt_set$ROW], htwt_set$HT_Z)
  htwt$WT_Z[htwt_set$ROW] <- minmax(htwt$WT_Z[htwt_set$ROW], htwt_set$WT_Z)

  # 24-240 months female
  htwt_set <- dplyr::filter(htwt, AGE > 24 & age <= 240 & SEX == 0)
  htwt_set$WT_Z <- score(htwt_set$WEIGHT, htwt_set$AGE, "WF")
  htwt_set$HT_Z <- score(htwt_set$HEIGHT, htwt_set$AGE, "HF")
  htwt$HT_Z[htwt_set$ROW] <- minmax(htwt$HT_Z[htwt_set$ROW], htwt_set$HT_Z)
  htwt$WT_Z[htwt_set$ROW] <- minmax(htwt$WT_Z[htwt_set$ROW], htwt_set$WT_Z)

  # 24-240 months male
  htwt_set <- dplyr::filter(htwt, AGE > 24 & age <= 240 & SEX == 1)
  htwt_set$WT_Z <- score(htwt_set$WEIGHT, htwt_set$AGE, "WM")
  htwt_set$HT_Z <- score(htwt_set$HEIGHT, htwt_set$AGE, "HM")
  htwt$HT_Z[htwt_set$ROW] <- minmax(htwt$HT_Z[htwt_set$ROW], htwt_set$HT_Z)
  htwt$WT_Z[htwt_set$ROW] <- minmax(htwt$WT_Z[htwt_set$ROW], htwt_set$WT_Z)

  #--------------------------------------------------------------------------------
  # GENERATE OUTPUT
  #--------------------------------------------------------------------------------
  htwt$HT_Z[is.na(htwt$AGE)] <- NA
  htwt$HT_Z[is.na(htwt$SEX)] <- NA
  htwt$WT_Z[is.na(htwt$AGE)] <- NA
  htwt$WT_Z[is.na(htwt$SEX)] <- NA
  df <- dplyr::mutate(df,
                      HT_Z = htwt$HT_Z,
                      WT_Z = htwt$WT_Z)

  return(df)

}


