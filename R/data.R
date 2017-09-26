#' Stature based on age (2y-20y).
#'
#' Stature value by percentiles based on sex and age (2 to 20 years).
#'
#' Contains L, M, and S parameters to determine new z-scores from:
#'
#' X = M * (1 + L*S*Z)^(1 / L), L != 0
#'
#' X = M * exp(S * Z), L == 0
#'
#' @format A data frame with 436 rows and 14 variables:
#' \describe{
#'   \item{Sex}{1 = male, 2 = female}
#'   \item{Agemos}{Age in months}
#'   \item{L}{Use to find new z-score values (see details below)}
#'   \item{M}{Use to find new z-score values (see details below)}
#'   \item{S}{Use to find new z-score values (see details below)}
#'   \item{P3}{Third percentile height in cm}
#'   \item{P5}{Fifth percentile height in cm}
#'   \item{P10}{Tenth percentile height in cm}
#'   \item{P25}{Twenty fifth percentile height in cm}
#'   \item{P50}{Fiftieth percentile height in cm}
#'   \item{P75}{Seventy fifth percentile height in cm}
#'   \item{P90}{Ninetieth percentile height in cm}
#'   \item{P95}{Ninety fifth percentile height in cm}
#'   \item{P97}{Ninety seventh percentile height in cm}
#' }
#' @source \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}
"statage"



#' Weight based on age (2y-20y).
#'
#' Weight values by percentiles based on sex and age (2 to 20 years).
#'
#' Contains L, M, and S parameters to determine new z-scores from:
#'
#' X = M * (1 + L*S*Z)^(1 / L), L != 0
#'
#' X = M * exp(S * Z), L == 0
#'
#' @format A data frame with 436 rows and 14 variables:
#' \describe{
#'   \item{Sex}{1 = male, 2 = female}
#'   \item{Agemos}{Age in months}
#'   \item{L}{Use to find new z-score values (see details below)}
#'   \item{M}{Use to find new z-score values (see details below)}
#'   \item{S}{Use to find new z-score values (see details below)}
#'   \item{P3}{Third percentile weight in kg}
#'   \item{P5}{Fifth percentile weight in kg}
#'   \item{P10}{Tenth percentile weight in kg}
#'   \item{P25}{Twenty fifth percentile weight in kg}
#'   \item{P50}{Fiftieth percentile weight in kg}
#'   \item{P75}{Seventy fifth percentile weight in kg}
#'   \item{P90}{Ninetieth percentile weight in kg}
#'   \item{P95}{Ninety fifth percentile weight in kg}
#'   \item{P97}{Ninety seventh percentile weight in kg}
#' }
#' @source \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}
"wtage"



#' Length based on age (0m-36m).
#'
#' Length value by percentiles based on sex and age (0 to 36 months).
#'
#' Contains L, M, and S parameters to determine new z-scores from:
#'
#' X = M * (1 + L*S*Z)^(1 / L), L != 0
#'
#' X = M * exp(S * Z), L == 0
#'
#' @format A data frame with 74 rows and 14 variables:
#' \describe{
#'   \item{Sex}{1 = male, 2 = female}
#'   \item{Agemos}{Age in months}
#'   \item{L}{Use to find new z-score values (see details below)}
#'   \item{M}{Use to find new z-score values (see details below)}
#'   \item{S}{Use to find new z-score values (see details below)}
#'   \item{P3}{Third percentile length in cm}
#'   \item{P5}{Fifth percentile length in cm}
#'   \item{P10}{Tenth percentile length in cm}
#'   \item{P25}{Twenty fifth percentile length in cm}
#'   \item{P50}{Fiftieth percentile length in cm}
#'   \item{P75}{Seventy fifth percentile length in cm}
#'   \item{P90}{Ninetieth percentile length in cm}
#'   \item{P95}{Ninety fifth percentile length in cm}
#'   \item{P97}{Ninety seventh percentile length in cm}
#' }
#' @source \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}
"lenageinf"



#' Weight based on age (0m-36m).
#'
#' Weight values by percentiles based on sex and age (0 to 36 months).
#'
#' Contains L, M, and S parameters to determine new z-scores from:
#'
#' X = M * (1 + L*S*Z)^(1 / L), L != 0
#'
#' X = M * exp(S * Z), L == 0
#'
#' @format A data frame with 76 rows and 14 variables:
#' \describe{
#'   \item{Sex}{1 = male, 2 = female}
#'   \item{Agemos}{Age in months}
#'   \item{L}{Use to find new z-score values (see details below)}
#'   \item{M}{Use to find new z-score values (see details below)}
#'   \item{S}{Use to find new z-score values (see details below)}
#'   \item{P3}{Third percentile weight in kg}
#'   \item{P5}{Fifth percentile weight in kg}
#'   \item{P10}{Tenth percentile weight in kg}
#'   \item{P25}{Twenty fifth percentile weight in kg}
#'   \item{P50}{Fiftieth percentile weight in kg}
#'   \item{P75}{Seventy fifth percentile weight in kg}
#'   \item{P90}{Ninetieth percentile weight in kg}
#'   \item{P95}{Ninety fifth percentile weight in kg}
#'   \item{P97}{Ninety seventh percentile weight in kg}
#' }
#' @source \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}
"wtageinf"



#' Height and Weight example dataset.
#'
#' Ages between 0 and 20 years.
#'
#' @format A data frame with 399 rows and 6 variables:
#' \describe{
#'   \item{TIME}{Time that the height and weight were recorded.}
#'   \item{HT}{Height in cm}
#'   \item{WT}{Weight in kg}
#'   \item{BIRTH_DT}{Birth date}
#'   \item{SEX}{0 is female, 1 is male}
#'   \item{AGE_Y}{Age in years}
#' }
"htwt_example"


