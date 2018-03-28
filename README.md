# pkpd R Package

A package used for assissting in pkpd analysis of data.

## Installation

R and the devtools package is required to install the pkpd package.

To install devtools on R run:

```
install.packages("devtools")
```

After devtools is installed, to install the pkpd package on R run:

```
devtools::install_github("jadonwagstaff/pkpd")
```

## Functions

*viz_htwt* - Provides a visual for percentile lines for height and weight ages 0 to 20.

*clean_htwt* - Determines z-score for distributions of height and weight for age and sex.

*individual_plots* - Plots concentration time curves for observed and predicted values.

### 

## Datasets

*htwt_example* - Example dataset to use on *viz_htwt* and *clean_htwt*.

*pred_example* - Example dataset to use on *individual_plots*.

## Author
Jadon Wagstaff

## Licence
MIT


