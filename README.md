
# HeartR package

## Description

**HeartR** is an R package designed for educational purposes to explore,
summarise and simulate heart disease related data. Informative plots may
be created to further analyse the distribution of variables and
correlation between features. The package provides several functions and
one S3 method:

1.  `plot_heart`: a function that provides multiple visualisation
    options
2.  `simulation`: for generating a synthetic heart disease dataset.
    However it may be used for other datasets with binary outcomes
3.  `summ_heartr`: for creating a list of summary statistics that will
    be used further in the `print.summ_heartr` method
4.  `plot_summary`: for generating interactive plots of summary
    statistics
5.  An S3 `print.summ_heartr` method: for providing an appealing
    customisable table for the output of the summ_heartr function

## Installation

You can install the development version of HeartR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("faridat45/heartR")
```

## Example

This is a basic example which shows you how to generate synthetic data,
adjusting the categorical and numeric variables included and number of
observations:

``` r
library(HeartR)

sim_data <- simulation(
  data = heart_dat,
  n = 20,
  contVars = c("age", "cholesterol","restingBP"),
  catVars = c("sex","chestPT","exerAngina")
)
utils::head(sim_data)
#>   age cholesterol restingBP sex chestPT exerAngina target
#> 1  62         255       116   0       2          0      0
#> 2  51         289       142   0       4          0      0
#> 3  38         306       109   1       2          0      0
#> 4  58         213       127   1       4          0      1
#> 5  56         314       134   0       3          0      0
#> 6  51         181       126   1       4          1      1
```

From this dataset, plots can be generated using the `plot_heart`
function:

<img src="man/figures/README-pressure-1.png" width="100%" />

A more detailed introduction is provided in the heartR vignette
