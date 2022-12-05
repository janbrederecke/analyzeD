# analyzeD <img src="man/figures/analyzeD.jpeg" width="200" height="200" align="right" />

R package with useful functions for data analysis and reporting.

## Overview

### regression_ family
**Easily compute different kinds of regression**

-   `regression_lin()` Calculates linear regression models using `ln()`.

-   `regression_log()` Calculates logistic regression models using `glm()`.

-   `regression_cox()` Calculates Cox regression models using
`survival::coxph()`.

### random_forest_ family
**Easily compute different kinds of random forests**

-   `random_forest_regr()` Calculates random forest models for regression
problems using `ranger::ranger()`.

-   `random_forest_class()` Calculates random forest models for classification
problems using `ranger::ranger()`.

-   `random_forest_surv()` Calculates random forest models for survival
problems using `ranger::ranger()`.

### create_table family
**Turn the output of regression_ and random_forest_ functions into HTML output**

-   `create_table()` Creates HTML tables from regression_ and random_forest_ results.

## Installation

You can get the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("janbrederecke/analyzeD")
```

## Acknowledgement

The logo was designed by [Tim Brederecke](https://www.instagram.com/timbrederecke/).

## Contact
Please submit feedback and suggestions through Github's [issues](https://github.com/janbrederecke/prepareD/issues) facility.

