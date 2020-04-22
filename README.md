
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metabolyseR

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build
status](https://travis-ci.org/jasenfinch/metabolyseR.svg?branch=master)](https://travis-ci.org/jasenfinch/metabolyseR)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jasenfinch/metabolyseR?branch=master&svg=true)](https://ci.appveyor.com/project/jasenfinch/metabolyseR)
[![codecov](https://codecov.io/gh/jasenfinch/metabolyseR/branch/master/graph/badge.svg)](https://codecov.io/gh/jasenfinch/metabolyseR/branch/master)
[![license](https://img.shields.io/badge/license-GNU%20GPL%20v3.0-blue.svg)](https://github.com/jasenfinch/metabolyseR/blob/master/DESCRIPTION)

Methods for pre-treatment, classification, feature selection and
correlation analyses of metabolomics
data

### Installation

``` r
devtools::install_github('jasenfinch/metabolyseR',build_vignettes = TRUE)
```

### Vignettes

Available vignettes for package usage can be found using the following:

``` r
browseVignettes('metabolyseR')
```

### Quick start

``` r
library(metabolyseR)
library(metaboData)
library(magrittr)
```

``` r
d <- analysisData(abr1$neg,abr1$fact)
```

``` r
print(d)
#> 
#> Analysis Data object containing:
#> 
#> Samples: 120 
#> Features: 2000 
#> Info: 9
```

``` r
d <- d %>%
  occupancyMaximum(cls = 'day') %>%
  transformTICnorm()
```

``` r
plotPCA(d,cls = 'day')
```

<img src="man/figures/README-pca-1.png" style="display: block; margin: auto;" />

``` r
plotSupervisedRF(d,cls = 'day')
```

<img src="man/figures/README-supervised_RF-1.png" style="display: block; margin: auto;" />

``` r
rf_results <- d %>%
  anova(cls = 'day')
```

``` r
plotExplanatoryHeatmap(rf_results,
                       threshold = 0.05,
                       featureNames = FALSE)
```

<img src="man/figures/README-rf_heatmap-1.png" style="display: block; margin: auto;" />

``` r
correlation <- d %>%
  correlations()
```

``` r
correlation
#> # A tibble: 61,592 x 6
#>    Feature1 Feature2 log2IntensityRatio     r        p     n
#>    <chr>    <chr>                 <dbl> <dbl>    <dbl> <int>
#>  1 N113     N117                 1.61   0.569 1.85e- 7   109
#>  2 N115     N117                 0.0958 0.666 1.11e- 5    60
#>  3 N115     N118                 2.77   0.602 1.68e- 2    46
#>  4 N117     N118                 2.68   0.520 1.16e- 3    81
#>  5 N113     N119                 1.18   0.635 4.45e-11   115
#>  6 N115     N119                -0.332  0.632 1.39e- 4    59
#>  7 N117     N119                -0.427  0.838 0.         106
#>  8 N127     N129                 4.30   0.606 2.43e- 5    73
#>  9 N115     N130                 3.07   0.762 1.10e- 3    31
#> 10 N117     N130                 2.97   0.642 1.07e- 2    41
#> # … with 61,582 more rows
```
