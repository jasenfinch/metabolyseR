# metabolyseR 0.15.4

* Fixed various tidyverse warnings.

* Fixed an error when calculating the mds dimensions for multiple class comparisons with differing numbers of observations.

* Added the [`transformPercent()`](https://jasenfinch.github.io/metabolyseR/reference/transform.html) method for the [`AnalysisData`](https://jasenfinch.github.io/metabolyseR/reference/AnalysisData-class.html) S4 class to scale as a percentage of feature maximum intensity.

* Feature intensities are now displayed as relative percent intensities in heat maps generated by [`plotExplanatoryHeatmap()`](https://jasenfinch.github.io/metabolyseR/reference/plotExplanatoryHeatmap.html).

* Reduced the gap between the dendrogram and heat map in outputs of [`plotExplanatoryHeatmap()`](https://jasenfinch.github.io/metabolyseR/reference/plotExplanatoryHeatmap.html).

# metabolyseR 0.15.3

* Fixed the margin value displayed in plots from [`plotSupervisedRF()`](https://jasenfinch.github.io/metabolyseR/reference/plotSupervisedRF.html)

* The [`plotExplanatoryHeatmap()`](https://jasenfinch.github.io/metabolyseR/reference/plotExplanatoryHeatmap.html) method for the [`Analysis`](https://jasenfinch.github.io/metabolyseR/reference/Analysis-class.html) S4 class now returns a warning and skips plotting if an error is encountered whilst trying to plot a heat map.

# metabolyseR 0.15.2

* Added the argument `refactor` to the method [`transformTICnorm()`](https://jasenfinch.github.io/metabolyseR/reference/transform.html) to enable the feature intensities of total ion count (TIC) normalised data to be refactored back to a range consistent with the original data by multiplying the normalised values by the median TIC.

* Removed the permutation cap when the `perm` argument of [`randomForest()`](https://jasenfinch.github.io/metabolyseR/reference/randomForest.html) is less than the total number of permutations possible.

# metabolyseR 0.15.1

* The class occupancy methods now throw a helpful error message if no features are available on which to compute class occupancy.

* Fixed a bug where it was not possible to customize the order of class labels in the legend of [`plotLDA()`](https://jasenfinch.github.io/metabolyseR/reference/plotLDA.html).

# metabolyseR 0.15.0

* It is now possible to specify multiple `cls` arguments to the [aggregation methods](https://jasenfinch.github.io/metabolyseR/reference/aggregate.html).

* Correlation thresholds are now available for both coefficient and total number using the `minCoef` and `maxCor` arguments in the [`correlations()`](https://jasenfinch.github.io/metabolyseR/reference/correlations.html) method.

* Added the [`predictions()`]() accessor method for the [`RandomForest`](https://jasenfinch.github.io/metabolyseR/reference/RandomForest-class.html) S4 class to enable the retrieval of the out of bag model response predictions.

* The [occupancy filtering methods](https://jasenfinch.github.io/metabolyseR/reference/occupancyFilter.html) now error if the value supplied to argument `occupancy` is non-numeric.

* Memory usage and performance improvements for the [`randomForest()`](https://jasenfinch.github.io/metabolyseR/reference/randomForest.html) method.

* Added [`type()`](https://jasenfinch.github.io/metabolyseR/reference/modelling-accessors.html) and [`response()`](https://jasenfinch.github.io/metabolyseR/reference/modelling-accessors.html) methods for the [`Univariate`](https://jasenfinch.github.io/metabolyseR/reference/Univariate-class.html) S4 class.

* [`plotLDA()`](https://jasenfinch.github.io/metabolyseR/reference/plotLDA.html) now returns a warning and skips plotting if an error is encountered during PC-LDA.

* The value `pre-treated` is now the default for argument `type` in the [`Analysis`](https://jasenfinch.github.io/metabolyseR/reference/Analysis-class.html) S4 class [accessor methods](https://jasenfinch.github.io/metabolyseR/reference/analysis-accessors.html).

* Added the `value` argument to the [`explanatoryFeatures()`](https://jasenfinch.github.io/metabolyseR/reference/modelling-accessors.html) method to allow the specification of on which importance value to apply the specified `threshold`.

* The specified `cls` argument is now correctly displayed on the x-axis title of the resulting plots from the [`plotFeature()`](https://jasenfinch.github.io/metabolyseR/reference/plotFeature.html) method.

# metabolyseR 0.14.10

* Added the method [`predict()`](https://jasenfinch.github.io/metabolyseR/reference/predict.html) for the [`RandomForest`](https://jasenfinch.github.io/metabolyseR/reference/RandomForest-class.html) S4 class to predict model response values.

* Added the method [`mtry()`](https://jasenfinch.github.io/metabolyseR/reference/modelling-accessors.html) for the [`AnalysisData`](https://jasenfinch.github.io/metabolyseR/reference/AnalysisData-class.html) S4 class to return the default `mtry` random forest parameter for a given response variable.

* Added the method [`tune()`]() for the [`AnalysisData`](https://jasenfinch.github.io/metabolyseR/reference/tune.html) S4 class to tune the random forest parameters `mtry` and `ntree` for a given response variable.

# metabolyseR 0.14.9

* Suppressed name repair console message encountered during random forest permutation testing.

* Added the [`proximity()`](https://jasenfinch.github.io/metabolyseR/reference/modelling-accessors.html) method for extracting sample proximities from the [`RandomForest`](https://jasenfinch.github.io/metabolyseR/reference/RandomForest-class.html) S4 class.

* Added the [`mds()`](https://jasenfinch.github.io/metabolyseR/reference/mds.html) method to perform multidimensional scaling on sample proximities from the [`RandomForest`](https://jasenfinch.github.io/metabolyseR/reference/RandomForest-class.html) S4 class.

* Added the [`roc()`](https://jasenfinch.github.io/metabolyseR/reference/roc.html) method to calculate receiver-operator characteristic curves from the [`RandomForest`](https://jasenfinch.github.io/metabolyseR/reference/RandomForest-class.html) S4 class.

# metabolyseR 0.14.8

* An error is now thrown during random forest classification when less than two classes are specified.

* [`plotSupervisedRF()`](https://jasenfinch.github.io/metabolyseR/reference/plotSupervisedRF.html) now skips plotting if errors are encountered during random forest training.

# metabolyseR 0.14.7

* Single replicate classes now automatically removed by [`plotLDA()`](https://jasenfinch.github.io/metabolyseR/reference/plotLDA.html).

# metabolyseR 0.14.6

* [`plotExplanatoryHeatmap()`](https://jasenfinch.github.io/metabolyseR/reference/plotExplanatoryHeatmap.html) method for the [`Analysis`](https://jasenfinch.github.io/metabolyseR/reference/Analysis-class.html) class now returns the plot only if the number of plots is equal to 1. 

* Removed reference to the `nCores` parameter from the documentation example of [`metabolyse()`](https://jasenfinch.github.io/metabolyseR/reference/metabolyse.html).

# metabolyseR 0.14.5

* Correlation analysis results now include an absolute correlation coefficient column by which the results are also arranged in descending order.

# metabolyseR 0.14.4

* Console output from [`imputeAll()`](https://jasenfinch.github.io/metabolyseR/reference/impute.html) now suppressed.

# metabolyseR 0.14.3

* Temporarily added [jasenfinch/missForest](https://github.com/jasenfinch/missForest) as a remote until [stekhoven/missForest](https://github.com/stekhoven/missForest) pull request [#25](https://github.com/stekhoven/missForest/pull/25) is resolved.

* The limit of the number of plotted features in [`plotExplanatoryHeatmap`](https://jasenfinch.github.io/metabolyseR/reference/plotExplanatoryHeatmap.html) can now be specified using the `featureLimit` argument. 

* [`plotExplanatoryHeatmap()`](https://jasenfinch.github.io/metabolyseR/reference/plotExplanatoryHeatmap.html) now returns NULL and returns a message when no explanatory features are found.

* Fixed the alignment of the dendrogram branches with heat map rows in [`plotExplanatoryHeatmap()`](https://jasenfinch.github.io/metabolyseR/reference/plotExplanatoryHeatmap.html).

* Fixed `ggplot2::guides()` warning in [`plotFeature()`](https://jasenfinch.github.io/metabolyseR/reference/plotFeature.html) and [`plotTIC()`](https://jasenfinch.github.io/metabolyseR/reference/plotTIC.html).

* Fixed bug in [`explanatoryFeatures()`](https://jasenfinch.github.io/metabolyseR/reference/modelling-accessors.html) methods for [`Analysis`](https://jasenfinch.github.io/metabolyseR/reference/Analysis-class.html) class and lists where the threshold was not applied.

* Fixed the error in [`plotRSD()`](https://jasenfinch.github.io/metabolyseR/reference/plotRSD.html) method for the [`Analysis`](https://jasenfinch.github.io/metabolyseR/reference/Analysis-class.html) class.

* Corrected the text in the [modelling vignette](https://jasenfinch.github.io/metabolyseR/articles/modelling.html) concerning the results of using unsupervised random forest for outlier detection.

# metabolyseR 0.14.2

* Package version, creation date and verbose argument added to prototype of `Analysis` class.

* All generics are now defined as standard generics.

* Added `metrics` method for `Analysis` class.

* `metrics` method for lists now ignores list elements that are not of class `RandomForest`.

# metabolyseR 0.14.1

* Changed the `RSDthresh` argument default to 50% instead of 0.5% in `QCrsdFilter` generic.

# metabolyseR 0.14.0

* Added a `NEWS.md` file to track changes to the package.

* `pkgdown` site now available at <https://jasenfinch.github.io/metabolyseR/>.

* Bug reports and issues URL at <https://github.com/jasenfinch/metabolyseR/issues> added to package DESCRIPTION.

* Dedicated vignettes now available for a quick start example analysis, data pre-treatment and data modelling.

* Function examples added to all documentation pages.

* Unit test coverage increased to > 95%.

* Parallel processing is now implemented using the [`future`](https://cran.r-project.org/package=future) package.

* `plan()` from the [`future`](https://cran.r-project.org/web/packages/future/index.html) package is re-exported.

* `RandomForest` and `Univariate` classes now inherit from class the `AnalysisData` class.

* Improvements to plot theme aesthetics.

* `type` argument added to `plotPCA()`, `plotLDA()`, `plotUnsupervisedRF()` and `plotSupervisedRF()` methods for the `Analysis` class.

* `"pre-treated"` for specifying type argument in `Analysis` class methods now used over `"preTreated"`

* Added `clsRename()` method for renaming class information columns.

* `plotMeasures()` method renamed to `plotMetrics()`.

* Added `plotMDS()`, `plotImportance()` and `plotMetrics()` methods for lists of `RandomForest` class objects.

* Added `plotExplanatoryHeatmap()` method for lists of `RandomForest` or `Univariate` class objects.

* Renamed `keepVariables()` and `removeVariables()` methods to `keepFeatures()` and `removeFeatures()`.

* Added the helper functions `preTreatmentElements()`, `preTreatmentMethods()` and `preTreatParameters()` for declaring pre-treatment parameters for the `AnalysisParameters` class.

* Added the helper functions `modellingMethods()` and `modellingParameters()` for declaring modelling parameters for the `AnalysisParameters` class.

* Added helper function `correlationsParameters()` for declaring correlations parameters for the `AnalysisParameters` class.

* Added `binaryComparisons()` method for retrieving all possible binary class comparisons from an `AnalysisData` class object.

* `changeParameter()` now assigns parameter values through direct assignment.

* Added `analysisResults()` method from extracting analysis elements results from the `Analysis` class.

* Added `exportParameters()` method for exporting analysis parameters to YAML file format.

* Added `dat()` and `sinfo()` accessor methods for the `Analysis` class.

* Relative standard deviation (RSD) values are now specified and returned as percentages.
