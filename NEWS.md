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
