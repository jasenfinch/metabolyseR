# metabolyseR 0.14.0

* Added a `NEWS.md` file to track changes to the package.

* `pkgdown` site now available at <https://jasenfinch.github.io/metabolyseR/>.

* Bug reports and issues URL at <https://github.com/jasenfinch/metabolyseR/issues> added to package DESCRIPTION.

* Dedicated vignettes now available for a quick start example analysis, data pre-treatment and data modelling.

* Function examples added to all documentation pages.

* Unit test coverage increased to > 95%.

* Parallel processing is now implemented using the [`future`](https://cran.r-project.org/package=future) package.
Information on how this can now be used is available in the usage vignette.

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
