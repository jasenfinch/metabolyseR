% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelling-accessors.R
\name{binaryComparisons}
\alias{binaryComparisons}
\alias{binaryComparisons,AnalysisData-method}
\alias{mtry}
\alias{mtry,AnalysisData-method}
\alias{type}
\alias{type,RandomForest-method}
\alias{type,Univariate-method}
\alias{response}
\alias{response,RandomForest-method}
\alias{response,Univariate-method}
\alias{metrics}
\alias{metrics,RandomForest-method}
\alias{metrics,list-method}
\alias{metrics,Analysis-method}
\alias{predictions}
\alias{predictions,RandomForest-method}
\alias{predictions,list-method}
\alias{predictions,Analysis-method}
\alias{importanceMetrics}
\alias{importanceMetrics,RandomForest-method}
\alias{importance}
\alias{importance,RandomForest-method}
\alias{importance,Univariate-method}
\alias{importance,list-method}
\alias{importance,Analysis-method}
\alias{proximity}
\alias{proximity,RandomForest-method}
\alias{proximity,list-method}
\alias{proximity,Analysis-method}
\alias{explanatoryFeatures}
\alias{explanatoryFeatures,Univariate-method}
\alias{explanatoryFeatures,RandomForest-method}
\alias{explanatoryFeatures,list-method}
\alias{explanatoryFeatures,Analysis-method}
\title{Modelling accessor methods}
\usage{
binaryComparisons(x, cls = "class")

\S4method{binaryComparisons}{AnalysisData}(x, cls = "class")

mtry(x, cls = "class")

\S4method{mtry}{AnalysisData}(x, cls = "class")

type(x)

\S4method{type}{RandomForest}(x)

\S4method{type}{Univariate}(x)

response(x)

\S4method{response}{RandomForest}(x)

\S4method{response}{Univariate}(x)

metrics(x)

\S4method{metrics}{RandomForest}(x)

\S4method{metrics}{list}(x)

\S4method{metrics}{Analysis}(x)

predictions(x)

\S4method{predictions}{RandomForest}(x)

\S4method{predictions}{list}(x)

\S4method{predictions}{Analysis}(x)

importanceMetrics(x)

\S4method{importanceMetrics}{RandomForest}(x)

importance(x)

\S4method{importance}{RandomForest}(x)

\S4method{importance}{Univariate}(x)

\S4method{importance}{list}(x)

\S4method{importance}{Analysis}(x)

proximity(x, idx = NULL)

\S4method{proximity}{RandomForest}(x, idx = NULL)

\S4method{proximity}{list}(x, idx = NULL)

\S4method{proximity}{Analysis}(x, idx = NULL)

explanatoryFeatures(x, ...)

\S4method{explanatoryFeatures}{Univariate}(
  x,
  threshold = 0.05,
  value = c("adjusted.p.value", "p.value")
)

\S4method{explanatoryFeatures}{RandomForest}(
  x,
  metric = "false_positive_rate",
  value = c("value", "p-value", "adjusted_p-value"),
  threshold = 0.05
)

\S4method{explanatoryFeatures}{list}(x, ...)

\S4method{explanatoryFeatures}{Analysis}(x, ...)
}
\arguments{
\item{x}{S4 object of class \code{AnalysisData},\code{RandomForest}, \code{Univariate}, \code{Analysis} or a list.}

\item{cls}{sample information column to use}

\item{idx}{sample information column to use for sample names. If \code{NULL}, the sample row number will be used. Sample names should be unique for each row of data.}

\item{...}{arguments to parse to method for specific class}

\item{threshold}{threshold below which explanatory features are extracted}

\item{value}{the importance value to threshold. See the usage section for possible values for each class.}

\item{metric}{importance metric for which to retrieve explanatory features}
}
\description{
Methods for accessing modelling results.
}
\section{Methods}{

\itemize{
\item \code{binaryComparisons}: Return a vector of all possible binary comparisons for a given sample information column.
\item \code{mtry}: Return the default \code{mtry} random forest parameter value for a given sample information column.
\item \code{type}: Return the type of random forest analysis.
\item \code{response}: Return the response variable name used for a random forest analysis.
\item \code{metrics}: Retrieve the model performance metrics for a random forest analysis
\item \code{predictions}: Retrieve the out of bag model response predictions for a random forest analysis.
\item \code{importanceMetrics}: Retrieve the available feature importance metrics for a random forest analysis.
\item \code{importance}: Retrieve feature importance results.
\item \code{proximity}: Retrieve the random forest sample proximities.
\item \code{explanatoryFeatures}: Retrieve explanatory features.
}
}

\examples{
library(metaboData)

d <- analysisData(abr1$neg[,200:300],abr1$fact)

## Return possible binary comparisons for the `day` response column
binaryComparisons(d,cls = 'day')

## Return the default random forest `mtry` parameter for the `day` response column
mtry(d,cls = 'day')

## Perform random forest analysis
rf_analysis <- randomForest(d,cls = 'day')

## Return the type of random forest
type(rf_analysis)

## Return the response variable name used
response(rf_analysis)

## Retrieve the model performance metrics
metrics(rf_analysis)

## Retrieve the out of bag model response predictions
predictions(rf_analysis)

## Show the available feature importance metrics
importanceMetrics(rf_analysis)

## Retrieve the feature importance results
importance(rf_analysis)

## Retrieve the sample proximities
proximity(rf_analysis)

## Retrieve the explanatory features
explanatoryFeatures(rf_analysis,metric = 'false_positive_rate',threshold = 0.05)
}
