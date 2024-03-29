% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotFeature.R
\name{plotFeature}
\alias{plotFeature}
\alias{plotFeature,AnalysisData-method}
\alias{plotFeature,Analysis-method}
\title{Plot a feature}
\usage{
plotFeature(analysis, feature, cls = "class", label = NULL, labelSize = 2, ...)

\S4method{plotFeature}{AnalysisData}(analysis, feature, cls = "class", label = NULL, labelSize = 2)

\S4method{plotFeature}{Analysis}(
  analysis,
  feature,
  cls = "class",
  label = NULL,
  labelSize = 2,
  type = c("pre-treated", "raw")
)
}
\arguments{
\item{analysis}{an object of class \code{AnalysisData} or`` Analysis`}

\item{feature}{feature name to plot}

\item{cls}{information column to use for class labels}

\item{label}{information column to use for sample labels}

\item{labelSize}{sample label size}

\item{...}{arguments to pass to the appropriate method}

\item{type}{\code{raw} or \code{pre-treated} data to plot}
}
\description{
Plot the trend of a feature.
}
\examples{
d <- analysisData(metaboData::abr1$neg,
                  metaboData::abr1$fact)

## Plot a categorical response variable
plotFeature(d,'N133',cls = 'day')

## Plot a continuous response variable
plotFeature(d,'N133',cls = 'injorder')
}
