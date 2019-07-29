% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/modellingPlots.R
\docType{methods}
\name{plotImportance}
\alias{plotImportance}
\alias{plotImportance,Univariate-method}
\alias{plotImportance,RandomForest-method}
\title{plotImportance}
\usage{
plotImportance(x, ...)

\S4method{plotImportance}{Univariate}(x, predictor = "class")

\S4method{plotImportance}{RandomForest}(x)
}
\arguments{
\item{x}{S4 object of class Univariate or RandomForest}

\item{predictor}{Predictor results to plot}
}
\description{
Plot univariate or random forest feature importance.
}