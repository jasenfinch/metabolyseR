% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelling-plots.R
\name{plotImportance}
\alias{plotImportance}
\alias{plotImportance,Univariate-method}
\alias{plotImportance,RandomForest-method}
\alias{plotImportance,list-method}
\title{Plot feature importance}
\usage{
plotImportance(x, ...)

\S4method{plotImportance}{Univariate}(x, response = "class", rank = TRUE, threshold = 0.05)

\S4method{plotImportance}{RandomForest}(x, metric = "false_positive_rate", rank = TRUE)

\S4method{plotImportance}{list}(x, metric = "false_positive_rate")
}
\arguments{
\item{x}{S4 object of class \code{Univariate} or \code{RandomForest}}

\item{...}{arguments to pass to specific method}

\item{response}{response results to plot}

\item{rank}{rank feature order for plotting}

\item{threshold}{explanatory threshold line for the output plot}

\item{metric}{importance metric to plot}
}
\description{
Plot Univariate or random forest feature importance.
}
\examples{
library(metaboData)

x <- analysisData(abr1$neg[,200:300],abr1$fact) \%>\%
       keepClasses(cls = 'day',classes = c('H','1','5')) \%>\% 
       occupancyMaximum(cls = 'day') \%>\%
       transformTICnorm()
       
rf <- randomForest(x,cls = 'day')

plotImportance(rf,rank = FALSE)
}
