% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keep.R
\name{keepClasses}
\alias{keepClasses}
\alias{keepClasses,AnalysisData-method}
\alias{keepFeatures}
\alias{keepFeatures,AnalysisData-method}
\alias{keepSamples}
\alias{keepSamples,AnalysisData-method}
\title{Keep samples, classes or features}
\usage{
keepClasses(d, cls = "class", classes = c())

\S4method{keepClasses}{AnalysisData}(d, cls = "class", classes = c())

keepFeatures(d, features = character())

\S4method{keepFeatures}{AnalysisData}(d, features = character())

keepSamples(d, idx = "fileOrder", samples = c())

\S4method{keepSamples}{AnalysisData}(d, idx = "fileOrder", samples = c())
}
\arguments{
\item{d}{S4 object of class AnalysisData}

\item{cls}{info column to use for class information}

\item{classes}{classes to keep}

\item{features}{features to remove}

\item{idx}{info column containing sample indexes}

\item{samples}{sample indexes to keep}
}
\value{
An S4 object of class \code{AnalysisData} with specified samples, classes or features retained.
}
\description{
Retain samples, classes or features in an \code{AnalysisData} object.
}
\section{Methods}{

\itemize{
\item \code{keepClasses}: Keep classes.
\item \code{keepFeatures}: Keep features.
\item \code{keepSamples}: Keep samples.
}
}

\examples{
library(metaboData)
 d <- analysisData(abr1$neg[,200:300],abr1$fact)
 
 ## Keep classes
 d \%>\% 
  keepClasses(cls = 'day',classes = 'H')
 
 ## Keep features
 d \%>\% 
  keepFeatures(features = c('N200','N201'))
 
 ## Keep samples
 d \%>\% 
  keepSamples(idx = 'injorder',samples = c(1,10))
}
