% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameters.R
\name{parameters}
\alias{parameters}
\alias{parameters,AnalysisParameters-method}
\alias{parameters,Analysis-method}
\alias{parameters<-}
\alias{parameters<-,AnalysisParameters-method}
\alias{parameters<-,Analysis-method}
\title{Get or set analysis parameters}
\usage{
parameters(d, ...)

\S4method{parameters}{AnalysisParameters}(d, element)

\S4method{parameters}{Analysis}(d)

parameters(d, element) <- value

\S4method{parameters}{AnalysisParameters}(d, element) <- value

\S4method{parameters}{Analysis}(d) <- value
}
\arguments{
\item{d}{S4 object of class \code{AnalysisParameters} or \code{Analysis}}

\item{...}{arguments to pass to the appropriate method}

\item{element}{analysis element for parameters to extract or assign.
Should be one of those returned by \code{analysisElements()}}

\item{value}{list containing parameter values}
}
\description{
Get or set parameters for \code{AnalysisParameters} or \code{Analysis} class objects.
}
\examples{
p <- analysisParameters('pre-treatment')

## extract pre-treatment parameters
parameters(p,'pre-treatment')

## set pre-treatment parameters
parameters(p,'pre-treatment') <- preTreatmentParameters(
  list(
    remove = 'classes',
    QC = c('RSDfilter','removeQC'),
    transform = 'TICnorm'
  )
)

print(p)
}
