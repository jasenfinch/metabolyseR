% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metabolyse.R
\name{metabolyse}
\alias{metabolyse}
\alias{reAnalyse}
\alias{reAnalyse,Analysis-method}
\title{Perform an analysis}
\usage{
metabolyse(data, info, parameters = analysisParameters(), verbose = TRUE)

reAnalyse(analysis, parameters = analysisParameters(), verbose = TRUE)

\S4method{reAnalyse}{Analysis}(analysis, parameters = analysisParameters(), verbose = TRUE)
}
\arguments{
\item{data}{tibble or data.frame containing data to analyse}

\item{info}{tibble or data.frame containing data info or meta data}

\item{parameters}{an object of AnalysisParameters class containing
parameters for analysis. Default calls \code{analysisParameters()}}

\item{verbose}{should output be printed to the console}

\item{analysis}{an object of class Analysis containing previous
analysis results}
}
\value{
An S4 object of class \code{Analysis}.
}
\description{
Perform analyses containing multiple analysis element steps.
}
\details{
Routine analyses are those that are often made up of numerous steps where parameters have likely already been previously established.
The emphasis here is on convenience with as little code as possible required.
In these analyses, the necessary analysis elements, order and parameters are first prepared and then the analysis routine subsequently performed in a single step.
The \code{metabolyse} function provides this utility, where the metabolome data, sample meta information and analysis parameters are provided.
The \code{reAnalyse} method can be used to perform further analyses on the results.
}
\examples{
library(metaboData)

## Generate analysis parameters
p <- analysisParameters(c('pre-treatment','modelling'))

## Alter pre-treatment and modelling parameters to use different methods
parameters(p,'pre-treatment') <- preTreatmentParameters(
  list(occupancyFilter = 'maximum',
       transform = 'TICnorm')
)
parameters(p,'modelling') <- modellingParameters('anova')

## Change "cls" parameters 
changeParameter(p,'cls') <- 'day'

## Run analysis using a subset of the abr1 negative mode data set
analysis <- metabolyse(abr1$neg[,1:200],
                       abr1$fact,
                       p)
                       
## Re-analyse to include correlation analysis
analysis <- reAnalyse(analysis,
                      parameters = analysisParameters('correlations'))

print(analysis)

}
