% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{transformArcSine}
\alias{transformArcSine}
\alias{transformArcSine,AnalysisData-method}
\alias{transformAuto}
\alias{transformAuto,AnalysisData-method}
\alias{transformCenter}
\alias{transformCenter,AnalysisData-method}
\alias{transformLevel}
\alias{transformLevel,AnalysisData-method}
\alias{transformLn}
\alias{transformLn,AnalysisData-method}
\alias{transformLog10}
\alias{transformLog10,AnalysisData-method}
\alias{transformPareto}
\alias{transformPareto,AnalysisData-method}
\alias{transformPercent}
\alias{transformPercent,AnalysisData-method}
\alias{transformRange}
\alias{transformRange,AnalysisData-method}
\alias{transformSQRT}
\alias{transformSQRT,AnalysisData-method}
\alias{transformTICnorm}
\alias{transformTICnorm,AnalysisData-method}
\alias{transformVast}
\alias{transformVast,AnalysisData-method}
\title{Scaling, transformation and normalisation methods}
\usage{
transformArcSine(d)

\S4method{transformArcSine}{AnalysisData}(d)

transformAuto(d)

\S4method{transformAuto}{AnalysisData}(d)

transformCenter(d)

\S4method{transformCenter}{AnalysisData}(d)

transformLevel(d)

\S4method{transformLevel}{AnalysisData}(d)

transformLn(d, add = 1)

\S4method{transformLn}{AnalysisData}(d, add = 1)

transformLog10(d, add = 1)

\S4method{transformLog10}{AnalysisData}(d, add = 1)

transformPareto(d)

\S4method{transformPareto}{AnalysisData}(d)

transformPercent(d)

\S4method{transformPercent}{AnalysisData}(d)

transformRange(d)

\S4method{transformRange}{AnalysisData}(d)

transformSQRT(d)

\S4method{transformSQRT}{AnalysisData}(d)

transformTICnorm(d, refactor = TRUE)

\S4method{transformTICnorm}{AnalysisData}(d, refactor = TRUE)

transformVast(d)

\S4method{transformVast}{AnalysisData}(d)
}
\arguments{
\item{d}{S4 object of class \code{AnalysisData}}

\item{add}{value to add prior to transformation}

\item{refactor}{TRUE/FALSE. Re-factor the normalised intensity values to a range consistent with the raw values by multiplying by the median sample TIC.}
}
\value{
An S4 object of class \code{AnalysisData} containing the transformed data.
}
\description{
Methods for data scaling, transformation and normalisation.
}
\details{
Prior to downstream analyses, metabolomics data often require transformation to fulfil the assumptions of a particular statistical/data mining technique.
Before applying a transformation, it is important to consider the effects that the transformation will have on the data, as this can greatly effect the outcome of further downstream analyses.
It is also important to consider at what stage in the pre-treatment routine a transformation is applied as this too could introduce artefacts into the data.
The best practice is to apply a transformation as the last in a pre-treatment routine after all other steps have been taken.
There are a wide range of transformation methods available that are commonly used for the analysis of metabolomics data.
}
\section{Methods}{

\itemize{
\item \code{transformArcSine}: Arc-sine transformation.
\item \code{transformAuto}: Auto scaling.
\item \code{transformCenter}: Mean centring.
\item \code{transformLevel}: Level scaling.
\item \code{transformLn}: Natural logarithmic transformation.
\item \code{transformLog10}: Logarithmic transformation.
\item \code{transformPareto}: Pareto scaling.
\item \code{transformPercent}: Scale as a percentage of the feature maximum intensity.
\item \code{transformRange}: Range scaling. Also known as min-max scaling.
\item \code{transformSQRT}: Square root transformation.
\item \code{transformTICnorm}: Total ion count normalisation.
\item \code{transformVast}: Vast scaling.
}
}

\examples{

## Each of the following examples shows the application of the transformation and then 
## a Linear Discriminant Analysis is plotted to show it's effect on the data structure.

## Initial example data preparation
library(metaboData)

d <- analysisData(abr1$neg[,200:300],abr1$fact) \%>\% 
 occupancyMaximum(occupancy = 2/3)

d \%>\% 
 plotLDA(cls = 'day')
 

## Arc-sine transformation
d \%>\% 
 transformArcSine() \%>\% 
 plotLDA(cls = 'day')

## Auto scaling
d \%>\% 
 transformAuto() \%>\% 
 plotLDA(cls = 'day')

## Mean centring
d \%>\% 
 transformCenter()\%>\% 
 plotLDA(cls = 'day')

## Level scaling
d \%>\% 
 transformLevel() \%>\% 
 plotLDA(cls = 'day')

## Natural logarithmic transformation
d \%>\% 
 transformLn() \%>\% 
 plotLDA(cls = 'day')

## Logarithmic transformation
d \%>\% 
 transformLog10()\%>\% 
 plotLDA(cls = 'day')

## Pareto scaling
d \%>\% 
 transformPareto() \%>\% 
 plotLDA(cls = 'day')

## Percentage scaling
d \%>\% 
 transformPercent() \%>\% 
 plotLDA(cls = 'day')

## Range scaling
d \%>\% 
 transformRange() \%>\% 
 plotLDA(cls = 'day')

## Square root scaling
d \%>\% 
 transformSQRT() \%>\% 
 plotLDA(cls = 'day')

## Total ion count nromalisation
d \%>\% 
 transformTICnorm() \%>\% 
 plotLDA(cls = 'day')

## Vast scaling
d \%>\% 
 transformVast() \%>\% 
 plotLDA(cls = 'day')
}
