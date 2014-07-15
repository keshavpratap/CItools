\name{ProcessControl}
\alias{ProcessControl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Process Control for compind
}
\description{
Process Control for calculating composite index using compind
}
\usage{
ProcessControl(aggregation = c('geometric' , 'linear'),                       normalization = c('min-max', 'none', 'z-score', 'robust-z-score'),                       weights = c( 'equal', 'random', 'fixed'),missingindicator = c('last','none','reweight', 'linear'), cleaning = c('none', 'hampel_twosided', 'hampel_onesided'), setmax = NULL, setmin = NULL)
}

\arguments{
  \item{aggregation}{type of aggregation used, among \code{'linear'} and \code{'geometric'}. 
    Default: \code{aggregation = 'geometric'}.
}
 \item{normalization}{type of normalization, among \code{'min-max'}, \code{'z-score'}, \code{'none'}and \code{'robust-z-score'}. 
    Default: \code{normalization = 'min-max'}.
}
\item{weights}{weighting scheme, among \code{'equal'}, \code{'random'}, and \code{'fixed'}. 
    Default: \code{weights = 'equal'}.
When using fixed as method, the user can input fixed weights input using    \code{weights=c(fixed, w1, w2,...)}. 
Please note the lenght of the vector weight in this case must be equal to number of indicators plus one. }

\item{missingindicator}{action in case of missing value, among \code{'last'}, \code{'none'},\code{'reweight'} and \code{'linear'}.  
    Default: \code{missingindicator = 'last'}.}
\item{cleaning}{winsorization of outliers, among \code{'random'}, \code{'none'},  \code{'hampel_twosided'} and \code{'hampel_onesided'}.
    Default: \code{cleaning = 'none'}.} 
\item{setmax}{caps the value of an indicator to a fixed number in min-max normalisation. Input should be as \code{setmax <- c(NA, NA, x1, NA,..) }if third indicator is to be capped at x1.See example for more detail. Please note the length of vector setmax should be equal to number of indicators.} 
\item{setmin}{Fixes the minmum value of an indicator to a fixed number in min-max normalisation. Input should be as \code{setmin <- c(NA, NA, x1, NA,..) }if third indicator is to be fixed at minimum value x1.See example for more detail. Please note the length of vector setmin should be equal to number of indicators.}
}

\value{
A list containing final methods for \code{aggregation, normalization, weights, missingindicators, cleaning, setmax, setmin} computation of index.
}

\author{
David Ardia, Kris Boudt, Valentin Todorov and Keshav Pratap
}

\seealso{
compind
}
\examples{

 #set controlds
  ctr=ProcessControl(aggregation='linear', setmax=c(994,272.6,232.4,80.8,2.9547,3.84317, 12,27.4), setmin=c(0,0,0,0,0,1.3424,0.8,0.1))
 # load data
  data(tai)
  indx_tai= compind(country~., data=tai, ctr=ctr)
  print(indx_tai)
}

