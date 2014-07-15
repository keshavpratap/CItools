\name{compare.compind}
\alias{compare.compind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparison of Composite Indices
}
\description{
Function which compares Composite Indices among entities
}
\usage{
## S3 method for class 'compind'
compare.compind(input, bench = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
calculated composite index: output of the function \code{compind}
}
  \item{bench}{
output of the function \code{compind} for the bechmark. Default to \code{NULL}, i.e. no benchmark used.
}
}
\details{
  - Compute the difference in the values of the composite indexes over time, and on average.
  - Compute the average shift in entities' ranks wrt a benchmark.
}
\value{
A list with the following components:\cr

  \code{IndexDifference}: array of size (number of timeperiods x number of entities x number of entities) containing the pairwise enitity difference
  in the value of the composite index for a given time period.\cr

  \code{AvIndexDifference}: matrix of size (number of entities x number of entities) containing the 
  time-average value of \code{MeanIndexDifference}.\cr

  \code{AbsRankDifference}: vector of size (number of time periods x 1) containing the absolue value of the average shift in entities' rank for a given time period. Is \code{NULL} if 
  not benchmark is provided.\cr
  
  \code{AvAbsRankDifference}: timeperiod-average value of \code{AbsRankDifference}. Is \code{NULL} if no benchmark is provided.
  
  \code{AvRankCorrelation}: timeperiod-average value of correlation between ranks. Is \code{NULL} if no benchmark is provided.  
}
\references{
Saisana, M., Saltelli, A., Tarantola, S. (2005)
  Uncertainty and sensitivity analysis techniques as tools for the quality assessment
  of composite indicators.
  \emph{Journal of Royal Statistical Society} \bold{168}, pp.307-323.
}
\author{
David Ardia, Kris Boudt, Valentin Todorov and Keshav Pratap
}

\examples{

  # Load data
  data(cip)
  # Compute Composite index (eight-indicators linear) for a range of years
  index_cip = compind(country~., time="year", for.period = 2005:2010, data=cip, ctr=ProcessControl(weights=c("fixed", 1/6, 1/6, 1/12, 1/12, 1/12, 1/12, 1/6, 1/6), aggregation="linear"))
  out = compare(index_cip)
  # Print and plot compare results
  print(out)
  plot(out)
  # Use a benchmark (a given year)
  bench= compind(country~., time="year", for.period = 2004, data=cip, ctr=ProcessControl(weights=c("fixed", 1/6, 1/6, 1/12, 1/12, 1/12, 1/12, 1/6, 1/6), aggregation="linear"))
  out = compare(index_cip, bench)
  print(out)
}
