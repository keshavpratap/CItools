\name{print.compare}
\alias{print.compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print results of Compare
}
\description{
Prints the results of \code{compare} of composite index. 
}
\usage{
## S3 method for class 'compare'
print(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
output of the function \code{compare}
}
}

\value{
It states minimum, maximum and mean average Index Difference. If a bench index is provided, then Absolute Rank Difference vector, timeperiod-average value of AbsRankDifference, timeperiod-average value of correlation between ranks is printed.

}

\author{
David Ardia, Kris Boudt, Valentin Todorov and Keshav Pratap
}

\examples{
  # load data
  data(cip)
  # compute Composite index (eight-indicators linear) for a range of years
  index_cip = compind(country~., time="year", for.period = 2005:2010, data=cip, ctr=ProcessControl(weights=c("fixed", 1/6, 1/6, 1/12, 1/12, 1/12, 1/12, 1/6, 1/6), aggregation="linear"))
  out = compare(index_cip)
  #Print and plot compare results
  print(out)
  plot(out)
  # use a benchmark (a given year)
  bench= compind(country~., time="year", for.period = 2004, data=cip, ctr=ProcessControl(weights=c("fixed", 1/6, 1/6, 1/12, 1/12, 1/12, 1/12, 1/6, 1/6), aggregation="linear"))
  out = compare(index_cip, bench)
  print(out)
}
