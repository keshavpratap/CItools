\name{print.sensitivity}
\alias{print.sensitivity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print results of Sensitivity
}
\description{
Prints the results of \code{sensitivity} of composite index. 
}
\usage{
## S3 method for class 'sensitivity'
print(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
output of the function \code{sensitivity}
}
}

\value{
It states the sensitivity: vector of marginal sensitivities and component: vector of component sensitivities.

}

\author{
David Ardia, Kris Boudt, Valentin Todorov and Keshav Pratap
}

\examples{
  # load data
  data(cip)
  # compute Composite index (eight-indicators linear) for a range of years
  index_cip = compind(country~., time="year", for.period = 2010, data=cip, ctr=ProcessControl(weights=c("fixed", 1/6, 1/6, 1/12, 1/12, 1/12, 1/12, 1/6, 1/6), aggregation="linear"))
  weights = c(1/6,1/6,0,0,1/6,1/6,0,1/3)
  out = sensitivity(index_cip,weights)
  print(out)
}