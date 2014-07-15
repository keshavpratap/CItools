\name{sensitivity.compind}
\alias{sensitivity.compind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sensitivity analysis of the composite index
}
\description{
Function which compute the marginal and component sensitivity of the composite index
}
\usage{
## S3 method for class 'compind'
sensitivity.compind(input, weights, covmethod = c("standard", "robust"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
calculated composite index: output of the function \code{compind}
}
  \item{weights}{
vector of weights.
}
  \item{covmethod}{
method for computing the covariance. Default: \code{covmethod = 'standard'}.
}
}
\details{
Sensitivity is defined as Sigma * w / (w' * Sigma * w)^{1/2}.
}
\value{
A list with the following components:\cr

  \code{sensitivity}: vector of marginal sensitivities.\cr
  
  \code{component}: vector of component sensitivities.\cr

  \code{Sigma}: covariance matrix of normalized data.
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

\seealso{
\code{\link{compind}} for computing the Composite index and \code{\link{compare.compind}} for comparison of Composite Indices.
}
\examples{
  # load data
  data(cip)
  # compute Composite index (eight-indicators linear) for a range of years
  index_cip = compind(country~., time="year", for.period = 2005:2010, data=cip, ctr=ProcessControl(weights=c("fixed", 1/6, 1/6, 1/12, 1/12, 1/12, 1/12, 1/6, 1/6), aggregation="linear"))
  # compute sensitivity
  weights = c(1/6,1/6,0,0,1/6,1/6,0,1/3)
  out = sensitivity(index_cip,weights)
  print(out)
}

