\name{print.compind}
\alias{print.compind}
\title{
Print composite index
}
\description{
Prints the comosite index ranking of the entities. Also it states the methods of imputation of missing data, cleaning, aggregation and weighting used for calculating index 
}
\usage{
## S3 method for class 'compind'
print(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
calculated composite index: output of the function \code{compind}
}
}

\value{
Returns the ranking of the composite index calculated. Also states the the methods of imputation of missing data, cleaning, aggregation and weighting used for calculating index 

}

\author{
David Ardia, Kris Boudt, Valentin Todorov and Keshav Pratap
}

\examples{
  # Calculation TAI index, see reference paper 
  # load data
  data(tai)
  # setmax and setmin values are according to reference paper
  indx_tai= compind(country~., data=tai, ctr=ProcessControl(aggregation='linear', setmax=c(994,272.6,232.4,80.8,2.9547,3.84317, 12,27.4), setmin=c(0,0,0,0,0,1.3424,0.8,0.1)))
  print(indx_tai)
}
