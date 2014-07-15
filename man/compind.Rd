\name{compind}
\alias{compind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computation of Composite Index
}
\description{
Function which computes indices
}
\usage{
compind(formula, data, time = NULL, for.period = NULL, ctr = ProcessControl())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
\code{formula} needs to be written in form \code{formula="entity ~ indicator 1+ indicator 2 + indicator 3+.."} where entity is the name of the column containg entity names.
}
  \item{data}{
dataset for which index is to be computed
}
  \item{time}{
name of the coloumn containing time period. Defualt \code{time=NULL} which assumes all data is for single time period.
}
  \item{for.period}{
In case time column contains multiple time period and index is to be calculated only for a particular period. See example.
}
  \item{ctr}{
Different types of controls to be taken input as \code{ctr=ProcessControl()}. See examples and details of the controls below.
}
}
\details{
The argument \code{ctr} is a input via function \code{ProcessControl()}  that can supply any of the following components. Please see \code{ProcessControl} for details.
   }

\value{
An object of class compind.

The object contains a list with the following components:

\item{index:}{matrix of size (nentity x ntime) containing the index scores. NA indicates no country observation available.}
\item{ranking: }{matrix of size (nentity x ntime) containing the index ranking. NA indicates no country observation available.}
\item{changeinranking: }{ matrix of size (nentity x ntime-1) containing the index ranking changes. NA indicates no country observation available. Negative value indicate a drop in the ranking (bad performance) while a positive value indicates a gain the ranking (good performance).}
\item{time.list }{list of size ntime, containing for each time period, a list of ranking, score, indicator, normindicators amd weights for that time period.}
\item{ctr}{returns values of all the controls used for computing the index}
}

\author{
David Ardia, Kris Boudt, Valentin Todorov and Keshav Pratap
}
\references{
Meghnad Desai et.al.(2002),'Measuring the Technology Achievement of Nations and the Capacity to Participate in the Network Age'Journal of Human Development, Vol. 3, No. 1, 2002 (URL: http://unpan1.un.org/intradoc/groups/public/documents/apcity/unpan014340.pdf)

}
\examples{
  # Calculation TAI index, see reference paper 
  # load data
  data(tai)
  # setmax and setmin values are according to reference paper
  # see use of controls in this example
  indx_tai= compind(country~., data=tai, ctr=ProcessControl(aggregation='linear', setmax=c(994,272.6,232.4,80.8,2.9547,3.84317, 12,27.4), setmin=c(0,0,0,0,0,1.3424,0.8,0.1)))
  print(indx_tai)
  
  # Following is an example of calculation of mixed-eight cip index, the index calculation is done using hierarchical aggregation
  # Load data
  data(cip)
  # Create new sub-indicator Industrial intensity (INDint), and Manufactured Exports Quality (MXQual)
  INDint <- compind(country ~ MHVAsh + MVAsh, time="year", for.period=2010, data=cip, ctr=ProcessControl(aggregation='linear'))
  MXQual <- compind(country ~ MHXsh + MXsh, time="year", for.period=2010, data=cip, ctr=ProcessControl(aggregation='linear'))
  # Calculation index using rest of the sub-indicators 
  indicators <- compind(country~MVApc + MXpc + ImWMVA + ImWMT, time= "year", data=cip, for.period=2010, ctr= ProcessControl(weights=c("fixed", 1/6, 1/6, 1/6, 1/6)))
  colnames(indicators$index) = "indicators"
  # Binding results of the subindicators
  cip8 <- cbind(indicators$index, INDint$index, MXQual$index)
  # Calculating mixed eight CIP index
  index_cip_2010 <- compind(data=cip8, ctr=ProcessControl(weights=c("fixed", 1, 1/6,1/6),normalization="none"))
  # Print index
  print(index_cip_2010)
  }