#################################################################
## Compute the sensitivity of CIP for given weights
# Args:
#   input: computed index of compind class
#   weights: vector of weights
#   covmethod: method for computing the covariance
# Returns:
#   A list with the following in it:
#   sensitivity: vector of marginal sensitivities 
#   component: vector of component sensitivities
#   sigma: covarince matrix of nomalized data

sensitivity <- function(input, ...) UseMethod("sensitivity")


sensitivity.compind <- function(input, weights, covmethod = c('standard', 'robust')){
  if (missing(weights)){
    stop ('weights is missing')
  }
  weights <- as.vector(weights)
  n <- length(weights)
  if (missing(input)){
    stop ('Index input is missing')
  }
  normdata <- input$time.list[[1]]$normindicators
  
  normdata <- as.matrix(normdata)
  tmp <- dim(normdata)
  if (tmp[2] != n){
    stop ('dimension mismatch')
  }
  if (covmethod[1] == 'robust'){
    require(robustbase)
    Sigma <- covMcd(normdata)$cov
  }
  else {
    Sigma <- stats::cov(normdata, use = 'complete.obs')
  }
  
  # component weights
  sensitivity <- as.vector((Sigma %*% weights) / sqrt(as.numeric(t(weights) %*% Sigma %*% weights)))
  component   <- weights * sensitivity
  indicator.names <- colnames(input$time.list[[1]]$indicators)
  names(sensitivity) <- indicator.names
  names(component) <- indicator.names
  out <- list(sensitivity = sensitivity, component = component, Sigma = Sigma)
  out$call <- match.call()
  class(out) <- "sensitivity"
  return(out)
}
