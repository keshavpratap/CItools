
#################################################################
## Complete missing data
#  Calls CompleteIndicator() which completes the indicator data using controls
# Args:
#   data to be completed, controls
#
# Returns:
#   Completed data based on the controls used

CompleteData <- function(data, ctr){
  
  if (ctr$missingindicator == 'reweight' |  ctr$missingindicator == 'none' ){
    return(data)
  }
  entity  <- sort(unique(as.character(data[,2])))
  nentity <- length(entity)
  n <- ncol(data)
  # iterate over all entity
  for (i in 1 : nentity){
    seldata <- data[data[,2] == entity[i],]
    #seldata = droplevels(seldata)
    # determine if NaN are present in the data
    pos <- order(as.character(seldata[,1]))
    # ordered indicators wrt year
    indicators <- as.matrix(seldata[pos,3:n])
    indicators <- CompleteIndicators(indicators, ctr)
    # kb
    seldata[pos,3:n] <- indicators
    data[data[,2] == entity[i],] = seldata
  }
  return(data)
}

CompleteIndicators <- function(indicators, ctr) {
  indicators <- as.matrix(indicators)
  # if reweight, keep dataset
  if (ctr$missingindicator == 'reweight' |  ctr$missingindicator == 'none' ){
    return(indicators)
  }
  # if no NaN detected, keep dataset
  if (!any(is.na(indicators) | is.nan(indicators))){
    return(indicators)
  }
  n <- nrow(indicators)
  # NaN observed but no history
  if (n == 1){
    #cat ('NA observed for a single time period; cannot complete missing data\n')
    return(indicators)
  }
  # NaN observed and history available; fill missing value depending on the method
  if (ctr$missingindicator == 'last'){
    #for( i in 1:ncol(indicators)){
    #  indicators[,i] = interpNA(indicators[,i],method="before")
    #}
    counter <- rep(0,length(indicators[1,]))
    maxstep <- 20;
    for (t in 2 : n){
      pos <- is.na(indicators[t,]) | is.nan(indicators[t,]) 
      if (any(pos)){
        counter[pos] <- counter[pos]+1;   
        indicators[t,(pos & counter<=maxstep) ] <- indicators[t-1,(pos & counter<=maxstep)]
        counter[!pos] <- 0
        counter[ counter[pos]>maxstep] <- 0; 
      }
    }
  }
  else{
    for (i in 1:ncol(indicators)){
      if (length(na.omit(indicators[,i]))>2){
        indicators[,i] <- interpNA(indicators[,i],method="linear")        
      }
    }
    # fill in remaining missing ones 
    counter <- rep(0,length(indicators[1,]))
    maxstep <- 20;
    for (t in 2 : n){
      pos <- is.na(indicators[t,]) | is.nan(indicators[t,]) 
      if (any(pos)){
        counter[pos] <- counter[pos]+1;   
        indicators[t,(pos & counter<=maxstep) ] <- indicators[t-1,(pos & counter<=maxstep)]
        counter[!pos] <- 0
        counter[ counter[pos]>maxstep] <- 0; 
      }
    }    
    #for (t in 2 : (n-1)){
    #  pos = is.na(indicators[t,]) || is.nan(indicators[t,])
    #  if (any(pos)){
    #    indicators[t,pos] = (indicators[t-1,pos] + indicators[t+1,pos])/2
    #  }
    #}
  }
  # replace remaining NA or NaN by unconditional average
  #M = matrix(data = apply(indicators, 2, mean, na.rm = TRUE), nrow = nrow(indicators), 
  #           ncol = ncol(indicators), byrow = TRUE)
  #pos = is.na(indicators) | is.nan(indicators)
  #indicators[pos] = M[pos]
  return(indicators)  
}

#################################################################
## Clean local outlying data
#  Calls CleanIndicator() which cleans the indicator data using controls
#  has functions HampleOnesided and HampleTwosided for cleaning using these two methods
# Args:
#   data to be cleaned, controls
#
# Returns:
#   Completed data based on the controls used

CleanData <- function(data, ctr){
  if (ctr$cleaning == 'none'){
    return(data)
  }
  entity  <- sort(unique(as.character(data[,2])))
  nentity <- length(entity)
  n <- ncol(data)
  # iterate over all entity
  for (i in 1:nentity){
    seldata <- data[data[,2] == entity[i],]
    #seldata = droplevels(seldata)
    # determine if NaN are present in the data
    pos <- order(as.character(seldata[,1]))
    # ordered indicators wrt time period
    indicators <- as.matrix(seldata[pos,3:n])
    indicators <- CleanIndicators(indicators, ctr)
    # kb
    seldata[pos,3:n] <- indicators
    data[data[,2] == entity[i],] = seldata    
  }
  return(data)
}

HampleTwosided <- function(x, k=2, t0 = 3){
  # package pracma
  n <- length(x)
  y <- x
  L <- 1.4826
  for (i in (k + 1):(n - k)) {
    localseries <- na.omit( x[(i - k):(i + k)] )
    if(length(localseries)>=3){
      x0 <- median(localseries)
      sigma <- L * median(abs(localseries - x0))      
      if(sigma!=0){
        if (x[i]  > (x0+t0 * sigma)) {
          y[i] <- (x0+t0 * sigma)
        }     
        if (x[i]  < (x0-t0 * sigma)) {
          y[i] <- (x0-t0 * sigma)
        }
      }
    }
  }
  return(y)
}
HampleOnesided <- function (x, k=4, t0 = 3){
  # package pracma
  n <- length(x)
  y <- x
  ind <- c()
  L <- 1.4826
  for (i in (k + 1):(n )) {
    localseries <- na.omit( x[(i - k):(i )] )
    if (length(localseries)>=3){
      x0 <- median(localseries)
      sigma <- L * median(abs(localseries - x0))      
      if (sigma!=0){
        if ( x[i]  > (x0+t0 * sigma)   ) {
          y[i] <- (x0+t0 * sigma)
        }     
        if ( x[i]  < (x0-t0 * sigma)   ) {
          y[i] <- (x0-t0 * sigma)
        }      
      }
    }
  }  
  return(y)
}

CleanIndicators <- function(indicators, ctr) {
  indicators <- as.matrix(indicators)
  # if reweight, keep dataset
  if (ctr$missingindicator == 'none'){
    return(indicators)
  }
  # if no NaN detected, keep dataset
  if (!any(is.na(indicators) | is.nan(indicators))){
    return(indicators)
  }
  n <- nrow(indicators)
  # NaN observed but no history
  if (n == 1){
    #cat ('NA observed for a single time period; cannot complete missing data\n')
    return(indicators)
  }
  # NaN observed and history available; fill missing value depending on the method
  if (ctr$cleaning == 'hampel_twosided'){
    for (i in 1:ncol(indicators)){
      indicators[,i] <- HampleTwosided(indicators[,i])
    }
  }
  else{
    for (i in 1:ncol(indicators)){
      indicators[,i] <- HampleOnesided(indicators[,i])
    }
  }
  return(indicators)  
}

#################################################################
## Normalization of the indicator variables
# Args:
#   indicatordata to be normalised, controls
#
# Returns:
#   Normalized indicators based on the controls used

NormalizeIndicators <- function(indicators, ctr){
  indicators <- as.matrix(indicators)
  tmp <- dim(indicators)
  m <- tmp[1]
  n <- tmp[2]
if(ctr$normalization == "none") {
  out <- indicators
} 
  else if (ctr$normalization == "min-max"){
    # DA we could maybe speedup with sweep instead
    if (is.null(ctr$setmax) & is.null(ctr$setmin)){
      min <- matrix(data = apply(indicators, 2, min, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
      max <- matrix(data = apply(indicators, 2, max, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
    }
    #when setmax or setmin are not null
    else{
      if (!is.null(ctr$setmax)){
        if (length(ctr$setmax) != ncol(indicators))
          stop ('Length of setmax vector not equal to no. of column of indicator')
        length.ind <- ncol(indicators)
        for (i in 1:length.ind){
          if (!is.na(ctr$setmax[i])){
            indicators[,i] <- ifelse(indicators[,i] > ctr$setmax[i], ctr$setmax[i], indicators[,i])
          }
        }
      }
      if (!is.null(ctr$setmin)){
        if (length(ctr$setmin)!=ncol(indicators))
          stop ('Length of setmin vector not equal to no. of column of indicator')
        length.ind <- ncol(indicators)
        for (i in 1:length.ind){
          if (!is.na(ctr$setmin[i])){
            indicators[,i] <- ifelse(indicators[,i] < ctr$setmin[i], ctr$setmin[i], indicators[,i])
          }
        }
      }
      min <- matrix(data = apply(indicators, 2, min, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
      max <- matrix(data = apply(indicators, 2, max, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
      length.ind <- ncol(indicators)
      for(i in 1:length.ind){
        if(ctr$setmax[i]>max[1,i] & (!is.na(ctr$setmax[i]))){
          max[,i]=apply(as.data.frame(max[,i]), 2, function(x) ifelse(x < ctr$setmax[i], ctr$setmax[i], x))
        }
        if(ctr$setmin[i]<min[1,i] & (!is.na(ctr$setmin[i]))){
          min[,i]=apply(as.data.frame(min[,i]), 2, function(x) ifelse(x > ctr$setmin[i], ctr$setmin[i], x))
        }
      }
    }
    out <- (indicators - min) / (max - min)
  }
  else if (ctr$normalization == "z-score"){
    mu  <- matrix(data = apply(indicators, 2, mean, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
    sig <- matrix(data = apply(indicators, 2, sd, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
    out <- (indicators - mu) / sig
    out[out>3] = 3;    out[out<(-3)] = -3;
  }
  
  else if (ctr$normalization == "robust-z-score"){
    mu  <- matrix(data = apply(indicators, 2, median, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
    sig <- matrix(data = apply(indicators, 2, mad, na.rm = TRUE), nrow = m, ncol = n, byrow = TRUE)
    out <- (indicators - mu) / sig;
    out[out>3] = 3;    out[out<(-3)] = -3;    
  } 
  
  else {
    stop ('control$normalization not well defined')
  }
  
  # set NaN where infinite value are observed
  pos <- is.infinite(indicators)
  out[pos] <- NaN
  return(out)
}

#################################################################
## Compute a matrix of weights of the same dimension of the indicators
# Args:
#   indicatordata for which weights are to calculated, controls
#
# Returns:
#   Weights based on the controls used
ComputeWeights <- function(indicators, ctr){
  indicators <- as.matrix(indicators)
  tmp <- dim(indicators)
  m <- tmp[1]
  n <- tmp[2]
  weights <- matrix(data = 0, nrow = m, ncol = n)
  dimnames(weights) <- dimnames(indicators)
  
  if (ctr$weights[1]=="equal"){
    o <- rep(1/n,n)
  }
  else {
    if (ctr$weights[1]=="random"){
      p <- runif(n) 
      o <- p/sum(p)
    }
    else {
      if (ctr$weights[1]=="fixed"){
        length.weights <- length(ctr$weights)
        ctr$weights <- ctr$weights[2:length.weights]
        if ((length.weights-1)!=n){
          stop ('weights and indicator length not same')
        }
        ctr$weights <- as.numeric(ctr$weights)
        if (any(is.na(ctr$weights))){
          stop ('Weights not available or are non numeric')
        }
        o <- ctr$weights        
      } 
      else{stop ('Weight method not recognized')
      }
    }
  }
  weights <- matrix(data = o, nrow = m, ncol = n, byrow = TRUE)
  dimnames(weights) <- dimnames(indicators)
  
  # reweighting scheme
  if (ctr$missingindicator == 'reweight') {
    idx <- is.nan(indicators) | is.nan(indicators)
    weights[idx] <- 0
    # reweight
    weights <- sweep(x = weights, MARGIN = 1, STATS = apply(weights, 1, sum), FUN = '/')
  }
  return(weights)
  
}