#################################################################
## Compute the composite index for a given dataset and control argument
#  calls ComputeSingleYearIndex to calculate index for each and every year
#
# Args:
#   formula: Input as format entity~ ind1+ind2+ind3..
#   data: dataset 
#   time: the name of the column corresponding to time/year
#   for.period : In case the time has multiple timeperiod and you need to
#                compute index only for one timeperiod, ex: for.period=2000
#   ctr: Process Control inputs
# Returns:
#   A list of index, ranking, changeinranking,time.list
#   time.list is a list of indicators, normindicators, weights
#   Also returns call and ctr in the out

compind <- function(formula, data, time= NULL, for.period= NULL, ctr=ProcessControl()){
  
  # inputs checking
  if (missing(data))
    stop ('data is missing')
  if (!is.data.frame(data)){
    data <- as.data.frame(data)
  }
  hdr <- colnames(data)
  if (is.null(hdr))
    stop ('data must have colnames')
  
  #creating newdataframe final.data to be used for index calculations
  #if time is null, assume all data belongs to one time frame named TimePeriod
  if (is.null(time)){
    row.number <- nrow(data)
    time.column <- as.data.frame(rep("TimePeriod", row.number))  
  }
  else {
    time.number <- which(colnames(data)==time )
    time.column <- data[, time.number, drop=FALSE]
    data[, time.number] <- NULL
  }
  #if formula missing, take rowname as entity name and all data column as indicators
  if (missing(formula)){
    rownames.as.entity <- as.data.frame(rownames(data))
    eid.indicators <- cbind(rownames.as.entity, data)
  }
  else{
    eid.indicators <- model.frame(formula, data, na.action = NULL)
  }
  
  final.data <- cbind(time.column, eid.indicators)
  
  # complete the data
  final.data <- CompleteData(final.data, ctr)
  
  # clean the data
  final.data <- CleanData(final.data, ctr)
  
  # take a subset of data if year provided
  if (!is.null(for.period)){
    pos <- !is.na(match(as.character(final.data[,1]), as.character(for.period)))
    if (!any(pos)){
      stop('no period corresponding to this period in data')
    }
    final.data <- final.data[pos,]
    #data = droplevels(data)
  }
  
  # retrive entity names and timeperiod
  entity  <- unique(as.character(final.data[,2]))
  nentity <- length(entity)
  time.period <- unique(as.character(final.data[,1]))
  time.length <- length(time.period)
  
  # initialization
  index <- ranking <- matrix(data = NA, nrow = nentity, ncol = time.length)
  dimnames(index) <- dimnames(ranking) <- list(entity = entity, time.period = time.period)
  time.list <- list()
  
  # compute index for each time period
  for (i in 1 : time.length){
    tmp <- ComputeSinglePeriodIndex(final.data, time.period[i], ctr)
    pos <- match(rownames(tmp$normindicators), entity) # position of entity in storage matrix
    index[pos,i] <- tmp$index
    ranking[pos,i] <- tmp$ranking
    time.list[[i]] <- list(time.period = time.period[i], index = tmp$index, ranking = tmp$ranking, 
                           indicators = tmp$indicators, normindicators = tmp$normindicators, weights = tmp$weights)
  }
  
  # change in ranking if more than two years
  changeinranking <- NULL
  if (time.length > 1){
    tmp <- apply(ranking[, seq(from = time.length, by = -1, to = 1)], 1, diff)
    changeinranking <- matrix(data = tmp, nrow = nentity, ncol = time.length - 1, byrow = FALSE)
    dimnames(changeinranking) <- list(entity = entity, time.period = time.period[2:time.length])
  }
  else{
    changeinranking <- matrix(data= "none", nrow = nentity, ncol = time.length - 1, byrow = FALSE)
  }
  
  out <- list(index = index, ranking = ranking, changeinranking = changeinranking, time.list = time.list)
  out$call <- match.call()
  out$ctr <- ctr
  class(out) <-  "compind"
  out 
}

####################################################
## Compute the CIP for a given time.period
ComputeSinglePeriodIndex <- function(data, time.period, ctr){
  # subset of data for the selected years
  seldata <- data[as.character(data[,1]) == as.character(time.period),]
  #seldata <- droplevels(seldata)
  
  # determine entity for the time period
  entity  <- as.character(seldata[,2])
  nentity <- length(entity)
  if (length(unique(entity)) != nentity){
    stop('several time same entity for same time period')
  }
  # select indicators
  indicators <- seldata[,3:ncol(seldata)]
  rownames(indicators) <- entity
  
  # normalization of indicators
  normindicators <- NormalizeIndicators(indicators, ctr)
  
  # weights for indicators
  weights <- ComputeWeights(normindicators, ctr)
  
  # aggregation of indicators
  pos <- colSums(weights)>0
  # avoid that NA of indicator not in definition propagates
  
  if (ctr$aggregation == 'linear'){
    if(ctr$missingindicator == 'reweight'){
      index <- apply(normindicators[,pos] * weights[,pos], 1, sum, na.rm = TRUE)
    }else{
      index <- apply(normindicators[,pos] * weights[,pos], 1, sum, na.rm = FALSE)
    } 
  }
  
  else if (ctr$aggregation == 'geometric'){
    if(ctr$missingindicator == 'reweight'){
      index <- apply(normindicators[,pos]^weights[,pos], 1, prod, na.rm = TRUE)
    }else{
      index <- apply(normindicators[,pos]^weights[,pos], 1, prod, na.rm = FALSE)
    }        
  }
  else{
    stop('control$method not well defined')
  }
  ranking <- rank(-index, na.last = "keep")
  #index <- round(index,4)
  
  out <- list(index = index, ranking = ranking, indicators = indicators , 
              normindicators = normindicators, weights = weights)
 
  return(out)
}



