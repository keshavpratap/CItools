#################################################################
## Compare index between K countries
# Args:
#   computed index of compind class
#   benchmark is also computed index of a particul time period of class compind
#
# Returns:
#   A list of Index Difference, AvIndexdifference, AbsRankdifference, 
#   AvAbsRanKDifference and AvRankCorrelation 

compare <- function(input, ...) UseMethod("compare")

compare.compind <- function(input, bench= NULL){
  
  nlist <- length(input$time.list)
  entity <- rownames(input$ranking)
  time.period <- colnames(input$ranking)
  nentity <- length(entity)
  
  # aggregated measure over years
  IndexDifference <- array(data = NA, c(nentity, nentity, nlist))
  dimnames(IndexDifference) <- list(entity = entity, entity = entity, year = time.period)
  
  # D_AB measure over time (difference in values of the composite indicator)
  for (time.interval in 1 : nlist){
    index.interval <- input$time.list[[time.interval]]
    tmp.index   <- input$time.list[[time.interval]]$index
    #
    #entityt = rownames(index.interval$normindicators)
    #nentityt = length(entityt)
    entityt <- names(tmp.index)
    nentityt <- length(entityt)    
    for (i in 1 : (nentityt-1)) {
      entityti <- entityt[i]
      posit <- which(entityt == entityti)
      posi  <- which(entity == entityti)
      for (j in ((i+1) : nentityt)) {
        entitytj <- entityt[j]
        posjt <- which(entityt == entitytj)
        posj  <- which(entity == entitytj)
        if (length(posit == 1) && length(posjt == 1)){
          #tmpi <- index.interval$normindicators[posit,] * index.interval$weights[posit,]
          #tmpj <- index.interval$normindicators[posjt,] * index.interval$weights[posjt,] 
          tmpi <- tmp.index[posit] 
          tmpj <- tmp.index[posjt] 
          # store in big matrix
          # IndexDifference[posi,posj,time.interval] = sum(tmpi - tmpj, na.rm = TRUE)
          IndexDifference[posi,posj,time.interval] = tmpi - tmpj
        }
      }
    }
  }
  AvIndexDifference <- round(apply(IndexDifference, c(1,2), mean, na.rm = TRUE),4)
  
  # RS measure (if bench available)
  AbsRankDifference = AvAbsRankDifference = AvRankCorrelation = NULL
  if (!is.null(bench)){
    if (!is.list(bench))
      stop ('bench must be a list')
    if (!any(match(names(bench), 'time.list')))
      stop ('time.list must be present in bench')
    if (!is.list(bench$time.list))
      stop ('time.list must be a list')      
    AbsRankDifference <- rep(NaN, nlist)
    RankCorrelation <- rep(NaN, nlist)
    tmpbench <- bench$time.list[[1]]$ranking # single year benchmark
    for (time.interval in 1 : nlist){
      tmp.index <- input$time.list[[time.interval]]$ranking
      nams <- intersect(names(tmp.index), names(tmpbench))
      idxindex <- !is.na(match(names(tmp.index), nams))
      idxbench <- !is.na(match(names(tmpbench), nams))
      if (sum(idxindex) > 1 && sum(idxbench) > 1){
        AbsRankDifference[time.interval] <- mean( abs(tmp.index[idxindex] - tmpbench[idxbench]), na.rm = TRUE)
        RankCorrelation[time.interval] <- cor( tmp.index[idxindex] , tmpbench[idxbench] , use="complete.obs")        
      }
    }
    AvAbsRankDifference <- mean(AbsRankDifference, na.rm = TRUE)    
    AvRankCorrelation   <- mean(RankCorrelation, na.rm = TRUE)
  }
  out = list(IndexDifference     = IndexDifference, 
             AvIndexDifference   = AvIndexDifference,
             AbsRankDifference   = AbsRankDifference,
             AvAbsRankDifference = AvAbsRankDifference,
             AvRankCorrelation   = AvRankCorrelation)
  
  out$call <- match.call()
  out$bench <- bench
  out$time.period <- time.period
  class(out) <- "compare"
  return(out)
} 
