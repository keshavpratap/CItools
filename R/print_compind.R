###################################################################
#  Print for compind
# Arg:
#  object belonging to compind class
#
# Returns:
#  Tells the type of controls used and produces the ranks predicted finally. 

print.compind <- function(x)
{
  cat("\nCall:\n", deparse(x$call), "\n", sep = "")
  cat("\nIndex is calculated using", x$ctr$aggregation[1], "aggregation,", x$ctr$normalization[1], "normalization technique,", 
      x$ctr$weights[1], "weights used. \nMissing values treated using", x$ctr$missingindicator[1], "method and cleaning method used was", x$ctr$cleaning[1], " \n")
  if (!is.null(x$ctr$setmax) & !is.null(x$ctr$setmin)){
    NonNAindex.max <- which(!is.na(x$ctr$setmax))
    maxcol <- colnames(x$time.list[[1]]$indicators[NonNAindex.max])
    max.vals <- x$ctr$setmax[NonNAindex.max]
    NonNAindex.min <- which(!is.na(x$ctr$setmin))
    mincol <- colnames(x$time.list[[1]]$indicators[NonNAindex.min])
    min.vals <- x$ctr$setmin[NonNAindex.min]
    cat("Indicator", maxcol , "are capped at maximum value of", max.vals, ".\n")
    cat("Indicator", mincol , "are set at minimum value of", min.vals, ".\n")
    
  }
  if (!is.null(x$ctr$setmax) & is.null(x$ctr$setmin)){
    NonNAindex.max <- which(!is.na(x$ctr$setmax))
    maxcol <- colnames(x$time.list[[1]]$indicators[NonNAindex.max])
    max.vals <- x$ctr$setmax[NonNAindex.max]
    cat("Indicator", maxcol , "are capped at maximum value of", max.vals, ".\n")   
  }
  
  if (is.null(x$ctr$setmax) & !is.null(x$ctr$setmin)){
    NonNAindex.min <- which(!is.na(x$ctr$setmin))
    mincol <- colnames(x$time.list[[1]]$indicators[NonNAindex.min])
    min.vals <- x$ctr$setmin[NonNAindex.min]
    cat("Indicator", mincol , "are set at minimum value of", min.vals, ".\n")
  }
  
  cat("\nRanked countries:\n")
  ret <- x$ranking[order(x$ranking[,1]),, drop=FALSE]
  print(ret)
}
