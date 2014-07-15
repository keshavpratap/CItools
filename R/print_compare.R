###################################################################
#  Print for compare
# Arg:
#  object belonging to compind compare
#
# Returns:
#  Prints the values for compare results 

print.compare <- function(x)
{
  cat("\nCall:\n", deparse(x$call), "\n", sep = "")
  AbsAvIndexDifference <- abs(x$AvIndexDifference)
  inds.max= which(AbsAvIndexDifference == max(AbsAvIndexDifference, na.rm=TRUE), arr.ind = TRUE)
  rnames.max = rownames(AbsAvIndexDifference)[inds.max[,1]]
  cnames.max = colnames(AbsAvIndexDifference)[inds.max[,2]]
  inds.min= which(AbsAvIndexDifference == min(AbsAvIndexDifference, na.rm=TRUE), arr.ind = TRUE)
  rnames.min = rownames(AbsAvIndexDifference)[inds.min[,1]]
  cnames.min = colnames(AbsAvIndexDifference)[inds.min[,2]]
  
  cat("\nThe maximum average Index Difference is ",  max(AbsAvIndexDifference, na.rm = TRUE), "between", rnames.max, "and", cnames.max, 
       ". \nThe minimum average Index Difference is", min(AbsAvIndexDifference, na.rm = TRUE), "between", rnames.min, "and", cnames.min, 
       ". \n The mean value of the Average Index Difference is", mean(x$AvIndexDifference, na.rm = TRUE), ".")
  if(!is.null(x$bench)){
    names(x$AbsRankDifference) <- x$time.period
    cat("\nThe Absolute Rank Difference is \n")
    print(x$AbsRankDifference)
    cat("\nThe time period avergage vale Absolute Rank Difference is", x$AvAbsRankDifference, 
        ".\nThe timeperiod-average value of correlation between ranks is", x$AvRankCorrelation,".\n")
    
  }
  
}
