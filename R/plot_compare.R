###################################################################
#  Plot for compare
# Arg:
#  object belonging to compare
#
# Returns:
#  Plots the values for compare results 

plot.compare <- function(x)
{par(mfrow = c(1,1))
 image(x$AvIndexDifference)
}