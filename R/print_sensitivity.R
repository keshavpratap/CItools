###################################################################
#  Print for sensitivity
# Arg:
#  object belonging to sensitivity
#
# Returns:
#  Prints the values for sensitivity results 

print.sensitivity <- function(x)
{
  cat("\nCall:\n", deparse(x$call), "\n", sep = "")
  cat("\nThe vector of marginal sensitivities is \n")
  print(x$sensitivity)
  cat("\nThe vector of component sensitivities is \n")
  print(x$component)
}