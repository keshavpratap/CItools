#################################################################
## Process control parameters for user to input
#
# Args:
#   Methods for aggregation, normalization, weights, missingindicators, cleaning
#   Default is the first argument in each. 
#   For fixed weight, the user has to input weights in the form c(fixed, w1, w2, w3...)
#   setmax and setmin are used to fix the max and min values in min-max normalisation  
#   setmax and setmin to be entered in the form (NA, NA, NA, maxval1, maxval2,..)
#   The length of setmax, setmax need to be equal to number of indicators 
#
# Returns:
#   List of controls the user enetered.

ProcessControl <- function(aggregation = c('geometric' , 'linear'),
                           normalization = c('min-max', 'none', 'z-score', 'robust-z-score'), 
                           weights = c( 'equal', 'random', 'fixed'),
                           missingindicator = c('last','none','reweight', 'linear'), 
                           cleaning = c('none', 'hampel_twosided' , 'hampel_onesided'),      
                           setmax = NULL,
                           setmin = NULL)
{
  return(list(aggregation = match.arg(aggregation), 
              normalization = match.arg(normalization),
              weights = weights,
              missingindicator = match.arg(missingindicator),
              cleaning = match.arg(cleaning),
              setmax = setmax,
              setmin=setmin
  ))    
}

