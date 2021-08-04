
#
#
# Script: averages.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"averages"

#
# Exponential smoothing
#

exponential_smoothing <- function (x, g = 0.1) {
  
  y <- x
  if ( length(x) > 1 & is.numeric(x)) {
    for (i in 2:length(x)) {
      y[i] <- x[i]*g + y[i-1]*(1-g)
    }
  }
  return(y)
}

# End Exponential smoothing
