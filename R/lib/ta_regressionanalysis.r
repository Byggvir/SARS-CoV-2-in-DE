#
# ---- Zeichnen des Ergebnisses einer exponentiellen Regressionsanalyse auf einem Plot
#

Wochentage <- c("Mo","Di","Mi","Do","Fr","Sa","So")

f_exp <- function (x,a,b) {
  
  return(exp(a+b*x))

}

f_lin <- function (x,a,b) {
  
  return (a+b*x)
  
}

plotregression <- function( a , b, xlim = c(0,1), ylim = c(0,3), linecol = c("green","orange","red"), is_log=TRUE) {
  
  if (is_log == TRUE) {
    
    f <- f_exp
    lty <- 1
  } else {
    
    f <- f_lin
    lty <- 3
  }

    
  for (i in 1:3 ) {
    par (new = TRUE)    
    curve( f(x,a[i],b[i])
           , from = xlim[1]
           , to = xlim[2]
           , col = linecol[i]
           , axes = FALSE
           , xlab = ""
           , ylab = ""
           , xlim = xlim
           , ylim = ylim
           , lwd = 3
           , lty = lty
    )
    text( xlim[2]
          , f(xlim[2],a[i],b[i])
          , round(f(xlim[2],a[i],b[i]),0)
          , col = linecol[i]
          , adj = 1
          , cex = 1
          , pos = 4
    )
    
  }
  
}

regression_label <- function( x , a , b, xlim = c(0,1), ylim = c(0,3), linecol = c("green","orange","red")) {
  
  for (i in 1:3 ) {
    text( x
          , exp(a[i]+b[i]*x)
          , round(exp(a[i]+b[i]*x),0)
          , col = linecol[i]
          , adj = 0.5
          , cex = 1
    )
    
  }
  
}

# ----
