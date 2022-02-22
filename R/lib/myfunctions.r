Wochentage <- c("Mo","Di","Mi","Do","Fr","Sa","So")
WochentageLang <- c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag")

RZahl <- function (b, SeriellesIntervall = 4) {
  
  return (round(exp(SeriellesIntervall*b),3))
  
}

limbounds <- function (x, zeromin=TRUE) {
  
  if (zeromin == TRUE) {
    range <- c(0,max(x,na.rm = TRUE))
  } else
  { range <- c(min(x, na.rm = TRUE),max(x,na.rm = TRUE))
  }
  if (range[1] != range[2])
  {  f <- 10^(floor(log10(range[2]-range[1])))
  } else {
    f <- 1
  }
  
  return ( c(floor(range[1]/f),ceiling(range[2]/f)) * f) 
}

PandemieWoche <- function ( d ) {
  
  if ( is.Date( d ) ) {  
    return( as.integer( d - as.Date( "2019-12-30" ) )  %/% 7 + 1 ) 
  }
  else {
    warning( "Not a date", call. = TRUE)
  }
  
}

get_p_value <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
