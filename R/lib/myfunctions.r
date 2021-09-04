Wochentage <- c("Mo","Di","Mi","Do","Fr","Sa","So")

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
  {  factor <- 10^(floor(log10(range[2]-range[1])))
  } else {
    factor <- 1
  }
  
  # print(factor)
  return ( c(floor(range[1]/factor),ceiling(range[2]/factor)) * factor) 
}
