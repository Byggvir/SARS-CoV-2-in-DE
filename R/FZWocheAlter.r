#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZWocheAlter"

library(REST)
library(tidyverse)

# Set Working directory to git root

if (rstudioapi::isAvailable()){
  
  # When executed in RStudio
  SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
  
} else {
  
  #  When executi on command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-1)],collapse='/')

setwd(WD)

fPrefix <- "Fallzahlen_Wo_"

# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust the data.
# The weekly cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

require(data.table)

source("R/lib/copyright.r")
source("R/lib/myfunctions.r")
source("R/lib/sql.r")

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

SQL <- 'select distinct Altersgruppe from Faelle where Altersgruppe <> "unbekan";'
Altersgruppen <- RunSQL(SQL)

png(  paste( 'png/',fPrefix, "Bund_Alter.png", sep="")
      , width = 3840
      , height = 2160
)

par(
  mar = c(10,10,10,10)
  , mfrow = c(2,3)
)

for (AG in Altersgruppen[,1]) {

SQL <- paste('call CasesPerWeekAgeGroup("' , AG , '");', sep='')
weekly <- RunSQL(SQL = SQL)

write.csv(weekly,file= paste('data/', fPrefix, AG,".csv", sep=""))

m <- length(weekly[,1])
reported <- weekly$Kw[m]


colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

Zeitraum <- 1:m

max_f <- max(weekly$AnzahlFall[Zeitraum])
sum_f <- sum(weekly$AnzahlFall[Zeitraum])

max_t <- max(weekly$AnzahlTodesfall[Zeitraum])
sum_t <- sum(weekly$AnzahlTodesfall[Zeitraum])

yf <- as.numeric(weekly$AnzahlFall)/max_f*100
yt <- as.numeric(weekly$AnzahlTodesfall)/max_t*100

labs <- weekly$Kw
j20 <- weekly$Kw < 54
j21 <- weekly$Kw > 53

labs[labs>53] <- labs[j21] - 53
labs[j20] <- paste(labs[j20],20,sep='/')
labs[j21] <- paste(labs[j21],21,sep='/')


bp1 <- plot( weekly$Kw[Zeitraum]
          , yf[Zeitraum]
          , type = 'l'
         , ylim = c(0,110)
         , main = "" 
         , sub = ""
         , xlab = "Pandemiewoche"
         , col = "blue"
         , ylab = "Anzahl"
#        , names.arg = labs
         , las = 2
         , lwd = 4
)

lines( weekly$Kw[Zeitraum]
      , yt[Zeitraum]
      , type = 'l'
      , ylim = c(0,110)
      , col = "red"
      , lwd = 4
)
 
title( main = paste("Fälle pro Woche DEU\nvon Pandemiewoche", Zeitraum[1], "bis", reported) 
       , line = 2
       , cex.main = 4.5)

title( sub = paste( "Altersgruppe", AG)
       , line = 6
       , cex.sub = 3)

legend ( 
  "topleft"
  , title = "Index Fallzahlen pro Woche "
  , legend = c(paste("Fälle, max =", max_f, ", ∑ =", sum_f)
               , paste("Todesfälle, max =", max_t, ", ∑ =", sum_t))
  , col = c("blue","red")
  , lwd = 4
  , cex = 2
  , inset = 0.05
)

grid()

}

copyright()

dev.off()
