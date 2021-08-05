#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZWoche"

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

fPNG <- "png/Fallzahlen_Wo_Bund.png"

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
SQL = 'call CasesPerWeek()'

weekly <- RunSQL(SQL = SQL)
m <- length(weekly[,1])
reported <- weekly$Kw[m]


png(  fPNG
    , width = 1920
    , height = 1080
    )

par(mfcol = c(2,1))

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

y <- as.numeric(weekly$AnzahlFall[1:m])

labs <- weekly$Kw
j20 <- weekly$Kw < 54
j21 <- weekly$Kw > 53

labs[labs>53] <- labs[j21] - 53
labs[j20] <- paste(labs[j20],20,sep='/')
labs[j21] <- paste(labs[j21],21,sep='/')


bp1 <- barplot( y # [fromto]
         , ylim = limbounds(y)*1.1
         , main = paste("Wöchentliche Fälle von Kalenderwoche", weekly$Kw[1], "bis", reported) 
         , sub = ""
         , xlab = ""
         , col = "lightblue"
         , ylab = "Anzahl"
         , names.arg = labs
         , las = 2
)

title ( sub = paste("Source: rki.de; Created:", heute ), line = 3)

text( bp1
      , y
      , round(y)
      , cex = 1
      , pos = 3
      , offset = 3
      , srt = 90
)

abline(h=y[m-1] , col = 'red')

abline(h=max(y) , col = 'red')

grid()

y <- as.numeric(weekly$AnzahlTodesfall[1:m])
bp2 <- barplot( y # [fromto]
         , ylim = limbounds(y)*1.1
         , main = paste("Wöchentliche Todesfälle DE von Kalenderwoche", weekly$Kw[1], "bis", reported) 
         , sub = ""
         , xlab = ""
         , col = "lightblue" 
         , ylab = "Anzahl"
         , names.arg = labs
         , las = 2
)

abline(h=y[m-1] , col = 'red' , lty = 3)
abline(h=max(y) , col = 'red' , lty = 3)

title ( sub = paste("Source: rki.de; Created:", heute ), line = 3)

text( bp2
      , y 
      , round(y)
      , cex = 1
      , pos = 3
      , offset = 1
      , srt = 90
)

copyright()

dev.off()
