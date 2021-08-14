#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

require(data.table)
library(REST)
library(tidyverse)

MyScriptName <-"RKI"
PNG <- "png/Fallzahlen_Wo_"

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

source("R/lib/sql.r")
source("R/lib/copyright.r")
source("R/lib/myfunctions.r")

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

SQL <- 'select * from Bundesland order by IdBundesland;'
BL <- RunSQL(SQL = SQL)

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")


for (i in BL[,1]) {

  png(  paste( PNG, BL[i,2],".png", sep="")
        , width = 1920
        , height = 1080
  )
  par(mfcol = c(2,1))
  
  SQL = paste ('call CasesPerWeekBL(',i,' );', sep ="")

  weekly <- RunSQL(SQL = SQL)
  write.csv(weekly,file=paste("data/Fallzahlen_Wo_", BL[i,2],".csv", sep=""))
  
  m <- length(weekly[,1])
  
  labs <- weekly$Kw
  j20 <- weekly$Kw < 54
  j21 <- weekly$Kw > 53
  
  labs[labs>53] <- labs[j21] - 53
  labs[j20] <- paste(labs[j20],20,sep='/')
  labs[j21] <- paste(labs[j21],21,sep='/')
  
  reported <- weekly$Kw[m]
  y <- as.numeric(weekly$AnzahlFall[1:m]) 
  bp1 <- barplot( y # [fromto]
         , ylim = limbounds(y)*1.2
         , main = paste("Weekly cases from calendarweek", weekly$Kw[1], "until", reported) 
         , sub = ""
         , xlab = ""
         , col = "lightblue"
         , ylab = "Anzahl"
         , names.arg = labs
         , las = 2
  )

  title ( sub = BL[i,2], line = 4, cex.sub = 1.5 )

  text( bp1
      , y
      , round(y)
      , cex = 1.5
      , pos = 3
      , offset = 3
      , srt = 90
  )
  abline( h = y[m-1]
          , col = "red"
          , lty = 3
  )
  
  abline( h = max(y)
          , col = "red"
          , lty = 3
  )
  
  grid()

  y <- as.numeric(weekly$AnzahlTodesfall[1:m])
  bp2 <- barplot( y # [fromto]
         , ylim = limbounds(y)*1.2
         , main = paste("Weekly deaths from calendarweek", weekly$Kw[1], "until", reported) 
         , sub = ""
         , xlab = ""
         , col = "lightblue" 
         , ylab = "Anzahl"
         , names.arg = labs
         , las = 2
  )

  title ( sub = BL[i,2], line = 4, cex.sub=1.5)

  text( bp2
      , y 
      , round(y)
      , cex = 1.5
      , pos = 3
      , offset = 2
      , srt = 90
)

abline( h=y[m-1]
        , col = "red"
        , lty = 3
        , lwd = 0.2
)

copyright()

dev.off()
}
