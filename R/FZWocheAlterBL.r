#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZWocheAlterBL"

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

SQL <- 'select * from Bundesland order by IdBundesland;'
Bundesland <- RunSQL(SQL = SQL)

for (BL in Bundesland[,1] ) {

  
  
  png(  paste('png/', fPrefix, Bundesland[BL,2],'_Alter', '.png', sep="")
        , width = 3840
       , height = 2160
  )
  
  par(
    mar = c(10,10,10,10)
    , mfrow = c(2,3)
  )
  
  
for (AG in Altersgruppen[,1]) {

SQL <- paste('call CasesPerWeekAgeGroupBL("' , AG , '",', BL, ');', sep='')
weekly <- RunSQL(SQL = SQL)

write.csv(weekly,file= paste('data/', fPrefix, Bundesland[BL,2], AG,".csv", sep=""))

m <- length(weekly[,1])
reported <- weekly$Kw[m]

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

max_f <- max(weekly$AnzahlFall[1:m])
sum_f <- sum(weekly$AnzahlFall[1:m])

max_t <- max(weekly$AnzahlTodesfall[1:m])
sum_t <- sum(weekly$AnzahlTodesfall[1:m])

yf <- as.numeric(weekly$AnzahlFall[1:m])/max_f*100
yt <- as.numeric(weekly$AnzahlTodesfall[1:m])/max_t*100

labs <- weekly$Kw
j20 <- weekly$Kw < 54
j21 <- weekly$Kw > 53

labs[labs>53] <- labs[j21] - 53
labs[j20] <- paste(labs[j20],20,sep='/')
labs[j21] <- paste(labs[j21],21,sep='/')


bp1 <- plot( weekly$Kw
          , yf
          , type = 'l'
         , ylim = c(0,110)
         , main = "" 
         , sub = ""
         , xlab = "Pandemiewoche"
         , col = "blue"
         , ylab = "Index (Maximum = 100)"
#        , names.arg = labs
         , las = 2
         , lwd = 4
         , cex.axis = 4
         , cex.lab = 3
)

lines( weekly$Kw
      , yt
      , type = 'l'
      , ylim = c(0,110)
      , col = "red"
      , lwd = 4
)
 
title( main = paste("F??lle pro Woche", Bundesland[BL,2], "\nPandemiewoche", weekly$Kw[1], "bis", reported) 
       , line = 2
       , cex.main = 4.5)

title( sub = paste( "Altersgruppe", AG)
       , line = 6
       , cex.sub = 3)

legend ( 
  "topleft"
  , title = "Index Fallzahlen pro Woche "
  , legend = c(paste("F??lle, max =", max_f, ", ??? =", sum_f)
               , paste("Todesf??lle, max =", max_t, ", ??? =", sum_t))
  , col = c("blue","red")
  , lwd = 4
  , cex = 3
  , inset = 0.05
)

grid()
} # End Altersgruppe
  
copyright()

dev.off()


} # End Bundesland
