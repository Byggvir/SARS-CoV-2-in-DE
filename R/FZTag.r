#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# last Change: 2021-06-25
#
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RKI"

require(data.table)
library(tidyverse)
library(REST)

# Set Working directory to git root

if (rstudioapi::isAvailable()){
  
  # When called in RStudio
  SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
  
} else {
  
  #  When called from command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-1)],collapse='/')
setwd(WD)

PNG <- "png/Fallzahlen_Tag_Bund"

# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust the data.
# The daily cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

source("R/lib/copyright.r")
source("R/lib/myfunctions.r")
source("R/lib/sql.r")

options( 
    digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
  )

#  Funktion zum Zeichnen des Diagrammes

diagram <- function (
  SQL
  , main = "CoViD-19 DE: Tägliche vom RKI gemeldete Fälle bis"
  , N = 1
) {

  # Holen der Daten aus der Datenbank
  
  daily <- RunSQL(SQL = SQL, prepare = 'set @i:=0;')

  m <- length(daily[,1])

  png(  
    paste( PNG, N, '.png', sep="" )
    , width=1920
    , height=1080
  )

  par(mfcol=c(2,1))

  colors <-c( "red", "yellow", "green", "blue", "black" )

  today <- Sys.Date()
  heute <- format(today, "%d %b %Y")
  startdate <- as.Date("2020-02-24")
  reported <- daily$Date[m]


  barplot(
           as.numeric(daily$AnzahlFall[1:m]) # [fromto]
         , ylim= limbounds(daily$AnzahlFall[1:m])
         , main = "" 
         , sub = ""
         , xlab = ""
         , col=c(rep("lightblue",6),"red")
         , ylab="Anzahl"
         #, names.arg = Tage # [fromto]
         , las = 2
  )

  title ( 
    main = paste( main, "Fälle bis", reported) 
    , line = 3
    , cex  = 5
  )

  
  title ( 
    sub = paste("Created:", heute )
    , line= 3
    , cex = 4
  )

  title ( 
    xlab = paste("Datum" )
    , line= 1
    , cex = 4
  )

  grid()

  barplot( as.numeric(daily$AnzahlTodesfall[1:m]) # [fromto]
         , ylim = limbounds(as.numeric(daily$AnzahlTodesfall[1:m]))
         , main = ""
         , sub = ""
         , xlab=""
         , col=c(rep("lightblue",6),"red") 
         , ylab="Anzahl"
         #, names.arg = Tage # [fromto]
         , las = 2
  )

  title ( 
    main = paste( main, "Todesfälle bis", reported) 
    , line = 3
    , cex  = 5
  )

  title ( 
    sub = paste("Created:", heute )
    , line= 3
    , cex = 4
  )
  title ( 
    xlab = paste("Datum" )
    , line= 1
    , cex = 4
  )
  grid()

  copyright()

  dev.off()

  return (daily)

}

daily <- diagram( 
  SQL = 'select Meldedatum as Meldedatum, sum(AnzahlFall) as AnzahlFall, sum(AnzahlTodesfall) as AnzahlTodesfall from Faelle where Meldedatum >="2020-02-24" group by Meldedatum;'
  , main = "CoViD-19 DE: Tägliche beim Gesundheitsamt gemeldete"
  , N = "B" )
