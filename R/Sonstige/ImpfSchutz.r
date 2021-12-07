#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2021-08-31
# (c) 2021 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"ImpfSchutz"

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

WD <- paste(SD[1:(length(SD)-2)],collapse='/')

setwd(WD)

fPNG <- "png/ImpfSchutz.png"

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

SQL <- 'select * from Schutz where IdBundesland=0 and Zeitraum = 41 and Datum >="2021-03-01" order by Datum;'
Schutz <- RunSQL(SQL = SQL)

png(  fPNG
    , width = 1920
    , height = 1080
    )

par( 
    mar = c(10,10,10,10)
  , mfcol = c(1,1)
  )

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

bp1 <- plot(
           Schutz[,1]
         , Schutz[,4]
         , ylim = limbounds(Schutz[,4],zeromin=FALSE)
         , main = paste("Impfschutz der Altersgruppe A60-A79 anhand R-Zahl") 
         , sub = ""
         , xlab = ""
         , col = "blue"
         , ylab = "%"
         , las = 2
         , type = 'l'
         , lwd = 5
         , cex.main = 4
         , cex.lab = 3
         , cex.axis = 2
)

polygon ( c(Schutz[! is.na(Schutz[,4]),1], rev(Schutz[! is.na(Schutz[,4]),1]))
  , c(Schutz[! is.na(Schutz[,4]),5],rev(Schutz[! is.na(Schutz[,4]),6]))
  , col = rgb(0,0,0.5,0.2)
)


title ( sub = paste("Vergleich mit Altesgruppe A05-A14 als Ungeimpfte" ), line = 5, cex.sub = 2)

grid()

copyright()

dev.off()
