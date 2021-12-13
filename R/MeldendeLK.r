#!usr/bin/env Rscript
#
# Script: Meldende Landkreise

# Stand: 2021-08-03
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

require(data.table)
library(tidyverse)
library(REST)
#library(ggplot2)

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

MyScriptName <- "MeldendeLK"

source("R/lib/copyright.r")
source("R/lib/myfunctions.r")
source("R/lib/sql.r")

today <- Sys.Date()
heute <- format(today, "%Y-%m-%d")

par ( mar = c(10,10,10,10)
      , mfcol = c(2,1)
      )

png( paste( "png/", MyScriptName, heute, ".png", sep = "")
    , width = 1920
    , height = 1080)
par ( mar = c(10,10,10,10)
      , mfcol = c(1,1)
)

# ---- Erstes Diagramm

SQL <- paste(
  'SELECT Meldedatum,
    count(IdLandkreis) 
  FROM (
  SELECT IdLandkreis as IdLandkreis,
      max(Meldedatum) as Meldedatum
  FROM Faelle as A
  GROUP BY IdLandkreis ) as B GROUP BY Meldedatum;'
    , sep = "")

LetzteMeldung <- RunSQL(SQL = SQL)

# par ( new = TRUE)

bp2 <- barplot(LetzteMeldung[,2]
        , main = "Meldende Landkreise"
        , cex.main = 4
        , cex.axis = 2
        , cex.names = 2
        , sub = ""
        , names.arg = LetzteMeldung[,1]
        , col = "cyan"
        , las = 1
        , xlab = ""
        , ylab = ""
        , ylim = limbounds(LetzteMeldung[,2]) * 1.1
)

title ( sub = paste( "Datum letzte Fallmeldung des GA ans RKI")
        , cex.sub = 2
)

text( bp2
      , LetzteMeldung[,2] 
      , paste( LetzteMeldung[,2], "\n" , round(LetzteMeldung[,2]/4.12,1) , "%" , sep ='')
      , cex = 2
      , adj = 0.5
      , pos = 3
      , offset = 0.2
      
)

mtext( paste("Datenbestand", format(today, "%Y-%m-%d"))
      , side = 4
      , outer = FALSE
      , line = 3
      , cex = 2
)
             
grid()

copyright()
 
dev.off()
