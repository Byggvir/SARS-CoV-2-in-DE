#!usr/bin/env Rscript
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <- "MeldedatumNeufall"

library(tidyverse)
require(data.table)
library(REST)
library(ggplot2)

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
      , mfcol = c(2,1)
)
SQL <- paste( '
SELECT distinct A.Meldedatum, dayofweek(A.Meldedatum),B.Infizierte
FROM Faelle as A
LEFT JOIN (SELECT Meldedatum, sum(AnzahlFall) AS Infizierte
FROM Faelle as C WHERE NeuerFall <> 0 GROUP BY Meldedatum) AS B
ON A.Meldedatum=B.Meldedatum where A.Meldedatum > "' , as.character(today-15), '";' , sep = "" )

NeueFaelle <-sqlGetRKI(SQL = SQL)
NeueFaelle[is.na(NeueFaelle[,3]),3] <- 0

# NeueFaelle[is.na(NeueFaelle[,3]),3] <- 0

mytags <- as.character(NeueFaelle[,1])

mytags[NeueFaelle[,2] != 2 ] <- ""

bp1 <- barplot(NeueFaelle[,3]
        , main = "Neufälle des letzten Tages nach Meldedatum GA"
        , cex.main = 4
        , cex.axis = 2
        , cex.names = 2
        , sub = ""
        , names.arg = mytags
        , col = "cyan"
        , las = 1
        , xlab = ""
        , ylab = ""
        , ylim = limbounds(NeueFaelle[,3]*1.2)
)

s <- sum(NeueFaelle[,3], na.rm = TRUE)

title ( sub = paste( "Summe =", s, "(ohne Fälle außerhalb des Zeitraums)")
        , cex.sub = 2
)
       
text( bp1
      , NeueFaelle[,3] 
      , paste(round(NeueFaelle[,3]/s *100, 2), "%", sep = "")
      , cex = 2
      , adj = 0.5
      , pos = 3
      , offset = 0.2
      
)
grid()

# ---- Zweites Diagramm

SQL <- paste('SELECT distinct A.Refdatum, dayofweek(A.Refdatum),B.Infizierte 
  FROM Faelle as A 
  LEFT JOIN (SELECT Refdatum, sum(AnzahlFall) AS Infizierte 
    FROM Faelle as C WHERE IstErkrankungsbeginn = 1 and NeuerFall <> 0 GROUP BY Refdatum) AS B
    ON A.Refdatum = B.Refdatum where A.Refdatum > "'
    , as.character(today-15)
    , '";'
    , sep = "")

NeueFaelle <-sqlGetRKI(SQL = SQL)
NeueFaelle[is.na(NeueFaelle[,3]),3] <- 0

# par ( new = TRUE)

bp2 <- barplot(NeueFaelle[,3]
        , main = "Neufälle des letzten Tages nach Erkrankungsbeginn GA"
        , cex.main = 4
        , cex.axis = 2
        , cex.names = 2
        , sub = ""
        , names.arg = mytags
        , col = "cyan"
        , las = 1
        , xlab = ""
        , ylab = ""
        , ylim = limbounds(NeueFaelle[,3]*1.2)
)

s <- sum(NeueFaelle[,3], na.rm = TRUE)


title ( sub = paste( "Summe =", s, "(ohne Fälle außerhalb des Zeitraumes)")
        , cex.sub = 2
)

text( bp2
      , NeueFaelle[,3] 
      , paste(round(NeueFaelle[,3]/s *100, 2), "%", sep = "")
      , cex = 2
      , adj = 0.5
      , pos = 3
      , offset = 0.2
    
)

mtext( paste("Datenbestand", as.character(today))
      , side = 4
      , outer = FALSE
      , line = 3
      , cex = 2
)
             
grid()

copyright()
 
dev.off()
