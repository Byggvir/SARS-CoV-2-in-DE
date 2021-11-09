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

WD <- paste(SD[1:(length(SD)-1)],collapse='/')

setwd(WD)

fPNG <- "png/ImpfSchutzTod"

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

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

do_plot <- function (CFR, rCFR , CFRu, Q = NA , AG = 'A80+' ) {

  if ( nrow(Q) == nrow(rCFR) ) {
    p <- 2
  } else {
    p <- 1
  }
  png(  paste(fPNG, AG,p,'.png',sep='')
      
    , width = 1920
    , height = 1080
    )

  par( 
      mar = c(10,10,10,10)
    , mfcol = c(1,1)
    )

  colors <-c( "red", "yellow", "green", "blue", "black" )

bp1 <- plot(
           CFR[,1]
         , CFR[,4] * 100
         , ylim = limbounds(CFR[,4] * 120)
         , main = paste("Case Fatality Rate (CFR) Deutschland") 
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

polygon(
  c(CFR[,1], rev(CFR[,1]))
  , c((CFR[,4] + CFR[,5]*1.96)* 100, rev((CFR[,4] - CFR[,5]*1.96)* 100))
  , col = rgb(0,0,1, 0.1)
  , lwd = 0.1
  , lty = 1
)

lines(
  rCFR[,1]
  , rCFR[,4] * 100
  , col = "black"
  , lwd = 5
  , lty = 1
)

polygon(
  c(rCFR[,1], rev(rCFR[,1]))
  , c((rCFR[,4] + rCFR[,5]*1.96)* 100, rev((rCFR[,4] - rCFR[,5]*1.96)* 100))
  , col = rgb(0,0,0,0.1)
  , lwd = 0.1
  , lty = 2
)

abline (
  h=CFRu*100
  , col = "red"
  , lwd = 4
)
title ( sub = paste('Altersgruppe' , AG ), line = 5, cex.sub = 2)

if ( nrow(Q) == nrow(rCFR) ){
  
  CFRg <- (rCFR[,4] * (1-S*Q[,4]) - (1 - Q[,4])*CFRu[1,1])/Q[,4]/(1-S)
  lines(
    rCFR[,1]
    , CFRg * 100
    , col = "green"
    , lwd = 5
    , lty = 1
  )
  CFRgl <- ((rCFR[,4] - rCFR[,5]*1.96)  * (1-S*Q[,4]) - (1 - Q[,4])*CFRu[1,1])/Q[,4]/(1-S)
  CFRgh <- ((rCFR[,4] + rCFR[,5]*1.96)  * (1-S*Q[,4]) - (1 - Q[,4])*CFRu[1,1])/Q[,4]/(1-S)
  
  polygon(
    c(rCFR[,1], rev(rCFR[,1]))
    , c(CFRgl, rev(CFRgh)) * 100
    , col = rgb(0,1,0,0.1)
    , lwd = 0.1
    , lty = 2
  )
  legend (  
    "topright"
    , legend = c('Rohe CFR','CFR der letzten 6 Wochen', 'CFR Geimpfte', 'CFR Ungeimpfte')
    , col = c("blue","black","green","red")
    , lty = 1
    , lwd = 2
    , cex = 2
    , inset = 0.05
  ) 
           
  } else {
    legend (
      "topright"
      , legend = c('Rohe CFR', 'CFR der letzten 6 Wochen', 'CFR Ungeimpfte')
      , col = c("blue","black","red")
      , lty = 1
      , lwd = 2
      , cex = 2
      , inset = 0.05
      )
  
  
}

grid()

copyright()

dev.off()

}

IdBundesland <- 0

S <- 0.4

for ( AG in c('A60-A79','A80+') ) {
  
  SQL <- paste ('select CFR as CFR from CFR where'
      , ' IdBundesland = '
      , IdBundesland
      , ' and Altersgruppe="'
      , AG ,'"'
      , ' and Meldedatum = "2020-12-31"'
      , ';'
      , sep ='')

  CFRu <- RunSQL(SQL = SQL)
  print(CFRu)
  
  SQL <- 'select * from avgImpfQuote where AlterVon = 60 and Quote>0.2;'
  Q <- RunSQL(SQL = SQL)
  print(Q[Q[,1]=="2021-08-01",])
  
  SQL <- paste (
    'select * from CFR where'
    , ' IdBundesland = '
    , IdBundesland
    , ' and Altersgruppe = "'
    , AG ,'"'
    , ' and Meldedatum >= "2020-04-01"'
    , ';'
    , sep ='')
  
  CFR <- RunSQL(SQL = SQL)
  
  SQL <- paste(
    'select * from RollendeCFR where'
    , ' IdBundesland = '
    , IdBundesland
    , ' and Altersgruppe = "'
    , AG ,'"'
    , ';'
    , sep ='')
  
  rCFR <- RunSQL(SQL = SQL)
  print(rCFR[rCFR[,1]=="2021-08-01",])
  
  do_plot(CFR, rCFR, CFRu, Q, AG)

  SQL <- paste(
    'select * from CFR where'
    , ' IdBundesland = '
    , IdBundesland
    , ' and Altersgruppe = "'
    , AG ,'"'
    , ' and Meldedatum >= "'
    , Q[1,1]
    , '";'
    , sep=''
    )

  CFR <- RunSQL(SQL = SQL)
  
  SQL <- paste(
    'select * from RollendeCFR where'
    , ' IdBundesland = '
    , IdBundesland
    , ' and Altersgruppe = "'
    , AG ,'"'
    , ' and Meldedatum >= "'
    , Q[1,1]
    , '";'
    , sep=''
    )
  rCFR <- RunSQL(SQL = SQL)

  do_plot(CFR, rCFR, CFRu, Q, AG)

}
