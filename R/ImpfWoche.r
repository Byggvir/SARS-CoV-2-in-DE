#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"ImpfWoche"

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

fPNG <- "png/Impfungen_Wo_Bund.png"

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
SQL <- 'select sum(Insgesamt) as Anzahl from Bevoelkerung.DEU where Stichtag ="2019-12-31" and Age >= 12 and Age < 18;'
Bev <- RunSQL( SQL = SQL)

SQL <- 'call ImpfungenAlter(12,17)'
ipw <- RunSQL(SQL = SQL)

write.csv(ipw,file="data/Impfungen_Wo_Bund.csv")

m <- length(ipw[,1])
reported <- ipw$Kw[m]


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

y <- cumsum(as.numeric(ipw$Anzahl[1:m]))/Bev$Anzahl * 100

labs <- paste(ipw$Jahr,ipw$Kw, sep='/')

bp1 <- plot(
           1:m
         , y # [fromto]
         , ylim = limbounds(y)*1.1
         , main = paste("Impfquote"
                        , " von", ipw$Jahr[1], 'Kw' , ipw$Kw[1]
                        ,  "bis", ipw$Jahr[m], 'Kw' , ipw$Kw[m]) 
         , sub = ""
         , xlab = ""
         , col = "lightblue"
         , ylab = "%"
         #, names.arg = labs
         , las = 2
         , type = 'l'
         , lwd = 5
)

title ( sub = paste("Created:", heute ), line = 4, cex.sub = 1)

text( 1:m
      , y
      , round(y,2)
  )

grid()

copyright()

dev.off()

# SQL <- 'call CasesPerWeekAgeGroup("A60-A79");'
# cpw <- RunSQL(SQL = SQL)
