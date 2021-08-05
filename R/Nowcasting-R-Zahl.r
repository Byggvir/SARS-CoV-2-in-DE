#!/usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <- "Nowcasting-R-Zahl"


require(data.table)
library(tidyverse)
library(REST)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(scales)
library(Cairo)
library(extrafont)
extrafont::loadfonts()

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

source("R/lib/copyright.r")
source("R/lib/myfunctions.r")

require(data.table)
require(date)
library(readODS)
library(REST)
library(gridExtra)
library(grid)
library(lubridate)

png( paste( "png/R-Zahl"
            , ".png"
            , sep = ""
  )
  , width = 1920
  , height = 1080
)

par(  mar = c(10,5,10,5) 
    , bg = rgb(0.95,0.95,0.95,1)
    , mfcol = c(1,1))


rzahlen <- function ( data ) {
  
  ylim <- c(0.5,1.5)
  
  plot( 
         as.Date(data[,1])
       , data[,8]
       , main = ""
       , sub = ""
       , xlab = ""
       , ylab = ""
       , ylim = ylim
       , type = "l"
       , lwd = 5
       , col = "blue"
  )

  abline (h=0.75, col="green")
  abline (h=0.90, col="orange")
  abline (h=1.00, col="red")

  title (
      main = "R-Zahlen in DEU nach RKI"
    , cex.main = 4
    , line = 4
    
  )
  title (
    sub = "7-Tage-R"
  , cex.sub = 3
  , line = 8
  
  )
  title (
    xlab = "Datum"
    , cex.lab = 2

  )
  title (
    ylab = "R-Zahl"
    , cex.lab = 2
    
  )
  legend( "topright"
    , legend = c("7-Tage-R")
    , col = c("blue") 
    , lwd = 5
    , cex = 3
         )
  grid()

}

daten <- read.csv( file='Nowcasting/Nowcast_R_aktuell.csv')

rzahlen(daten[5:nrow(daten),])

dev.off()

