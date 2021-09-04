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
source("R/lib/sql.r")

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
  , width = 3840
  , height = 2160
)

par(  mar = c(10,5,10,5) 
    , bg = rgb(0.95,0.95,0.95,1)
    , mfrow = c(2,1))


rzahlen <- function ( daten , Zeitraum ) {
  
  ylim <- c(0.5,1.5)
  
  plot( 
         as.Date(daten[,1])
       , daten[,2]
       , main = ""
       , sub = ""
       , xlab = ""
       , ylab = ""
       , ylim = ylim
       , type = "l"
       , lwd = 5
       , col = "blue"
  )
  
  polygon( 
    as.Date(c(daten[,1], rev(daten[,1])))
    , c(daten[,6],rev(daten[,7]))
    , col = rgb(1,0,0,0.1)
    , border = NA
  )
  polygon( 
    as.Date(c(daten[,1], rev(daten[,1])))
    , c(daten[,3],rev(daten[,4]))
    , col = rgb(0,0,1,0.1)
    , border = NA
  )
  
col <- c("blue","yellow","yellow", "red","green","green")

for ( i in c(2,5) ) {
  lines( 
    as.Date(daten[,1])
    , daten[,i]
    , ylim = ylim
    , type = "l"
    , lwd = 5
    , col = col[i-1]
  )
}
  abline (h=0.75, col="green")
  abline (h=0.90, col="orange")
  abline (h=1.00, col="red")

  title (
      main = "Vergleich R-Zahlen RKI Nowcasting\nmit Regressionsanalyse "
    , cex.main = 4
    , line = 3
    
  )
  title (
    sub = paste("Regressionsanalyse", Zeitraum + 1, "Tage")
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
  legend( "bottomright"
    , title = "R-Zahl"
    , legend = c("7-Tage Nowcasting", paste("Regressionsanalyse", Zeitraum + 1 ,"Tage"))
    , col = c("blue", "red") 
    , lwd = 5
    , cex = 2
    , inset = 0.01
         )
  grid()

}

nowcast <- read.csv( file='Nowcasting/Nowcast_R_aktuell.csv')
m <- nrow(nowcast)

for ( Alter in c('A05-A14','A60-A79') ) {
  
SQL <- paste( 'select Datum, R, Rlow, Rhigh from RZahl'
              , ' where Altersgruppe = "', Alter , '" '
              , ' and IdBundesland=0 '
              , ' and Zeitraum = '
              , 41
              , ' and Datum <= "'
              , nowcast[m-1,1]
              , '" order by Datum;'
              , sep ='')
print(SQL)
daten <- RunSQL(SQL)


dummy <- nowcast[5:(m-1),]
d1 <- dummy[dummy[,1] >= daten[1,1],c(1,8:10)]

d1[,5:7] <- daten[,2:4]

rzahlen(d1,41)

}

dev.off()
