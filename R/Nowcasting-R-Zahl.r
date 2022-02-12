#!/usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

MyScriptName <- "Nowcasting-R-Zahl"

require(data.table)
require(date)
library(lubridate)
library(readODS)
library(tidyverse)
library(REST)
library(grid)
library(gridExtra)
library(gtable)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(scales)
library(ragg)
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

citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

nowcast <- read.csv( file='Nowcasting/Nowcast_R_aktuell.csv')
m <- nrow(nowcast)

SQL <- paste( 'select Datum, Bundesland, R, Rlow, Rhigh from RZahl Z'
              , ' join Bundesland as B on B.IdBundesland = Z.IdBundesland'
              , ' where'
              , ' Altersgruppe = "A0+" '
              , ' and Z.IdBundesland = 0 '
              , ' and Zeitraum = '
              , 20
              , ' and Datum <= "'
              , nowcast[m,1]
              , '" and Datum >= '
              , ' "2021-01-01"'
              , ' order by Datum, B.Bundesland;'
              , sep ='')



daten <- RunSQL(SQL)

daten$RKI<- nowcast[nowcast$Datum >= daten[1,1],8]
daten$RKIlow<- nowcast[nowcast$Datum >= daten[1,1],9]
daten$RKIhigh<- nowcast[nowcast$Datum >= daten[1,1],10]

daten %>% ggplot(
  aes( x = Datum )) +
 #  geom_line( aes( y = R , colour = "Regression" )) +
 #  geom_line( aes( y = Rlow , colour = "Regression UG" )) +
 #  geom_line( aes( y = Rhigh , colour = "Regression OG" )) +
  geom_line( aes( y = RKI , colour = "Nowcast" ), size = 3) +
  geom_line( aes( y = RKIlow , colour = "Nowcast UG" ), size = 2) +
  geom_line( aes( y = RKI , colour = "Nowcast OG" ), size = 2) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  facet_wrap(vars(Bundesland)) +
  labs(  title = "Vergleich R-Zahl Nowcast RKI gegen Regressionsanalyse"
         , subtitle= paste("Stand:", heute, sep ='')
         , x = "Datum"
         , y = "R-Zahl" 
         , colour = "Methode"
         , caption = citation ) +
  theme_ipsum() +
  theme(    axis.text.x  = element_text ( color = 'black', size = 24)
            , axis.title.x = element_text ( color='black', size = 24)
            , axis.text.y  = element_text ( color = 'blue', size = 24)
            , axis.title.y = element_text ( color='blue', size = 24)
            , axis.text.y.right = element_text ( color = 'red', size = 24 )
            , axis.title.y.right = element_text ( color='red', size = 24 )
            , strip.text.x = element_text (
              size = 24
              , color = "black"
              , face = "bold.italic"
            ) ) + 
  theme(plot.title=element_text(size=48, hjust=0.5, face="italic", color="black")) +
  theme(plot.subtitle=element_text(size=36, hjust=0.5, face="italic", color="black")) -> pp

ggsave(  paste('png/R-Zahl.png', sep = '')
         , bg = "white"
         , width = 29.7 * 2
         , height = 21 * 2
         , units = "cm"
         , dpi = 300 )
