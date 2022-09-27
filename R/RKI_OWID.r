#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2021-12-10
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#
MyScriptName <-"SouthAfrica"

require(data.table)

library(tidyverse)
#library(REST)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(scales)
library(ragg)
# library(extrafont)
# extrafont::loadfonts()

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


source("R/lib/myfunctions.r")
source("R/lib/mytheme.r")
source("R/lib/sql.r")

citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nOur World in Data"

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

today <- Sys.Date()
heute <- format(today, "%d. %B %Y")

SQL <- 'select * from FaelleProTag;'
RKI <- RunSQL(SQL)

if ( ! exists( "OWID" ) ) {
  
  OWID <- read.csv( file = 'https://covid.ourworldindata.org/data/OWID-covid-data.csv' )
  # OWID <- read.csv( file = 'data/OWID-covid-data.csv' )
  
}
OWID$date <- as.Date(OWID$date)
OWID %>% filter( iso_code == 'DEU' ) -> OWID

SQL <- 'select * from FaelleProTag;'
RKI <- RunSQL(SQL)

OWID %>% ggplot() +
  geom_line( data = OWID %>% filter(! is.na(new_cases) & date > '2021-10-31'), aes( x = date, y = new_cases, colour = 'OWID' ), show.legend = TRUE ) +
  geom_line( data = RKI %>% filter( Meldedatum > '2021-10-31'), aes( x = Meldedatum, y = AnzahlFall, colour = 'RKI' ), show.legend =TRUE )+
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  theme_ta() +
  theme( legend.position = 'bottom' ) +
  labs(  title = "Vergleich Fallzahlen  RKI / OWID"
         , subtitle = paste("Stand:", heute)
         , x = "Datum"
         , y = "Fallzahl" 
         , colour = "Quelle" ) -> pp


ggsave(  filename = paste( 'png/Vergleich.png', sep = '' )
         , path = WD
         , plot = pp
         , device = 'png'
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 300 
         )
