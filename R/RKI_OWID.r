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


require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nOur World in Data"

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)
today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- 'select * from FaelleProTag;'
daten2 <- RunSQL(SQL)

daten1 <- read.csv(file = 'data/owid-covid-data.csv')
daten1$date <- as.Date(daten1$date)
daten1 %>% filter( iso_code == 'DEU' ) -> daten1

SQL <- 'select * from FaelleProTag;'
daten2 <- RunSQL(SQL)

daten1 %>% ggplot() +
  geom_line(data = daten1 %>% filter(! is.na(new_cases) & date > '2021-10-31'), aes( x=date, y = new_cases), color = 'blue') +
  geom_line(data = daten2 %>% filter( Meldedatum > '2021-10-31'), aes( x = Meldedatum, y = AnzahlFall), color = 'red') +
  scale_y_continuous(
     labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  theme_ipsum() +
  theme(  plot.title = element_text(size=48)
          , axis.text.y  = element_text ( color = 'blue' )
          , axis.title.y = element_text ( color ='blue' )
          , axis.text.y.right = element_text ( color = 'red' )
          , axis.title.y.right = element_text ( color='red' )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
          ) ) +
  labs(  title = "Vergleich Fallzahlen  RKI / OWID"
         , subtitle = paste(region ," Stand:", heute)
         , x = "Datum"
         , y = "Fälle" 
         , colour = "Fälle")


ggsave(  filename = paste( 'png/', region, '.png', sep = '' )
         , path = WD
         , device = 'png'
         #, type = "cairo-png"
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 300 
         )
