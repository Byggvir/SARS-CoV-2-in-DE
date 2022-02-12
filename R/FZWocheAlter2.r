#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZWocheAlter"

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

fPrefix <- "Fallzahlen_Wo_"

# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust the data.
# The weekly cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- paste('select * from FaelleProWocheAltersgruppe;', sep='')

if ( ! exists("weekly") ) {
  weekly <- RunSQL(SQL = SQL)
}

scl <- max(weekly$AnzahlFall) / max(weekly$AnzahlTodesfall)

weekly %>% filter(PandemieWoche < max(PandemieWoche) - 1 ) %>% ggplot(
  aes( x = PandemieWoche )) +
  geom_line(aes(y = AnzahlFall, colour = 'Fall')) +
  geom_line(aes(y = AnzahlTodesfall * scl, colour = 'Todesfall' ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE)
                      , sec.axis = sec_axis( ~./scl, name = "Todesfälle", labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) )  +
  facet_wrap(vars(Altersgruppe)) +
  theme_ipsum() +
  labs(  title = "Wöchentliche Fälle nach Altersgruppe"
       , subtitle= paste("Deutschland, Stand:", heute)
       , x = "Pandemiewoche"
       , y = "Fälle" 
       , colour= 'Legende'
       , caption = citation ) -> pp1

ggsave('png/FZWoche_Fall.png'
       , device = 'png'
       , bg = "white"
       , width = 1920 * 2
       , height = 1080 * 2
       , units = "px"
)

weekly %>% filter( PandemieWoche < max( PandemieWoche ) -1 ) %>% ggplot(
  aes( x = PandemieWoche ) ) +
  geom_line(aes(y = AnzahlTodesfall)) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  facet_wrap(vars(Altersgruppe)) +
  theme_ipsum() +
  labs(  title = "Wöchentliche Todesfälle nach Altersgruppe"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x = "Pandemiewoche"
         , y = "Todesfälle"
         , caption = citation ) -> pp2

ggsave('png/FZWoche_Todesfall.png'
       , device = 'png'
       , bg = "white"
       , width = 1920 * 2
       , height = 1080 * 2
       , units = "px"
)


SQL <- paste('select * from InzidenzAltersgruppe;', sep='')
inzidenz <- RunSQL(SQL = SQL)

idz <-  inzidenz %>% filter ( PandemieWoche < max( PandemieWoche ) - 1 )
scl <- max(idz$AnzahlFall / idz$AnzahlBev * 100000) / max( idz$AnzahlTodesfall / idz$AnzahlBev*100000)

idz %>% ggplot(
  aes( x = PandemieWoche ) ) +
  geom_line(aes(y = AnzahlFall / AnzahlBev * 100000, colour = 'Fall')) +
  geom_line(aes(y = AnzahlTodesfall * scl / AnzahlBev * 100000, colour = 'Todesfall' ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE)
                      , sec.axis = sec_axis( ~./scl, name = "Todesfälle pro 100.000", labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) )  +
  facet_wrap(vars(Altersgruppe)) +
  theme_ipsum() +
  labs(  title = "Wöchentliche Fälle nach Altersgruppe"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x = "Pandemiewoche"
         , y = "Fälle pro 100.000"
         , colour = 'Legende'
         , caption = citation ) -> pp3

ggsave('png/FZWoche_FallInzidenz.png'
       , device = 'png'
       , bg = "white"
       , width = 1920 * 2
       , height = 1080 * 2
       , units = "px"
)


inzidenz %>% filter(PandemieWoche < max(PandemieWoche) -1) %>% ggplot(
  aes( x = PandemieWoche )) +
  geom_line(aes(y = AnzahlTodesfall/AnzahlBev*100000)) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  facet_wrap(vars(Altersgruppe)) +
  theme_ipsum() + 
  labs(  title = "Wöchentliche Todesfälle nach Altersgruppe"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x ="Pandemiewoche"
         , y = "Todesfälle pro 100.000"
         , caption = citation ) -> pp4

ggsave('png/FZWoche_TodesfallInzidenz.png'
,  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)


SQL <- paste('select * from InfektBev;', sep='')
bev <- RunSQL(SQL = SQL)

bev %>% ggplot(
  aes( x = Altersgruppe,y = Anzahl )) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  theme_ipsum() +
  labs(  title = "Personen je Altersgruppe"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x ="Altersgruppe"
         , y = "Anzahl Personen"
         , caption = citation ) -> pp5

ggsave('png/FZWoche_Bev.png'
       , device = 'png'
       , bg = "white"
       , width = 1920 * 2
       , height = 1080 * 2
       , units = "px"
)

SQL <- paste('select * from FaelleProAltersgruppe;', sep='')
fa <- RunSQL(SQL = SQL)

fa %>% ggplot(
  aes( x = Altersgruppe,y = AnzahlFall/AnzahlBev*100 )) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  theme_ipsum() +
  labs(  title = "Anteil Infizierte in der Altersgruppe"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x ="Altersgruppe"
         , y = "Anteil infiziert [%]"
         , caption = citation ) -> pp6

ggsave('png/FZWoche_FallSum.png'
,  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)


fa %>% ggplot(
  aes( x = Altersgruppe,y = AnzahlTodesfall/AnzahlBev*100 )) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  theme_ipsum() +
  labs(  title = "Anteil Todesfälle in den Altersgruppen an Bevölkerungsgruppe"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x ="Altersgruppe"
         , y = "Anteil Todesfälle [%]"
         , caption = citation ) -> pp6

ggsave('png/FZWoche_Todesfall.png'
       , device = 'png'
       , bg = "white"
       , width = 1920 * 2
       , height = 1080 * 2
       , units = "px"
)

fa %>% ggplot(
  aes( x = Altersgruppe, y = AnzahlTodesfall )) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  theme_ipsum() +
  labs(  title = "Todesfälle in der Altersgruppen"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x = "Altersgruppe"
         , y = "Todesfälle"
         , caption = citation ) -> pp6

ggsave('png/FZWoche_TodesfallSum2.png'
       , device = 'png'
       , bg = "white"
       , width = 1920 * 2
       , height = 1080 * 2
       , units = "px"
)

