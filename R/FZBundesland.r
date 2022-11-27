#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZBundesland"

library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(ggrepel)
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

SQL <- paste('select * from InzidenzBL;', sep='')
weekly <- RunSQL(SQL = SQL)

scl <- max(weekly$AnzahlFall)/max(weekly$AnzahlTodesfall) 
  
weekly %>% filter( PandemieWoche < max(PandemieWoche)) %>% ggplot(
  aes( x = PandemieWoche )) +
  geom_line(aes(y = AnzahlFall, colour = "Fälle" ), color = 'blue') +
  geom_line(aes(y = AnzahlTodesfall * scl, colour = "Todesfälle" ), color = 'red') +
  scale_y_continuous(  sec.axis = sec_axis(~./scl, name = "Todesfälle", labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ))
                       , labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  facet_wrap(vars(Bundesland)) +
  theme_ipsum() +
  theme(  plot.title = element_text( size=48 )
          , axis.text.y  = element_text ( color = 'blue', size = 24)
          , axis.title.y = element_text ( color='blue', size = 24)
          , axis.text.y.right = element_text ( color = 'red', size = 24 )
          , axis.title.y.right = element_text ( color='red', size = 24 )
          , strip.text.x = element_text (
              size = 24
              , color = "black"
              , face = "bold.italic"
          ) ) + 
  labs(  title = "Wöchentliche Fälle nach Bundesland"
       , subtitle= paste("Deutschland, Stand:", heute)
       , x ="Pandemiewoche"
       , y = "Fälle" 
       , colour = "Fälle/Todesfälle"
       , caption = citation ) -> pp1

ggsave(  'png/FZBundeslaender-Absolut.png'
       , bg = "white"
       , width = 29.7 * 2
       , height = 21 * 2
       , units = "cm"
       , dpi = 300 )

scl <- max(weekly$AnzahlFall/weekly$Bev)/max(weekly$AnzahlTodesfall/weekly$Bev) 

weekly %>% filter( PandemieWoche < max(PandemieWoche) ) %>% ggplot(
  aes( x = PandemieWoche )) +
  geom_line(aes(y = AnzahlFall / Bev * 100000, colour = "Fälle" ), color = 'blue') +
  geom_line(aes(y = AnzahlTodesfall / Bev *100000 * scl, colour = "Todesfälle" ), color = 'red') +
  scale_y_continuous( sec.axis = sec_axis(~./scl, name = "Todesfälle", labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ))
                      , labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  facet_wrap(vars(Bundesland)) +
  theme_ipsum() +
  theme(  plot.title = element_text(size=48)
          , axis.text.y  = element_text ( color = 'blue' )
          , axis.title.y = element_text ( color='blue' )
          , axis.text.y.right = element_text ( color = 'red' )
          , axis.title.y.right = element_text ( color='red' )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
          ) ) +
  labs(  title = "Wöchentliche Fallzahl pro 100.000 nach Bundesland"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x = "Pandemiewoche"
         , y = "Fälle pro 100.000" 
         , colour = "Fälle / Todesfälle"
         , caption = citation ) -> pp1


# pp1.animation = pp1 +
#   transition_time(PandemieWoche) +
#   # labs(subtitle = "Year: {frame_time}") +
#   shadow_wake(wake_length = 0.1)
# 
# 
# animate( plot = pp1.animation
#         , height = 1080
#         , width = 1920
#         )
# 
# anim_save("/tmp/FZBundeslaender-Inzidenz.gif")

ggsave(  filename = 'png/FZBundeslaender-Inzidenz.png'
         , plot = pp1
         , path = WD
         , device = 'png'
         , bg = "white"
         , width = 29.7 * 2
         , height = 21 * 2
         , units = "cm"
         , dpi = 300 )
