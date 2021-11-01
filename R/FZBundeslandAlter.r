#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZBundeslandAlter"

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

fPrefix <- "Fallzahlen_Wo_"

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- 'select * from Bundesland where IdBundesland > 0 order by IdBundesland;'
Bundesland <- RunSQL(SQL = SQL)

SQL <- 'select * from InzidenzAltersgruppeBL;'
if ( ! exists("weekly")) {
  weekly <- RunSQL(SQL = SQL)
}

SQL <- 'select * from FallAltersgruppen;'
Altersgruppen <- RunSQL(SQL = SQL)

for (i in 1:nrow(Bundesland)) {

BL <- Bundesland[i,1]

w <- weekly %>% filter(IdBundesland == i )
scl <- max(w$AnzahlFall/w$Bev)/max(w$AnzahlTodesfall/w$Bev)

weekly %>% filter(IdBundesland == i ) %>% ggplot(
  aes( x = PandemieWoche )) +
  geom_line(aes(y = AnzahlFall / Bev * 100000, colour = "Fälle" ), color = 'blue') +
  geom_line(aes(y = AnzahlTodesfall  / Bev * 100000 * scl, colour = "Todesfälle" ), color = 'red') +
  scale_y_continuous( sec.axis = sec_axis(~./scl, name = "Todesfälle / 100000", labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ))
                      , labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  facet_wrap(vars(Altersgruppe)) +
  labs(  title = "CoViD-19 Wöchentliche Fälle / 100000"
         , subtitle= paste(Bundesland[i,2],", Stand:", heute, sep ='')
         , x ="Pandemiewoche"
         , y = "Fälle / 100.000" 
         , colour = "Fälle / Todesfälle"
         , caption = citation ) +
  theme_ipsum() +
  theme(  axis.text.y  = element_text ( color = 'blue' )
        , axis.title.y = element_text ( color='blue' )
        , axis.text.y.right = element_text ( color = 'red' )
        , axis.title.y.right = element_text ( color='red' )
        , strip.text.x = element_text (
          size = 24
          , color = "black"
          , face = "bold.italic"
        ) ) + 
  theme(plot.title=element_text(size=48, hjust=0.5, face="italic", color="black")) +
  theme(plot.subtitle=element_text(size=36, hjust=0.5, face="italic", color="black")) -> pp
 
ggsave(  paste('png/FZBundeslaenderAlter',Bundesland[i,2],'.png', sep = '')
       , type = "cairo-png"
       , bg = "white"
       , width = 29.7 * 2
       , height = 21 * 2
       , units = "cm"
       , dpi = 300 )
}

for ( AG in Altersgruppen[,1]) {
  print(AG)
  w <- weekly %>% filter(Altersgruppe == AG )
  scl <- max(w$AnzahlFall/w$Bev)/max(w$AnzahlTodesfall/w$Bev)
  
  weekly %>% filter(Altersgruppe == AG ) %>% ggplot(
    aes( x = PandemieWoche )) +
    geom_line(aes(y = AnzahlFall / Bev * 100000, colour = "Fälle" ), color = 'blue') +
    geom_line(aes(y = AnzahlTodesfall  / Bev * 100000 * scl, colour = "Todesfälle" ), color = 'red') +
    scale_y_continuous( sec.axis = sec_axis(~./scl, name = "Todesfälle / 100.000", labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ))
                        , labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    facet_wrap(vars(Bundesland)) +
    labs(  title = "CoViD-19 Inzidenz nach Bundesland"
           , subtitle= paste("Altersgruppe ", AG,", Stand:", heute, sep ='')
           , x = "Pandemiewoche"
           , y = "Fälle / 100.000" 
           , colour = "Fälle/Todesfälle"
           , caption = citation ) +
    theme_ipsum() +
    theme(  axis.text.y  = element_text ( color = 'blue' )
            , axis.title.y = element_text ( color='blue' )
            , axis.text.y.right = element_text ( color = 'red' )
            , axis.title.y.right = element_text ( color='red' )
            , strip.text.x = element_text (
              size = 24
              , color = "black"
              , face = "bold.italic"
            ) ) + 
    theme(plot.title=element_text(size=48, hjust=0.5, face="italic", color="black")) +
    theme(plot.subtitle=element_text(size=36, hjust=0.5, face="italic", color="black")) -> pp
  
  ggsave(  paste('png/FZBundeslaenderAlter',AG,'.png', sep = '')
           , type = "cairo-png"
           , bg = "white"
           , width = 29.7 * 2
           , height = 21 * 2
           , units = "cm"
           , dpi = 300 )
}
