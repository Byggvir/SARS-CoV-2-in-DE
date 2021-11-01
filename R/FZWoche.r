#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZWoche"

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
options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

today <- Sys.Date()
heute <- format(today, "%d %b %Y")


SQL <- ' select * from FaelleProWocheAltersgruppe;'
weekly <- RunSQL(SQL = SQL)

m <- length(weekly[,1])
reported <- weekly$Kw[m]

scl <- max(weekly$AnzahlFall)/max(weekly$AnzahlTodesfall) 

weekly %>% ggplot(
  aes( x = PandemieWoche )) +
  geom_line(aes(y = AnzahlFall, colour = "Fälle" ), color = 'blue') +
  geom_line(aes(y = AnzahlTodesfall * scl, colour = "Todesfälle" ), color = 'red') +
  scale_y_continuous( sec.axis = sec_axis(~./scl, name = "Todesfälle", labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ))
                      , labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  facet_wrap(vars(Altersgruppe)) +
  labs(  title = "Wöchentliche Fälle"
         , subtitle = paste ("Deutschland, Stand:", heute, sep ='')
         , x ="Pandemiewoche"
         , y = "Fälle" 
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
            , face = "bold.italic")
          , plot.caption = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic" )
          ) + 
  theme(plot.title=element_text(size=48, hjust=0.5, face="italic", color="black")) +
  theme(plot.subtitle=element_text(size=36, hjust=0.5, face="italic", color="black")) -> pp

ggsave(  paste('png/FZBund_Alter.png', sep = '')
         , type = "cairo-png"
         , bg = "white"
         , width = 29.7 * 2
         , height = 21 * 2
         , units = "cm"
         , dpi = 300 )


weekly %>% ggplot(
  aes( x = AnzahlFall, y = AnzahlTodesfall ) 
  ) +
  geom_point(aes(x = AnzahlFall,y = AnzahlTodesfall, colour = Altersgruppe ) ) +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  geom_smooth( method = "lm", formula = y ~ x, data = weekly %>% filter(Altersgruppe == "A35-A59")) +
  geom_smooth( method = "lm", formula = y ~ x, data = weekly %>% filter(Altersgruppe == "A60-A79")) +
  geom_smooth( method = "lm", formula = y ~ x, data = weekly %>% filter(Altersgruppe == "A80+")) +
  facet_wrap(vars(Jahr)) +
  labs(  title = "SARS-CoV-2 Todesfälle ~ Fälle nach Kalenderwoche"
         , subtitle = paste ("Deutschland, Stand:", heute, sep ='')
         , x = "Fälle"
         , y = "Todesfälle" 
         , colour = "Altersgruppe"
         , caption = citation ) +
  theme_ipsum() +
  theme(  axis.text.y  = element_text ( color = 'blue' )
          , axis.title.y = element_text ( color='blue' )
          , axis.text.y.right = element_text ( color = 'red' )
          , axis.title.y.right = element_text ( color='red' )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "italic" )
          , plot.caption = element_text (
            size = 12
            , color = "black"
            , face = "italic" )
          ) + 
  theme(plot.title=element_text(size=36, hjust=0.5, face="bold.italic", color="black")) +
  theme(plot.subtitle=element_text(size=24, hjust=0.5, face="italic", color="black")) -> pp2

ggsave( paste('png/FZBund_AlterScatterplot.png', sep = '')
         , type = "cairo-png"
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 300 )
