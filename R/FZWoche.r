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

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

# Set Working directory to git root

if (rstudioapi::isAvailable()){
  
  # When executed in RStudio
  SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
  
} else {
  
  #  When executing on command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-1)],collapse='/')

setwd(WD)

fPrefix <- "Fallzahlen_Woche"

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- ' select * from FaelleProWocheAltersgruppe;'
FZWoche <- RunSQL(SQL = SQL)

m <- length(FZWoche[,1])
reported <- FZWoche$Kw[m]

scl <- max(FZWoche$AnzahlFall)/max(FZWoche$AnzahlTodesfall) 

FZWoche %>% ggplot(
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
  theme(  axis.text.y  = element_text ( color = 'blue', size = 18 )
          , axis.title.y = element_text ( color='blue', size = 24 )
          , axis.text.y.right = element_text ( color = 'red', size = 18 )
          , axis.title.y.right = element_text ( color='red', size = 24 )
          , axis.text.x = element_text ( color = 'black', size = 18 )
          , axis.title.x = element_text ( color='black', size = 18 )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
            )
          , plot.caption = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic" )
          ) + 
  theme(plot.title=element_text(size=48, hjust=0.5, face="italic", color="black")) +
  theme(plot.subtitle=element_text(size=36, hjust=0.5, face="italic", color="black")) -> pp

ggsave(  paste(fPrefix,'_Alter.png', sep = '')
         , device = 'png'
         , bg = "white"
         , width = 1920 * 2
         , height = 1080 * 2
         , units = "px"
)

FZWoche %>% ggplot(
  aes( x = AnzahlFall, y = AnzahlTodesfall ) 
  ) +
  geom_point(aes(x = AnzahlFall,y = AnzahlTodesfall, colour = Altersgruppe ) ) +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  geom_smooth( data = FZWoche %>% filter(Altersgruppe >= "A35-A59"), method = "lm", formula = y ~ x, aes(colour = Altersgruppe)) +
  facet_wrap(vars(Jahr)) +
  labs(  title = "SARS-CoV-2 Todesfälle ~ Fälle nach Kalenderwoche"
         , subtitle = paste ("Deutschland, Stand:", heute, sep ='')
         , x = "Fälle"
         , y = "Todesfälle" 
         , colour = "Altersgruppe"
         , caption = citation ) +
  theme_ipsum() +
  theme(  axis.text.y  = element_text ( color = 'blue', size = 18)
          , axis.title.y = element_text ( color='blue', size = 18)
          , axis.text.x = element_text ( color = 'black', size = 18 )
          , axis.title.x = element_text ( color='black', size = 18 )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
          )
          , plot.caption = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic" )
        ) + 
  theme(plot.title=element_text(size=36, hjust=0.5, face="bold.italic", color="black")) +
  theme(plot.subtitle=element_text(size=24, hjust=0.5, face="italic", color="black")) -> pp2

ggsave( paste(fPrefix,'_AlterScatterplot.png', sep = '')
        , device = 'png'
        , bg = "white"
        , width = 1920 * 2
        , height = 1080 * 2
        , units = "px"
)
SQL <- ' select * from FaelleProWoche;'
FZWoche_2 <- RunSQL(SQL = SQL)

FZWoche_2 %>% filter( PandemieWoche > 74  ) %>% ggplot(
  aes( x = PandemieWoche, y = AnzahlFall)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=AnzahlFall), size=2.5, position=position_dodge(width=0.9), hjust=0,vjust=0.5, angle= 90) +
  expand_limits( y = 500000) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  labs(  title = "Corona-Fälle nach Woche des Meldedatums"
         , subtitle = paste ("Deutschland, Stand:", heute, sep ='')
         , x = "PandemieWoche"
         , y = "Fälle" 
         , colour = "Fälle"
         , caption = citation ) +
  theme_ipsum() +
  theme(  axis.text.y  = element_text ( size = 12 )
          , axis.title.y = element_text ( size = 18 )
          , axis.text.x = element_text ( size = 12 )
          , axis.title.x = element_text (size = 12 )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
          )
          , plot.caption = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic" )
  ) + 
  theme(plot.title=element_text(size=36, hjust=0.5, face="italic", color="black")) +
  theme(plot.subtitle=element_text(size=24, hjust=0.5, face="italic", color="black")) -> pp

ggsave(  paste('png/Fallzahlen_Woche.png', sep = '')
         , device = 'png'
         , bg = "white"
         , width = 1920 * 2
         , height = 1080 * 2
         , units = "px"
)
