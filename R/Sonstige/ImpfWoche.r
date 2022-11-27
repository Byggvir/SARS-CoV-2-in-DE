#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"ImpfWoche"

require(data.table)
library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(scales)
library(ragg)
library(extrafont)
extrafont::loadfonts()
# Set Working directory to git root

if (rstudioapi::isAvailable()){
  
  # When executed in RStudio
  SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
  
} else {
  
  #  When executi on command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-2)],collapse='/')

setwd(WD)

require(data.table)

source("R/lib/color_palettes.r")
source("R/lib/copyright.r")
source("R/lib/myfunctions.r")
source("R/lib/sql.r")

citation <- "© 2021 by Thomas Arend\nQuellen: Robert Koch-Institut (2021)\nSARS-CoV-2 Impfungen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153\nQuelle: © Statistisches Bundesamt (Destatis) Sonderauswertung, 2021"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date()
heute <- format(today, "%Y%m%d")

SQL <- 'select * from ImpfQuote;'
impfquote <- RunSQL(SQL)

impfquote %>% ggplot( aes( x = ImpfDatum, y = Quote)) +
  geom_line( data = impfquote %>% filter(AlterVon == 12), aes( colour = "A12-A17" ) )  +
  geom_line( data = impfquote %>% filter(AlterVon == 18), aes( colour = "A18-A59" ) ) +
  geom_line( data = impfquote %>% filter(AlterVon == 60), aes( colour = "A60+" ) ) +
  scale_color_manual(values=cbp1) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Corona: Impfquote nach Datum und Altersgruppe") +
  labs(  title = "Impfquote nach Altersgruppe"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x ="Datum"
         , y = "Quote [%]" 
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
            , face = "bold.italic")
          , plot.caption = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic" )
  ) + 
  theme(plot.title=element_text(size=48, hjust=0.5, face="italic", color="black")) +
  theme(plot.subtitle=element_text(size=36, hjust=0.5, face="italic", color="black")) -> p

ggsave( plot = p, 
        file = paste( 
          "png/ImpfQuote.png"
          , sep = ""
        )
,  bg = "white"
        , width = 29.7, height = 21, units = "cm", dpi = 150)
