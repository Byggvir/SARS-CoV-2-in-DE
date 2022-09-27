#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZMonat_"

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

fPrefix <- "Fallzahlen_Monat"

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

citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- ' select * from FaelleProMonat;'
daten <- RunSQL(SQL = SQL)


daten %>% ggplot(
  aes( x = Datum, y = AnzahlFall)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=AnzahlFall), size=2.5, position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  labs(  title = "Corona-Fälle nach Monat des Meldedatums"
         , subtitle = paste ("Deutschland, Stand:", heute, sep ='')
         , x ="Monat"
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

ggsave(  paste('png/',fPrefix,'.png', sep = '')
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 300 )
