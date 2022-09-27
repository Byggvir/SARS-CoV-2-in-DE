#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZJahr_"

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

fPrefix <- "Fallzahlen_Jahr"

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/mytheme.r")
source("R/lib/sql.r")

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- ' select * from FaelleProJahr;'
daten <- RunSQL(SQL = SQL)

bar <- data.table (
  Jahr = c(daten$Jahr, daten$Jahr)
  , Outcome = c(rep('Fall',3),rep('Todesfall',3))
  , Anzahl = c(daten$AnzahlFall,daten$AnzahlTodesfall)
  
)

bar %>% ggplot(
  aes( x = Jahr, y = Anzahl, colour = Outcome, fill = Outcome ) ) +
  geom_bar( position = "dodge", stat = "identity") +
  geom_text( aes( label = Anzahl ), size = 5, position = position_dodge( width = 0.9 ), vjust = -0.25 ) +
#  scale_fill_viridis( discrete = T ) +
  scale_x_continuous(  breaks = 2020:2022 , labels = 2020:2022 ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  theme_ta() +
  labs(  title = "Corona-Fälle und Todesfälle nach Jahr des Meldedatums"
         , subtitle = paste ("Deutschland, Stand:", heute, sep ='')
         , x = "Jahr"
         , y = "Anzahl" 
         , colour = "Outcome"
         , caption = citation ) -> pp

ggsave(  paste('png/',fPrefix,'.png', sep = '')
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 300 )
