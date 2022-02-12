#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# last Change: 2021-06-25
#
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZTag"

library(tidyverse)
library(REST)
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
  
  #  When executing on command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-1)],collapse='/')

setwd(WD)

fPrefix <- "Ausprobieren_"

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/mytheme.r")
source("R/lib/sql.r")

# Reads the cumulative cases and death from rki.de
# The Excel file is in a very poor format. Therefore we have to adjust the data.
# The daily cases and deaths are in the second worksheet. We need only column 2 and 5.
# The date  in column one is one day ahead in time.

source("R/lib/copyright.r")
source("R/lib/myfunctions.r")
source("R/lib/sql.r")

options( 
    digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
  )
citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nGitHub SARS-CoV-2 Infektionen"

SQL <- 'select * from FaelleProTag'
daten <- RunSQL( SQL )

daten$Meldedatum <- as.Date(daten$Meldedatum)

daten %>%  filter ( Meldedatum > "2021-08-31" ) %>%  ggplot( aes( x = Meldedatum, y = AnzahlFall )) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_date ( breaks = '1 month') + 
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ta() +
  theme(  plot.title = element_text( size = 24 )
          , legend.position="right"
          , axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          , strip.text.x = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic"
          ) ) +  labs(  title = paste('Fälle pro Tag')
         , subtitle = "Meldedatum Gesundheitsamt"
         , x = paste( 'Datum' )
         , y = paste( 'Anzahl Fälle' )
         , caption = citation ) -> p1

daten %>% filter ( Meldedatum > "2021-08-31" ) %>% ggplot( aes( x = Meldedatum, y = AnzahlTodesfall )) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_date ( breaks = '1 month') + 
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ta() +
  theme(  plot.title = element_text( size = 24 )
          , legend.position="right"
          , axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          , strip.text.x = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic"
          ) ) +
  labs(  title = paste('Todesfälle pro Tag')
         , subtitle = "Meldedatum Gesundheitsamt"
         , x = paste( 'Datum' )
         , y = paste( 'Anzahl Fälle' )
         , caption = citation ) -> p2

P <- grid.arrange( p1, p2 , ncol = 2)

ggsave(  paste( 
           file = 'png/', MyScriptName, '.png', sep='')
         , plot = P
         , device = 'png'
         , bg = "white"
         , width = 1920 * 2
         , height = 1080 * 2
         , units = "px"
)
