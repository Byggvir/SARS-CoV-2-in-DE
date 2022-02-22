#!/usr/bin/env Rscript
#
#
# Script: CFRMonat.r
#
# Stand: 2022-01-21
# (c) 2021 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <- "Weather"

options(OutDec=',')

require(data.table)
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
#library(extrafont)
#extrafont::loadfonts()

# Set Working directory to git root

if (rstudioapi::isAvailable()){
  
  # When called in RStudio
  SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
  
} else {
  
  #  When called from command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-2)],collapse='/')
setwd(WD)

source("R/lib/myfunctions.r")
source("R/lib/mytheme.r")
source("R/lib/sql.r")

daten <- read.csv('/tmp/T.csv', dec = '.', quote = '"' , header = TRUE, colClasses = c("Date","numeric", "numeric"))

daten %>% ggplot() + 
  geom_point( aes( x = Datum, y = maxT, colour = 'Max' ) ) +
  geom_point( aes( x = Datum, y = minT, colour = 'Min' ) ) +
  geom_smooth( aes( x = Datum, y = maxT, colour = 'Max' ) ) +
  geom_smooth( aes( x = Datum, y = minT, colour = 'Min' ) ) +
  scale_x_date( breaks = '1 month' ) + 
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
  labs(  title = paste( 'Temperaturen Rheinbach' )
         , subtitle = 'Minimale / Maximale Temperatur des Tages'
         , x = "Datum"
         , y = "Temperatur [°C]"
  ) -> P

ggsave(  paste( 
  file = 'png/', MyScriptName, '_T.png', sep='')
  , plot = P
  , device = 'png'
  , bg = "white"
  , width = 1920 * 2
  , height = 1080 * 2
  , units = "px"
)
