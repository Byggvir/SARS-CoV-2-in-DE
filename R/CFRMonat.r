#!/usr/bin/env Rscript
#
#
# Script: CFRMonat.r
#
# Stand: 2022-01-21
# (c) 2021 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <- "CFRMonat"

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

WD <- paste(SD[1:(length(SD)-1)],collapse='/')
setwd(WD)

source("R/lib/myfunctions.r")
source("R/lib/mytheme.r")
source("R/lib/sql.r")

today <- Sys.Date()
heute <- format(today, "%Y%m%d")

CFRMonat <- RunSQL('select * from CFRMonat where Datum > "2020-02-01";')
CFRMonat$Geschlecht[CFRMonat$Geschlecht == 'M'] <- 'Männer'
CFRMonat$Geschlecht[CFRMonat$Geschlecht == 'W'] <- 'Frauen'
CFRMonat$Geschlecht[CFRMonat$Geschlecht == 'B'] <- 'Beide'
CFRMonat$CFR <- as.numeric(CFRMonat$CFR)

AG <- unique(CFRMonat$Altersgruppe)

for (A in AG) {
  

CFRMonat %>% filter( Altersgruppe == A ) %>% ggplot( ) +
  geom_line( aes( x = Datum, y = CFR, colour = Geschlecht )) +
#  facet_wrap ( vars(Altersgruppe) ) +
  expand_limits( y = 0 ) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_date(date_minor_breaks = "1 month") +
  scale_y_continuous(labels = scales::percent) +
  theme_ta() +
  labs(   title = paste( 'Deutschland CFR der Altersgruppe', A )
          , subtitle= paste( 'Anteil Todesfälle an Fällen eines Monats. Stand:', heute )
          , x = 'Monat'
          , y = 'CFR in [%]' 
          , colour = 'Legende' 

) -> p

ggsave( file = paste( 
          "png/"
          , MyScriptName
          , '-'
          , A
          , '.png'
          , sep = ""
        )
        , plot = p
        , device = 'png'
        , bg = "white"
        , width = 3840
        , height = 2160
        , units = "px"
)
}

