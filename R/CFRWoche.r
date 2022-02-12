#!/usr/bin/env Rscript
#
#
# Script: CFRWoche.r
#
# Stand: 2022-01-21
# (c) 2021 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

options(OutDec=',')
MyScriptName <- "CFRWoche"

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

if ( ! exists("CFRWoche") ) {
  CFRWoche <- RunSQL('select * from CFRWoche where Datum > "2020-03-01";')
  CFRWoche$Geschlecht[CFRWoche$Geschlecht == 'M'] <- 'Männer'
  CFRWoche$Geschlecht[CFRWoche$Geschlecht == 'W'] <- 'Frauen'
  CFRWoche$Geschlecht[CFRWoche$Geschlecht == 'B'] <- 'Beide'
  CFRWoche$CFR <- as.numeric(CFRWoche$CFR)
  
}
AG <- unique(CFRWoche$Altersgruppe)


mbreaks1 <-seq( 0, max(CFRWoche$Pw), 10)
mbreaks2 <-seq( 0, max(CFRWoche$Pw), 5)
                
for (A in AG) {
  

CFRWoche %>% filter( Altersgruppe == A & Pw > 102 ) %>% ggplot( ) +
  geom_line( aes( x = Datum, y = CFR, colour = Geschlecht )) +
#  facet_wrap ( vars(Altersgruppe) ) +
  expand_limits( y = 0 ) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_date(date_breaks = "1 week", date_labels = "%W") +
  # scale_x_continuous( mbreaks1 ,minor_breaks = mbreaks2 ) +
  scale_y_continuous( labels = scales::percent ) +
  theme_ta() +
  labs(   title = paste( 'Deutschland CFR der Altersgruppe', A )
          , subtitle= paste( 'Anteil Todesfälle an Fällen einer Kalenderwochen Stand:', heute )
          , x = 'PandemieWoche'
          , y = 'CFR in [%]' 
          , colour = 'Legende'
          , caption = 'Erste Woche = Kalenderwoche 1/2020' 

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

