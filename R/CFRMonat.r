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
citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"


options(OutDec=',')

require(data.table)
library(tidyverse)
# library(REST)
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

if ( ! exists("CFRMonat")) {
  CFRMonat <- RunSQL('select * from CFRMonat;')
  CFRMonat$Geschlecht <- factor (CFRMonat$Geschlecht
                                 , levels = c( 'B', 'W', 'M')
                                 , labels = c('Beide', 'Frauen', 'Männer'))
  
  CFRMonat$CFR <- as.numeric(CFRMonat$CFR)
  
  
}
if ( ! exists("StdCFRMonat")) {
  StdCFRMonat <- RunSQL('select * from StdCFRMonat where Datum >= "2020-03-01";')
}

AG <- unique(CFRMonat$Altersgruppe)

for ( D in as.Date( c( '2020-03-01','2022-01-01' ) ) ) {

for (A in AG) {


CFRMonat %>% filter( Altersgruppe == A & Datum >= D) %>% ggplot( ) +
    geom_ribbon( aes( x = Datum, 
                      ymin = CFR -  2 * SigmaRel,
                      ymax = CFR + 2 * SigmaRel,
                      group = Geschlecht,
                      colour = Geschlecht,
                      fill = Geschlecht
                      )
                 , alpha = 0.1
                 , show.legend = FALSE
                 , linetype = 'dotted'
                 ) +
    geom_line( aes( x = Datum,
                    y = CFR,
                    group = Geschlecht,
                    colour = Geschlecht
                    )
               ) +
  expand_limits( y = 0 ) +
  scale_x_date(date_minor_breaks = "3 month") +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum() +
  labs(   title = paste( 'Deutschland CFR der Altersgruppe', A )
          , subtitle= paste( 'Anteil Todesfälle an Fällen eines Monats. Stand:', heute )
          , x = 'Monat'
          , y = 'CFR in [%]'
          , colour = 'Geschlecht'
          , caption = citation ) -> p

  ggsave( file = paste(
          "png/"
          , MyScriptName
          , '-'
          , A
          , '-'
          , D
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


StdCFRMonat %>% filter (Datum >= D ) %>% ggplot( ) +
  geom_line( aes( x = Datum, y = StdCFR, colour = 'Standardisierte CFR' ) ) +
  geom_line( # data = CFRMonat %>% filter ( Altersgruppe == 'Alle' & Geschlecht == 'Beide'),
             aes( x = Datum, y = CFR, colour = 'Rohe CFR' )) +
  #  facet_wrap ( vars(Altersgruppe) ) +
  expand_limits( y = 0 ) +
#  scale_fill_viridis(discrete = TRUE) +
  scale_x_date(date_minor_breaks = "3 month") +
  scale_y_continuous(labels = scales::percent) +
  theme_ta() +
  labs(   title = paste( 'Deutschland CFR')
          , subtitle= paste( 'Rohe und altersstandardisierte CFR je Monat. Stand:', heute )
          , x = 'Monat'
          , y = 'CFR in [%]'
          , colour = 'Legende'
          , caption = citation ) -> Ps

ggsave( file = paste(
  "png/"
  , MyScriptName
  , '-'
  , D
  , '-Std'
  , '.png'
  , sep = ""
)
, plot = Ps
, device = 'png'
, bg = "white"
, width = 3840
, height = 2160
, units = "px"
)

}
