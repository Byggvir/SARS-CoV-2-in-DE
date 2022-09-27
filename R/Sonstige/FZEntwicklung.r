#!/usr/bin/env Rscript
#
#
# Script: FZEntwicklung.r
#
# last Change: 2022-01-05
#
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZEntwicklung.r"

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

fPrefix <- "FZ_"

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

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

B <- RunSQL( SQL = 'select distinct Bestandsdatum from NeuFaelle;')

SQL <- 'select Bestandsdatum, Meldedatum, Art, sum(Anzahl) as Anzahl from NeuFaelle where Art <> "G" and Meldedatum >= "2021-03-01" group by Bestandsdatum, Meldedatum, Art;'

Bestand <- RunSQL(SQL = SQL)
Bestand$Art[Bestand$Art == 'F'] <- 'Fall'
Bestand$Art[Bestand$Art == 'T'] <- 'Todesfall'

scl <- max(Bestand$Anzahl[Bestand$Art == 'Todesfall']) / max(Bestand$Anzahl[Bestand$Art == 'Fall'])

for ( D in B$Bestandsdatum ) {
  
  Stand <- format(as.Date(D,origin = "1970-01-01"),"%Y%m%d")
  
  if ( ! file.exists(paste('png/Entwicklung/',fPrefix, Stand,'.png', sep = ''))) {
    
  Bestand %>% filter ( Bestandsdatum == D ) %>% ggplot() +
    geom_line( data = Bestand %>% filter ( Bestandsdatum == D & Art == 'Fall' ),
               aes(x = Meldedatum, y = Anzahl, colour = Art) ) +
      geom_line( data = Bestand %>% filter ( Bestandsdatum == D & Art == 'Todesfall' ),
                 aes(x = Meldedatum, y = Anzahl / scl , colour = Art) ) +
      scale_fill_viridis(discrete = TRUE ) +
    scale_y_continuous(  sec.axis = sec_axis( ~.*scl, name = "Todesfälle pro Woche", labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) )
                           , labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
    expand_limits( x = max(B$Bestandsdatum), y = max(Bestand$Anzahl[Bestand$Art == 'Fall'])) +
    theme_ta() +
        labs(  title = "Corona-Fälle nach Meldedatum"
             , subtitle = paste ("Deutschland, Stand:", Stand, sep ='')
             , x ="Tag"
             , y = "Fälle"
             , caption = citation ) -> p1

   ggsave(  paste('png/Entwicklung/',fPrefix, Stand,'.png', sep = '')
          , plot = p1
          , bg = "white"
          , device = 'jpg'
          , width = 3840
          , height = 2160 
          , units = "px" )
  }
}
