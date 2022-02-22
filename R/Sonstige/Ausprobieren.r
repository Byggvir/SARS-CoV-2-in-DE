#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"Ausprobieren"

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

WD <- paste(SD[1:(length(SD)-2)],collapse='/')

setwd(WD)

fPrefix <- "Ausprobieren_"

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nWochenberichte bis 44. Kw"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date() - 1
heute <- format(today, "%d %b %Y")

AlterVon <- 60
AlterBis <- '∞'

SQL <- paste(
    'select A.Woche, A.AlterVon, A.Outcome, A.Alle as OutAlle, A.Geimpft as OutGeimpft, A.Ungeimpft as OutUngeimpft, B.Geimpft/100 as Geimpft, B.Ungeimpft /100 as Ungeimpft '
  , 'from ImpfAnteile as A join ImpfAnteile as B on A.Woche = B.Woche and A.AlterVon = B.AlterVon'
  , 'where A.AlterVon =',AlterVon,'and A.Outcome <> "Impfquote" and B.Outcome = "Impfquote";'
  , sep = ' ')

durchbruch <- RunSQL(SQL = SQL)

durchbruch %>%   mutate( ordered = factor( Outcome, 
                                           levels=c(  "Symptomatisch"
                                                      , "Hospitalisiert"
                                                      , "Intensivstation"
                                                      , "Gestorben"))) %>% 
  ggplot() +
  geom_point(aes( x=Geimpft, y = OutGeimpft/OutAlle,  colour="Geimpft")) +
  geom_point(aes( x=Geimpft, y = OutUngeimpft/OutAlle, colour="Ungeimpft")) +
  geom_smooth(aes( x=Geimpft, y = OutGeimpft/OutAlle, colour="Geimpft")) +
  geom_smooth(aes( x=Geimpft, y = OutUngeimpft/OutAlle, colour="Ungeimpft")) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = T) +
  expand_limits(y=0) +
  facet_wrap(vars(ordered)) +
  theme_ipsum() +
  theme(  plot.title = element_text(size=24)
         , strip.text.x = element_text (
          size = 12
          , color = "black"
          , face = "bold.italic"
        ) ) +
  labs(  title = "Impfdurchbruch Anteile der Geimpften / Ungeimpften"
       , subtitle= paste('Alter von', AlterVon ,'bis', AlterBis)
       , x = "Impfquope"
       , y = "Anteil am Endpunkt"
       , colour = "Impfstatus"
       , caption = citation ) -> p

ggsave(  paste('png/AusprobierenID', AlterVon,'.png', sep='')
       , bg = "white"
       , width = 29.7
       , height = 21
       , units = "cm"
       , dpi = 300 )
