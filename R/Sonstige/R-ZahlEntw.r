#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

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

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\n GitHub Nowcasts"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date() - 1
heute <- format(today, "%d %b %Y")

Jahr <- 2022
Monat <- 1

SQL <- '
select 
  A.Datum
  , A.BerechnetAm
  , weekday(A.Datum) as Wochentag
  , PS_7_Tage_R_Wert
  , UG_PI_7_Tage_R_Wert
  , OG_PI_7_Tage_R_Wert
from Nowcasts as A;'

daten <- RunSQL(SQL)

daten %>% filter ( year(Datum) == Jahr & month(Datum) == Monat & BerechnetAm <= "2022-03-15" ) %>% ggplot( aes(x= BerechnetAm)) +
  geom_line(aes( y = PS_7_Tage_R_Wert, colour ="Punktschätzer"), size = 1) +
  geom_line(aes( y = UG_PI_7_Tage_R_Wert, colour ="Untergrenze"), size = 0.5) +
  geom_line(aes( y = OG_PI_7_Tage_R_Wert, colour ="Obergrenze"), size = 0.5) +
  geom_hline(aes(yintercept=1), color = 'red') +
  scale_color_manual(values = c("Punktschätzer" = "black",
                                "Untergrenze" = "green",
                                "Obergrenze" = "steelblue")) +
  scale_fill_viridis(discrete = T) +
  facet_wrap(vars(Datum)) +
  theme_ta() +
  labs(  title = "Entwicklung des berechneten R-Wertes für ausgewählte Tage"
       , subtitle= "Stand: 21.02.2022"
       , x = "Datum"
       , y = "R-Zahl"
       , colour = "Schätzung"
       , caption = citation ) -> p

ggsave(  paste('png/R_Entwicklung', Monat, '.png', sep='')
       , device = 'png'
       , bg = "white"
       , width = 3840 * 3
       , height = 2160 * 3
       , units = "px"
       )
