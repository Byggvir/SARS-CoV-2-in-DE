#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZBundesland"

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
library(Cairo)
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

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date() - 1
heute <- format(today, "%d %b %Y")

SQL <- 'select * from Bundesland order by IdBundesland;'
Bundesland <- RunSQL(SQL)

SQL <- paste(
    '
    select 
      A.Idlandkreis as IdLankreis
    , L.Landkreis as Landkreis
    , A.IdLandkreis div 1000 as IdBundesland
    , A.Pw - 53 as Pw
    , A.AnzahlFall as Anzahl
    , B.AnzahlFall as AnzahlVorwoche
    , L.EW_insgesamt from FaelleLandkreisPw as A 
    
    join FaelleLandkreisPw as B 
    on A.IdLandkreis = B.IdLandkreis and A.Pw = B.Pw + 1 
    join Landkreis as L
    on A.IdLandkreis = L.IdLandkreis
    where 101 >= A.Pw and A.Pw >= 96;'
  , sep = ' ')

Landkreise <- RunSQL(SQL = SQL)

for ( B in 1:16) {
  
Landkreise %>% filter(IdBundesland == B) %>% ggplot() +
  stat_ellipse(aes( x = AnzahlVorwoche / EW_insgesamt * 100000, y = Anzahl / EW_insgesamt * 100000), type = "t", geom = "polygon", alpha = 0.1 ) +
  geom_abline(intercept = 0,slope = 1, color ='red') +
  geom_point( aes( x = AnzahlVorwoche / EW_insgesamt * 100000, y = Anzahl / EW_insgesamt * 100000, colour=Landkreis), size = 2) +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_fill_viridis(discrete = T) +
  expand_limits( x = 0 , y = 0 ) +
  facet_wrap(vars(Pw)) +
  theme_ipsum() +
  theme(  plot.title = element_text(size=24)
          , legend.position="none"
         , strip.text.x = element_text (
          size = 12
          , color = "black"
          , face = "bold.italic"
        ) ) +
  labs( title= paste("Landkreise",Bundesland[B+1,2]) 
       , subtitle = "Fallzahlen pro 100.000 Einwohner\nWoche ~ Vorwoche"
       , x = "Fälle Vorwoche pro 100.000"
       , y = "Fälle Woche pro 100.000"
       , caption = citation )

ggsave(  paste('png/LandkreisWocheVorwoche-',Bundesland[B+1,2],'.png', sep='')
       , type = "cairo-png"
       , bg = "white"
       , width = 29.7
       , height = 21
       , units = "cm"
       , dpi = 300 )
}
