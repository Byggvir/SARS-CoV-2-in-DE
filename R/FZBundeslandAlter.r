#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZBundeslandAlter"

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
  
  #  When executi on command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-1)],collapse='/')

setwd(WD)

fPrefix <- "Fallzahlen_Wo_"

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- 'select * from Bundesland order by IdBundesland;'
Bundesland <- RunSQL(SQL = SQL)

for (i in 1:nrow(Bundesland)) {

BL <- Bundesland[i,1]

SQL <- paste(
    'select PandemieWoche(Meldedatum) as Kw'
  , ', Altersgruppe as Altersgruppe'
  , ', sum(AnzahlFall) as AnzahlFall, sum(AnzahlTodesfall) as AnzahlTodesfall'
  , ' from Faelle as F join Bundesland as B on B.IdBundesland = F.IdLandkreis div 1000 '
  , ' where IdBundesland = ' , BL, ' and Altersgruppe <> "unbekan" group by Kw, Altersgruppe;', sep='')

weekly <- RunSQL(SQL = SQL)

scl <- max(weekly$AnzahlFall)/max(weekly$AnzahlTodesfall) 
  
weekly %>% ggplot(
  aes( x = Kw )) +
  geom_line(aes(y = AnzahlFall, colour = "Fälle" ), color = 'blue') +
  geom_line(aes(y = AnzahlTodesfall * scl, colour = "Todesfälle" ), color = 'red') +
  scale_y_continuous( sec.axis = sec_axis(~./scl, name = "Todesfälle", labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ))
                      , labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  facet_wrap(vars(Altersgruppe)) +
  labs(  title = "Wöchentliche Fälle"
         , subtitle= paste(Bundesland[i,2],", Stand:", heute, sep ='')
         , x ="Pandemiewoche"
         , y = "Fälle" 
         , colour = "Fälle/Todesfälle"
         , caption = citation ) +
  theme_ipsum() +
  theme(  axis.text.y  = element_text ( color = 'blue' )
        , axis.title.y = element_text ( color='blue' )
        , axis.text.y.right = element_text ( color = 'red' )
        , axis.title.y.right = element_text ( color='red' )
        , strip.text.x = element_text (
          size = 24
          , color = "black"
          , face = "bold.italic"
        ) ) + 
  theme(plot.title=element_text(size=48, hjust=0.5, face="italic", color="black")) +
  theme(plot.subtitle=element_text(size=36, hjust=0.5, face="italic", color="black")) -> pp
 
ggsave(  paste('png/FZBundeslaenderAlter',Bundesland[i,2],'.png', sep = '')
       , type = "cairo-png"
       , bg = "white"
       , width = 29.7 * 2
       , height = 21 * 2
       , units = "cm"
       , dpi = 300 )
}
