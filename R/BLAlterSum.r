#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2021-12-10
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"BLAlterSum"

require(data.table)
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


source("R/lib/myfunctions.r")
source("R/lib/mytheme.r")
source("R/lib/sql.r")

citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL = 'select * from FaelleBLSum;'

Faelle <- RunSQL(SQL = SQL)

Faelle$Geschlecht[Faelle$Geschlecht=='M'] <- 'Männlich'
Faelle$Geschlecht[Faelle$Geschlecht=='W'] <- 'Weiblich'

for (B in unique(Faelle$Bundesland)) {

Faelle %>% filter ( Bundesland == B)   %>% ggplot( aes( x = Altersgruppe, y = AnzahlFall / Einwohner , fill = Geschlecht) ) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = sprintf( "%.1f", round(AnzahlFall / Einwohner * 100,1))), size = 6, vjust = 0, hjust = c(rep(1,6),rep(0,6))) +
  expand_limits( y = 1 ) +    
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(labels = scales::percent) +

  theme_ta() +
  labs( title = paste( B,"Fälle nach Altersgruppe pro Einwohner")
        , subtitle= paste("Stand: ", heute, sep ='')
        , x = "Altersgruppe"
        , y = "Anteil in der Altersgruppe [%]" 
        , colour = "Geschlecht"
        , caption = citation ) -> p1

ggsave(  plot = p1
       , filename = paste('png/', MyScriptName,'-', B,".png", sep="")
       , device = "png"
       ,  bg = "white"
       , width = 3840
       , height = 2160
       , units = "px"
       )
}
