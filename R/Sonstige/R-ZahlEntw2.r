#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"R-ZahlEntw2"

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



SQL <- 'select A.Datum,A.BerechnetAm, weekday(A.Datum) as Wochentag, A.PS_7_Tage_R_Wert,B.PS_7_Tage_R_Wert,round(A.PS_7_Tage_R_Wert-B.PS_7_Tage_R_Wert,2) as Differenz from Nowcasts as A join Nowcasts as B on B.BerechnetAm = adddate(A.Datum,14) and A.Datum = B.Datum where A.BerechnetAm = adddate(A.Datum,5);'
daten <- RunSQL(SQL)

daten$Bez <- WochentageLang[daten$Wochentag+1]

daten %>%
  ggplot( aes( x = reorder(Bez,Wochentag), y=Differenz ) ) +
  geom_boxplot() +
  guides( fill = "none" ) +
  stat_summary(fun = mean, geom = "point", shape = 5, size = 4) +
  # geom_line(aes( y = Differenz), size = 1) +
  geom_hline(aes( yintercept = 0), color = 'red', size = 0.2) +
  scale_fill_viridis(discrete = T) +
  # facet_wrap(vars(reorder(Bez,Wochentag))) +
  theme_ipsum() +
  theme(      plot.title=element_text(size=24, hjust=0.5, face="italic", color="black")
            , plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black")
            , axis.text.x  = element_text ( color = 'black', size = 12, angle = 90)
            , axis.title.x = element_text ( color='black', size = 12)
            , axis.text.y  = element_text ( color = 'black', size = 12)
            , axis.title.y = element_text ( color='black', size = 12)
            , strip.text.x = element_text (
              size = 18
              , color = "black"
              , face = "italic"
            ) ) + 
  labs(  title = "Differenz des R-Wert zum 14 Tage später gemeldeten Wert"
       , subtitle= paste( "R(t) - R(t+14) / Stand:", heute )
       , x = "Datum"
       , y = "R-Zahl"
       , colour = "Schätzung"
       , caption = citation ) -> p

ggsave(  paste('png/R_EntwicklungDiff', 3, '.png', sep='')
       , bg = "white"
       , width = 29.7
       , height = 21
       , units = "cm"
       , dpi = 300 )
