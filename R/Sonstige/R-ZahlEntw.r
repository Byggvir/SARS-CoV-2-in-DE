#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"R-ZahlEntw"

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

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\n GitHub Nowcasts"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date() - 1
heute <- format(today, "%d %b %Y")

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

daten %>% filter ( Datum >= "2021-11-01" & Datum < "2021-12-01" & BerechnetAm < "2021-12-15") %>% ggplot( aes(x= BerechnetAm)) +
  geom_line(aes( y = PS_7_Tage_R_Wert, colour ="Punktschätzer"), size = 1) +
  geom_line(aes( y = UG_PI_7_Tage_R_Wert, colour ="Untergrenze"), size = 0.5) +
  geom_line(aes( y = OG_PI_7_Tage_R_Wert, colour ="Obergrenze"), size = 0.5) +
  geom_hline(aes(yintercept=1), color = 'red') +
  scale_color_manual(values = c("Punktschätzer" = "black",
                                "Untergrenze" = "green",
                                "Obergrenze" = "steelblue")) +
  scale_fill_viridis(discrete = T) +
  facet_wrap(vars(Datum)) +
  theme_ipsum() +
  theme(      plot.title=element_text(size=48, hjust=0.5, face="italic", color="black")
            , plot.subtitle=element_text(size=36, hjust=0.5, face="italic", color="black")
            , axis.text.x  = element_text ( color = 'black', size = 12, angle = 90)
            , axis.title.x = element_text ( color='black', size = 12)
            , axis.text.y  = element_text ( color = 'black', size = 12)
            , axis.title.y = element_text ( color='black', size = 12)
            , strip.text.x = element_text (
              size = 24
              , color = "black"
              , face = "bold.italic"
            ) ) + 
  labs(  title = "Entwicklung des berechneten R-Wertes für ausgewählte Tage"
       , subtitle= "Stand: 30.11.2021"
       , x = "Datum"
       , y = "R-Zahl"
       , colour = "Schätzung"
       , caption = citation ) -> p

ggsave(  paste('png/R_Entwicklung', 11, '.png', sep='')
       , type = "cairo-png"
       , bg = "white"
       , width = 29.7 * 2
       , height = 21 * 2
       , units = "cm"
       , dpi = 300 )
