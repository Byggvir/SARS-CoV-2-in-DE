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
#library(REST)
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

WD <- paste(SD[1:(length(SD)-2)],collapse='/')

setwd(WD)

fPrefix <- "Impddurchbruch"

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nSARS-CoV-2 Wochenbericht"

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

Woche <- '2021-12-06'

ToWoche <- isoweek(as.Date(Woche))
FromWoche <- ToWoche - 3

SQL <- paste('select * from Impfdurchbruch where Woche = "', Woche ,'";', sep='')
impfd <- RunSQL(SQL = SQL)

impfd %>% 
mutate(name = factor( Outcome, 
                       levels=c( "Impfquote"
                               , "Symptomatisch"
                               , "Hospitalisiert"
                               , "Intensivstation"
                               , "Gestorben"))) %>%
ggplot(
  aes( x = name, y = Anzahl4W, fill = Gruppe  )) +
  geom_bar( position = "fill", stat = "identity") +
  geom_text( aes(label=Anzahl4W), size=4, position=position_fill(vjust = 0.5)) +
#  geom_hline(yintercept = 0.5) +
  # scale_fill_manual(values = YellowOrRed) +
  # scale_fill_brewer(palette="Blues") +
  scale_y_continuous(labels = scales::percent) +
  #  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  facet_wrap(vars(Altersgruppe)) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(  title = paste("Impfdurchbrüche nach Altersgruppe Kw",FromWoche,"-",ToWoche)
         , subtitle= paste("Deutschland, Stand:", heute)
         , x ="Outcome"
         , y = "Fälle" 
         , colour = "Geimpft / Ungeimpft"
         , caption = citation ) -> pp1

ggsave(  paste('png/Impfdurchbruch-',ToWoche,'.png', sep='')
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 150
)

SQL <- paste('select * from Impfdurchbruch where Woche = "',Woche,'" and Outcome <> "Impfquote";', sep='')
impfd <- RunSQL(SQL = SQL)

impfd %>% 
  mutate(name = factor( Outcome, 
                        levels=c( "Impfquote"
                                  , "Symptomatisch"
                                  , "Hospitalisiert"
                                  , "Intensivstation"
                                  , "Gestorben"))) %>%
  ggplot(
    aes( x = name, y = AnzahlKum, fill = Gruppe  )) +
  geom_bar( position = "fill", stat = "identity") +
#  geom_hline( yintercept = 0.5 ) +
  geom_text( aes( label = AnzahlKum ), size=4, position=position_fill(vjust = 0.5)) +
  # scale_fill_manual(values = YellowOrRed) +
  # scale_fill_brewer(palette="Blues") +
  scale_y_continuous(labels = scales::percent) +
  #  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  facet_wrap(vars(Altersgruppe)) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(  title = paste("Impfdurchbrüche nach Altersgruppe Kw 5 -",ToWoche)
         , subtitle= paste("Deutschland, Stand:", heute)
         , x ="Outcome"
         , y = "Fälle" 
         , colour = "Geimpft / Ungeimpft"
         , caption = citation ) -> pp1

ggsave(  paste('png/Impfdurchbruch-',ToWoche,'-Kum.png', sep='')
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 150
)
