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

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nSARS-CoV-2 Wochenberichte"

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

Woche <- "2021-11-22"
ToWoche <- isoweek(as.Date(Woche))
FromWoche <- ToWoche - 3

SQL <- paste('select * from ImpfDSchutz;', sep='')
Schutz <- RunSQL(SQL = SQL)

Schutz %>% 
  mutate( ordered = factor( Outcome, 
                        levels=c(  "Symptomatisch"
                                  , "Hospitalisiert"
                                  , "Intensivstation"
                                  , "Gestorben"))) %>%
  ggplot(
    aes( x = Woche, y = Schutz, group = Altersgruppe, color = Altersgruppe  )) +
  geom_line() + geom_point() +
  facet_wrap(vars(ordered)) +
  # scale_fill_manual(values = YellowOrRed) +
  # scale_fill_brewer(palette="Blues") +
  scale_y_continuous(labels = scales::percent) +
  # expand_limits( y = 0 ) +
  #  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(  title = paste("Impfung: Effektivität der letzten vier Wochen nach Endpunkt und Altersgruppe ")
         , subtitle= paste("Deutschland, Stand:", heute)
         , x = "Woche"
         , y = "Schutz [%]" 
         , colour = "Altersgruppe"
         , caption = citation ) -> p

ggsave(  paste('png/ImpfDSchutzEntw.png', sep='')
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 150
)
