#!/usr/bin/env Rscript
#
#
# Script: Meldeverzug.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"Meldeverzug"

library( tidyverse )
library( REST )
library( grid )
library( gridExtra )
library( gtable )
library( lubridate )
library( readODS )
library( ggplot2 )
library( ggrepel )
library( viridis )
library( hrbrthemes )
library( scales )
library( ragg )
library(extrafont)
extrafont::loadfonts()

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

citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- paste('select A.Meldedatum,A.Bestandsdatum,datediff(A.Bestandsdatum,A.Meldedatum) - 1 as Verzug, weekday(A.Meldedatum) as
DoW, sum(A.AnzahlFall) / Gesamt * 100 as Anteil from NeuInfektionen as A join NeuI as B on A.Meldedatum = B.Meldedatum where A.Meldedatum >= "2021-05-01" and A.Meldedatum < "2021-12-01" and datediff(A.Bestandsdatum,A.Meldedatum) < 15 group by A.Bestandsdatum,A.Meldedatum order by A.Meldedatum,A.Bestandsdatum;
', sep='')

daten <- RunSQL(SQL = SQL)

daten$WTag <- factor(daten$DoW,labels = WochentageLang)

daten %>% filter ( Verzug < 5 ) %>% ggplot( ) +
  geom_boxplot(aes( x = Verzug, y = Anteil,group = Verzug )) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  facet_wrap( vars( WTag ), nrow = 2 ) +
  theme_ipsum( base_family = 'Helvetica' ) +
  theme(  plot.title = element_text( size = 36 )
          , axis.text.y  = element_text( color = 'black', size = 24 )
          , axis.title.y = element_text( color = 'black', size = 24 )
          , strip.text.x = element_text(
              size = 24
              , color = "black"
              , face = "bold.italic"
          ) ) + 
  labs(  title = "Anteil der nach n Tagen gemeldeten an den gesamten Fällen des Tages"
       , subtitle= paste("Deutschland, Stand:", heute)
       , x = "Verzug"
       , y = "Anteil [%]" 
       , caption = citation 
       )

ggsave(  'png/Verzug.png'
       , device = 'png'
       , bg = "white"
       , width = 29.7 * 2
       , height = 21 * 2
       , units = "cm"
       , dpi = 300 )

# for (d in 0:6) {
#   print(Wochentage[d+1])
#   for (v in 0:4) {
#     a <- daten %>% filter(DoW==d & Verzug ==v)
#     print(mean(a[,5]))
#   }
# }
