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
  
  #  When executing on command line 
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

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date()
heute <- format(today, "%F")

Stichtag <- '2021-12-29'

citation <- paste('© 2022 by Thomas Arend, Stand: ',heute,'\nQuelle: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153',sep='')

SQL <- paste( 'select * from ImpfQuoteBL as Q join RZahl as R on R.IdBundesland = Q.IdBundesland where Altersgruppe = "A0+" and Datum = "'
              , Stichtag
              , '" and Zeitraum = 20 ;'
              , sep=''
)
#if ( ! exists("IQuote")) {
  IQuote <- RunSQL(SQL = SQL)
#}

IQuote %>% ggplot() +
  geom_point( aes( x = 1 - Quote, y = R, colour = factor(Bundesland)), size = 18) +
  geom_text( aes( x = 1 - Quote, y = R, label = Abk ), size=6, vjust=0.5,hjust=0.5) +
  geom_smooth(aes( x = 1 - Quote, y = R), method = 'gam' , formula = y ~ x) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = T) +
  labs(  title = "SARS-CoV-2\nR-Zahl ~ Ungeimpfenquote nach Bundesland"
         , subtitle= paste("Stichtag: ", Stichtag,sep ='')
         , x = "Anteil Ungeimpfter [%]"
         , y = "R-Zahl" 
         , colour = "Fälle / Todesfälle"
         , caption = citation ) +
  theme_ipsum() +
  theme(  plot.title=element_text(size=48, hjust=0.5, face="italic", color="black")
        , plot.subtitle=element_text(size=36, hjust=0.5, face="italic", color="black")
        , legend.text = element_text(size = 18)
        , legend.title = element_text(size = 24)
        , axis.text.y  = element_text ( color = 'black', size = 18 )
        , axis.title.y = element_text ( color='black', size = 24 )
        , axis.text.x = element_text ( color = 'black', size = 18 )
        , axis.title.x = element_text ( color='black', size = 24 )
        , strip.text.x = element_text (
          size = 24
          , color = "black"
          , face = "bold.italic"
        ) )

ggsave(  paste('png/FZIQuoteR_',Stichtag,'.png', sep = '')
       , bg = "white"
       , width = 29.7 * 2
       , height = 21 * 2
       , units = "cm"
       , dpi = 300 )
