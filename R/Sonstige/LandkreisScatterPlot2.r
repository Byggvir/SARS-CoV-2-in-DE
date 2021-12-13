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

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nGitHub SARS-CoV-2 Infektionen"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

untilday <- Sys.Date() - 4
# untilday <- as.Date("2021-11-27")
z1 <- format(untilday, "%Y-%m-%d")
z2 <- format(untilday-6, "%Y-%m-%d")
z3 <- format(untilday-7, "%Y-%m-%d")
z4 <- format(untilday-13, "%Y-%m-%d")


SQL <- paste(
    'call Bundesland714("',untilday,'");'
  , sep = ' ')
Bundesland <- RunSQL(SQL)

SQL <- paste(
    'call Landkreis714("',untilday,'");'
  , sep = ' ')

Landkreise <- RunSQL(SQL = SQL)

for ( B in Bundesland[,2] ) {
  
Landkreise %>% filter(Bundesland == B) %>% ggplot() +
  stat_ellipse(aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000, size = EW_insgesamt ), type = "t", geom = "polygon", alpha = 0.1 ) +
  geom_abline( intercept = 0, slope = 1, color ='red' ) +
  geom_abline( intercept = 0, slope = 0.75, color ='green' ) +
  geom_point( aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000,  size = EW_insgesamt), color = 'darkgrey') +
  geom_point( data = Bundesland %>% filter(Bundesland == B)
              , aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000)
              , shape = 3, stroke = 4, fill = "blue", color = "darkblue", alpha = 0.5, size = 10 ) +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_fill_viridis(discrete = T) +
  expand_limits( x = 0 , y = 0 ) +
  theme_ipsum() +
  theme(  plot.title = element_text( size = 24 )
          , legend.position="right"
         , strip.text.x = element_text (
          size = 12
          , color = "black"
          , face = "bold.italic"
        ) ) +
  labs(  title= paste('Landkreise nach Bundesland bis Meldedatum', untilday)
       , subtitle = "Änderung Fallzahlen pro 100.000 Einwohner"
       , x = paste( 'Fälle von', z4, 'bis', z3, 'pro 100.000' )
       , y = paste( 'Fälle von', z2, 'bis', z1, 'pro 100.000' )
       , caption = citation )

ggsave(  paste('png/Landkreis-Tendenz-',B, '.png', sep='')
       , device = 'png'
       , type = "cairo-png"
       , bg = "white"
       , width = 29.7 * 2
       , height = 21 * 2
       , units = "cm"
       , dpi = 300 )
}

Bundesland %>% ggplot() +
  stat_ellipse(aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000 ), type = "t", geom = "polygon", alpha = 0.1 ) +
  geom_abline( intercept = 0, slope = 1, color ='red' ) +
  geom_abline( intercept = 0, slope = 0.75, color ='green' ) +
  geom_point( aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000, colour = Bundesland), size= 18, alpha = 0.5) +
  geom_text( aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000, label = Abk), vjust=0.5, hjust=0.5, size= 6) +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_fill_viridis(discrete = T) +
  expand_limits( x = 0 , y = 0 ) +
  theme_ipsum() +
  theme(  plot.title = element_text( size = 24 )
          , plot.subtitle = element_text( size = 18 )
          , legend.position="right"
          , axis.title.x = element_text( size = 18 )
          , axis.title.y = element_text( size = 18 )
          , axis.text.x = element_text( size = 18 )
          , axis.text.y = element_text( size = 18 )
          , strip.text.x = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic"
          ) 
          , legend.text = element_text(
            size = 12
            , color = "black"
            , face = "bold.italic"
          )
          , legend.title = element_text(
            size = 18
            , color = "black"
            , face = "bold.italic"
          )) +
  labs(  title= paste('Veränderung der Corona-Fallzahlen der Bundesländer bis Meldedatum', untilday)
         , subtitle = paste('Fallzahlen pro 100.000 Einwohner pro 7-Tageszeitraum') 
         , x = paste( 'Fälle von', z4, 'bis', z3, 'pro 100.000' )
         , y = paste( 'Fälle von', z2, 'bis', z1, 'pro 100.000' )
         , caption = citation 
         , colour = 'Bundesland')

ggsave(  paste('png/BundeslandTendenz', '.png', sep='')
         , type = "cairo-png"
         , bg = "white"
         , width = 29.7 * 2
         , height = 21 * 2
         , units = "cm"
         , dpi = 300 )
