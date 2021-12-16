#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2021-12-10
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#
MyScriptName <-"SouthAfrica"

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


require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nOur World in Data"

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)
today <- Sys.Date()
heute <- format(today, "%d %b %Y")

region <- c('Denmark', 'South Africa')

daten <- read.csv(file = 'https://covid.ourworldindata.org/data/owid-covid-data.csv')

daten$date <- as.Date(daten$date)

for (r in region ) {
  
  daten %>% filter( location == r ) -> rdaten

  max_cases <- max( rdaten$new_cases_smoothed_per_million, na.rm=TRUE)
  max_hosp  <- max( rdaten$weekly_hosp_admissions_per_million,na.rm=TRUE)

  scl <- max_cases / max_hosp

  rdaten %>% ggplot() +
    geom_line(data = rdaten %>% filter( ! is.na(new_cases_smoothed_per_million) ), aes( x = date, y = new_cases_smoothed_per_million ), color = 'blue') +
    geom_line(data = rdaten %>% filter( ! is.na(weekly_hosp_admissions_per_million) ), aes( x = date, y = weekly_hosp_admissions_per_million * scl), color = 'red') +
    scale_y_continuous(  sec.axis = sec_axis(~./scl, name = "Hospitalisierung", labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ))
                       , labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  theme_ipsum() +
  theme(  plot.title = element_text( size = 48 )
          , axis.text.y  = element_text ( color = 'blue' )
          , axis.title.y = element_text ( color ='blue' )
          , axis.text.y.right = element_text ( color = 'red' )
          , axis.title.y.right = element_text ( color='red' )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
          ) ) +
  labs(  title = "Fallzahlen + Hospitalisierte"
         , subtitle = paste(r, "Stand:", heute)
         , x = "Datum"
         , y = "Fälle" 
         , colour = "Fälle")


ggsave(  filename = paste( 'png/', r, '.png', sep = '' )
         , path = WD
         , device = 'png'
         #, type = "cairo-png"
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 300 
         )

rdaten %>% ggplot() +
  geom_point( data = rdaten %>% filter( ! is.na(new_cases_smoothed_per_million) & ! is.na(weekly_hosp_admissions_per_million)), aes( x = new_cases_smoothed_per_million,y = weekly_hosp_admissions_per_million ), color = 'blue') +
  geom_smooth( data = rdaten %>% filter( ! is.na(new_cases_smoothed_per_million) & ! is.na(weekly_hosp_admissions_per_million)), aes( x = new_cases_smoothed_per_million,y = weekly_hosp_admissions_per_million ), color = 'blue') +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    theme_ipsum() +
  theme(  plot.title = element_text( size = 48 )
          , axis.text.y  = element_text ( color = 'blue' )
          , axis.title.y = element_text ( color ='blue' )
          , axis.text.y.right = element_text ( color = 'red' )
          , axis.title.y.right = element_text ( color='red' )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
          ) ) +
  labs(  title = "Hospitalisierte ~ Fallzahlen"
         , subtitle = paste(r ," Stand:", heute)
         , x = "Fälle pro Tag"
         , y = "Hospitalisierte pro Woche" 
         , colour = "Fälle")


ggsave(  filename = paste( 'png/', r, '-sp.png', sep = '' )
         , path = WD
         , device = 'png'
         #, type = "cairo-png"
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 300 
)
 ra <- lm(weekly_hosp_admissions_per_million ~ new_cases_smoothed_per_million, data = rdaten %>% filter( ! is.na(new_cases_smoothed_per_million) & ! is.na(weekly_hosp_admissions_per_million)) ) 

 print(summary(ra))

}
