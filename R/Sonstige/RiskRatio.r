#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"RiskRatio"

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

WD <- paste(SD[1:(length(SD)-2)],collapse='/')

setwd(WD)

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date() - 1
heute <- format(today, "%d %b %Y")

u <- function ( U , R ) {
  
  return ( U / ( R * ( 1 - U ) + U ) )

  }


g <- function ( G , R ) {
  
  return ( R * G  / ( 1 - G * ( 1 - R ) ) )
  
}

G <- function ( S , g ) {
  
  R = 1 - S
  return ( 1 / ( 1 + ( 1 / g - 1 ) * R ) ) 
  
}     


S <- function ( G , g ) {
  
  return ( 1 - g / G * ( 1 - G ) / ( 1 - g )) 
  
}     


x <- data.table( 
  x = seq(0,1,0.001)
)

x %>% ggplot( aes( x = x ) ) +
  geom_function( fun = g , args = list( R = 0.01 ), aes( colour = '99 %' ) ) +
  geom_function( fun = g , args = list( R = 0.05 ), aes( colour = '95 %' ) ) +
  geom_function( fun = g , args = list( R = 0.1 ), aes( colour = '90 %' ) ) +
  geom_function( fun = g , args = list( R = 0.2 ), aes( colour = '80 %' ) ) +
  geom_function( fun = g , args = list( R = 0.3 ), aes( colour = '70 %' ) ) +
  geom_function( fun = g , args = list( R = 0.4 ), aes( colour = '60 %' ) ) +
  geom_function( fun = g , args = list( R = 0.5 ), aes( colour = '50 %' ) ) +
  scale_x_continuous( labels = scales::percent ) +
  scale_y_continuous( labels = scales::percent ) +
  coord_fixed ( expand = TRUE, clip = 'on' ) +
  theme_ta() + 
  labs(  title = "Anteil der Geimpften an Erkrankten"
         , subtitle = 'in Abhängigkeit von Impfquote und Schutz'
         , x = "Impfquote"
         , y = "Anteil der Geimpften an Erkrankten"
         , caption = citation 
         , colour = "Schutz" ) -> p

ggsave(  paste('png/RiskRatio', '.png', sep='')
         , device = 'png'
         , bg = "white"
         , width = 2160
         , height = 2160                                                                                                                     
         , units = "px"
)

x <- data.table( 
  x = seq(0.5,1,0.001)
)

x %>% ggplot( aes( x = x ) ) +
  geom_function( fun = S , args = list( g = 0.5 ), aes( colour = '50 %' ) ) +
  geom_function( fun = S , args = list( g = 0.6 ), aes( colour = '60 %' ) ) +
  geom_function( fun = S , args = list( g = 0.7 ), aes( colour = '70 %' ) ) +
  geom_function( fun = S , args = list( g = 0.75 ), aes( colour = '75 %' ), size = 2 ) +
  geom_function( fun = S , args = list( g = 0.8 ), aes( colour = '80 %' ) ) +
  geom_function( fun = S , args = list( g = 0.9 ), aes( colour = '90 %' ) ) +
  scale_x_continuous( labels = scales::percent ) +
  scale_y_continuous( labels = scales::percent ) +
  coord_cartesian(ylim = c( 0, 1 ) ) +
  theme_ta() + 
  labs(  title = "Schutz, wenn x % der Erkrankten geimpft sind"
         , subtitle = 'in Abhängigkeit von der Impfquote'
         , x = "Impfquote"
         , y = "Schutz"
         , caption = citation 
         , colour = "Anteil Erkrankter" ) -> p

ggsave(  paste('png/RiskRatio-2', '.png', sep='')
         , device = 'png'
         , bg = "white"
         , width = 3840
         , height = 2160                                                                                                                     
         , units = "px"
)
