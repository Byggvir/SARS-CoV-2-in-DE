#!/usr/bin/env Rscript
#
#
# Script: OWID.r
#
# Download CoViD-19 data from Our World in Data 
# and draw diagrams for selected locations.
#
# Locations are definde in a CVS dataset,
# For each locatione the dataset defindes the different waves
# by StartWeek and Endweek.
#
# Stand: 2021-12-22
#
# ( c ) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <- "OWID.r"

require( data.table )
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

# library( extrafont )
# extrafont::loadfonts()

# Set Working directory to git root

if ( rstudioapi::isAvailable() ){
 
 # When executed in RStudio
 SD <- unlist( str_split( dirname( rstudioapi::getSourceEditorContext()$path ),'/' ) )
 
} else {
 
 # When executi on command line 
 SD = ( function() return( if( length( sys.parents() ) == 1 ) getwd() else dirname( sys.frame( 1 )$ofile ) ) )()
 SD <- unlist( str_split( SD,'/' ) )
 
}

WD <- paste( SD[1:( length( SD )-1 )],collapse = '/' )

setwd( WD )


source( "R/lib/myfunctions.r" )
source( "R/lib/mytheme.r" )
source( "R/lib/sql.r" )

citation <- "© 2022 by Thomas Arend\nQuelle: Our World in Data"

options( 
 digits = 7
 , scipen = 7
 , Outdec = "."
 , max.print = 3000
 )

PandemieWoche <- function ( d ) {
  
  if ( is.Date( d ) ) {  
    return( as.integer( d - as.Date( "2019-12-30" ) )  %/% 7 + 1 ) 
  }
  else {
    warning( "Not a date", call. = TRUE)
  }
  
}

today <- Sys.Date()
PWeek <- PandemieWoche( today )
heute <- format( today, "%d %b %Y" )

CoronaWaves <- read_ods( path = "data/CoronaWaves2.ods"
             , sheet = 1 )

Welle <- function( Location, PWeeks ) {

 CoronaWaves %>% filter( location == Location ) -> WaveWeeks
  
 m <- max( PWeeks )
 
 WaveWeeks$EndWeek[WaveWeeks$EndWeek>m] <- m

 for ( i in 1:nrow( WaveWeeks ) ) {
 
  PWeeks[WaveWeeks$StartWeek[i]:WaveWeeks$EndWeek[i]] <- i
 
 }

  return ( factor( PWeeks, labels = paste( 'Welle', 1:nrow( WaveWeeks ), 'PandemieWoche', WaveWeeks$StartWeek, 'bis', WaveWeeks$EndWeek ) ) )

}

if ( ! exists( "OWID" ) ) {
 
 OWID <- read.csv( file = 'https://covid.ourworldindata.org/data/OWID-covid-data.csv' )
 # OWID <- read.csv( file = 'data/OWID-covid-data.csv' )

}

locations <- unique( CoronaWaves$location )

OWID$date <- as.Date( OWID$date )
OWID$Jahr <- year( OWID$date )
OWID$Kw <- isoweek( OWID$date )
OWID$Pw <- PandemieWoche( OWID$date )

OWID$weekly_hosp_admissions[is.na( OWID$weekly_hosp_admissions )] <- 0
OWID$new_tests[is.na( OWID$new_tests )] <- 0

Offset = 2 # Versatz Infektion - Tod

for ( l in locations ) {
 
  print( l )
 
  locationdata <- data.table( 
    Pw = 1:PWeek
    , new_cases = rep( 0,PWeek )
    , new_deaths = rep( 0,PWeek )
    , weekly_hosp_admissions = rep( 0,PWeek )
    , new_tests = rep( 0,PWeek )
  )

  OWID %>% filter( location == l ) -> ROWID
 
  cases <- aggregate( new_cases ~ Pw, data = ROWID, sum )
  deaths <- aggregate( new_deaths ~ Pw, data = ROWID, sum )
  hosp <- aggregate( weekly_hosp_admissions ~ Pw, data = ROWID, sum )
  tests <- aggregate( new_tests ~ Pw, data = ROWID, sum )
  
  locationdata$new_cases[cases$Pw] <- cases$new_cases
  locationdata$new_deaths[deaths$Pw - Offset] <- deaths$new_deaths
  locationdata$weekly_hosp_admissions[hosp$Pw] <- hosp$weekly_hosp_admissions
  locationdata$new_tests[tests$Pw] <- tests$new_tests
  locationdata$Welle <- Welle( l,locationdata$Pw )

  max_cases  <- max( locationdata$new_cases, na.rm = TRUE )
  max_deaths <- max( locationdata$new_deaths, na.rm = TRUE )
  max_hosp   <- max( locationdata$weekly_hosp_admissions, na.rm = TRUE )
  max_tests  <- max( locationdata$new_tests, na.rm = TRUE )
 
  scl <- max_cases / max_deaths
  locationdata %>% filter ( Pw < PWeek ) %>% ggplot() +
    geom_line( aes( x = Pw, y = new_cases ), color = 'black' ) +
    geom_line( aes( x = Pw, y = new_deaths * scl ), color = 'red' ) +
    scale_y_continuous(  sec.axis = sec_axis( ~./scl, name = "Todesfälle pro Woche", labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) )
            , labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
    theme_ta() +
      theme(  
        axis.text.y = element_text ( color = 'black' )
        , axis.title.y = element_text ( color = 'black' )
        , axis.text.y.right = element_text ( color = 'red' )
        , axis.title.y.right = element_text ( color = 'red' )
        
    ) +
    labs(  title = paste( "Fälle & Todesfälle", l )
       , subtitle = paste( "Stand:", heute )
       , x = "Pandemiewoche"
       , y = "Fälle pro Woche" 
       , colour = "Fälle" ) -> p1

  locationdata %>% filter ( Pw < PWeek & new_cases > 0 & new_deaths > 0 ) %>% ggplot( aes( group = Welle ) ) +
    geom_point( aes( x = new_cases, y = new_deaths, colour = Welle ), size = 6 ) +
    geom_smooth( aes( x = new_cases,y = new_deaths, colour = Welle ), method = 'lm' ) +
    geom_text( aes( x = new_cases, y = new_deaths, label = Pw ) ) +
    scale_x_continuous( labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
    facet_wrap(vars(Welle)) +
    theme_ta() +
    labs(  title = paste( "Todesfälle ~ Fälle" , l )
         , subtitle = paste( l, " Stand:", heute) #, "Offset", delta , "Wochen" )
         , x = "Fälle pro Woche"
         , y = "Todesfälle pro Woche" 
         , colour = "Wellen" ) -> p2


  gg1 <- grid.arrange( p1, p2, ncol = 1 )

  ggsave(  filename = paste( 'png/OWID/', l, '_deaths_', Offset,'.png', sep = '' )
         , plot = gg1
         , path = WD
         , device = 'png'
         , bg = "white"
         , width = 29.7 * 2
         , height = 21 * 2
         , units = "cm"
         , dpi = 300 
  )

if (max_hosp > 0) { 
  scl <- max_cases / max_hosp

  locationdata %>% filter ( Pw < PWeek ) %>% ggplot() +
    geom_line( aes( x = Pw, y = new_cases ), color = 'black' ) +
    geom_line( aes( x = Pw, y = weekly_hosp_admissions * scl ), color = 'red' ) +
    scale_y_continuous(  sec.axis = sec_axis( ~./scl, name = "Hospitalisierte pro Woche", labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) )
                       , labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
    theme_ta() +
    labs(  title = paste( "Fälle & Hospitalisierte", l )
           , subtitle = paste( "Stand:", heute )
           , x = "Pandemiewoche"
           , y = "Fälle pro Woche" 
          , colour = "Fälle" ) -> p3

  locationdata %>% filter ( Pw < PWeek & weekly_hosp_admissions > 0  & new_cases > 0) %>% ggplot( aes( group = Welle ) ) +
    geom_point( aes( x = new_cases, y = weekly_hosp_admissions, colour = Welle ), size = 6 ) +
    geom_smooth( aes( x = new_cases, y = weekly_hosp_admissions, colour = Welle ), method = 'lm' ) +
    geom_text( aes( x = new_cases,  y = weekly_hosp_admissions, label = Pw ) ) +
    scale_x_continuous( labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
    facet_wrap(vars(Welle)) +
    theme_ta() +
    labs(  title = paste( "Hospitalisierte ~ Fälle" , l )
         , subtitle = paste( l, " Stand:", heute )
         , x = "Fälle pro Woche"
         , y = "Hospitalisierte pro Woche" 
        , colour = "Wellen" 
        ) -> p4


  gg2 <- grid.arrange( p3, p4 )

  ggsave(  filename = paste( 'png/OWID/', l, '_hosp.png', sep = '' )
     , plot = gg2
     , path = WD
     , device = 'png'
     , bg = "white"
     , width = 29.7 * 2
     , height = 21 * 2
     , units = "cm"
     , dpi = 300 
 )
  
  scl <- max_hosp /  max_deaths
  
  locationdata %>% filter ( Pw < PWeek ) %>% ggplot() +
    geom_line( aes( x = Pw, y = weekly_hosp_admissions ), color = 'black' ) +
    geom_line( aes( x = Pw, y = new_deaths * scl ), color = 'red' ) +
    scale_y_continuous(  sec.axis = sec_axis( ~./scl, name = "Todesfälls pro Woche", labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) )
                         , labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
    theme_ta() +
    labs(  title = paste( "Hospitalisierte & Todesfälle", l )
           , subtitle = paste( "Stand:", heute )
           , x = "Pandemiewoche"
           , y = "Hospitalisierte pro Woche" 
           , colour = "Fälle" ) -> p7
  
  locationdata %>% filter ( Pw < PWeek & weekly_hosp_admissions > 0  & new_cases > 0) %>% ggplot( aes( group = Welle ) ) +
    geom_point( aes( x = weekly_hosp_admissions, y = new_deaths, colour = Welle ), size = 6 ) +
    geom_smooth( aes( x = weekly_hosp_admissions, y = new_deaths , colour = Welle ), method = 'lm' ) +
    geom_text( aes( x = weekly_hosp_admissions, y = new_deaths , label = Pw ) ) +
    scale_x_continuous( labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
    facet_wrap(vars(Welle)) +
    theme_ta() +
    labs(  title = paste( "Todesfälle ~ Hospitalisierte" , l )
           , subtitle = paste( l, " Stand:", heute )
           , x = "Hospitalisierte pro Woche"
           , y = "Todesfälle pro Woche" 
           , colour = "Wellen" 
    ) -> p8
  
  
  gg4 <- grid.arrange( p7, p8 )
  
  ggsave(  filename = paste( 'png/OWID/', l, '_hosp-deaths.png', sep = '' )
           , plot = gg4
           , path = WD
           , device = 'png'
           , bg = "white"
           , width = 29.7 * 2
           , height = 21 * 2
           , units = "cm"
           , dpi = 300 
  )
  
}
  
if ( max_tests > 0 ) {
  
scl <- max_cases / max_tests

locationdata %>% filter ( Pw < PWeek ) %>% ggplot() +
  geom_line( aes( x = Pw, y = new_cases ), color = 'black' ) +
  geom_line( aes( x = Pw, y = new_tests * scl ), color = 'red' ) +
  scale_y_continuous(  sec.axis = sec_axis( ~./scl, name = "Tests pro Woche", labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) )
                       , labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
  theme_ta() +
  labs(  title = paste( "Fälle & Testungen", l )
         , subtitle = paste( "Stand:", heute )
         , x = "Pandemiewoche"
         , y = "Fälle pro Woche"
         , colour = "Fälle" ) -> p5

locationdata %>% filter ( Pw < PWeek & new_tests > 0  & new_cases > 0) %>% ggplot( aes( group = Welle ) ) +
  geom_point( aes( x = new_cases, y = new_tests, colour = Welle ), size = 6 ) +
  geom_smooth( aes( x = new_cases, y = new_tests, colour = Welle ), method = 'lm' ) +
  geom_text( aes( x = new_cases,  y = new_tests, label = Pw ) ) +
  scale_x_continuous( labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
  facet_wrap(vars(Welle)) +
  theme_ta() +
  labs(  title = paste( "Testungen ~ Fälle" , l )
         , subtitle = paste( l, " Stand:", heute )
         , x = "Fälle pro Woche"
         , y = "Testungen pro Woche"
         , colour = "Wellen" ) -> p6


gg3 <- grid.arrange( p5, p6, ncol = 1 )

ggsave(  filename = paste( 'png/OWID/', l, '_tests.png', sep = '' )
         , plot = gg3
         , path = WD
         , device = 'png'
         , bg = "white"
         , width = 29.7 * 2
         , height = 21 * 2
         , units = "cm"
         , dpi = 300
)
}
  
 # for (w in unique(locationdata$Welle)) {
 #    print (w)
 #    ra <- lm( weekly_hosp_admissions ~ new_cases, data = locationdata %>% filter( Welle == w ) ) 
 #    print( summary( ra ) )
 # }
}
