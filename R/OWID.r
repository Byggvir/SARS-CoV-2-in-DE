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
source( "R/lib/color_palettes.r" )

citation <- "© 2021 by Thomas Arend\nQuelle: Our World in Data"

options( 
 digits = 7
 , scipen = 7
 , Outdec = "."
 , max.print = 3000
 )

PWeek <- ( as.integer( Sys.Date() - as.Date( "2019-12-29" ) ) - 1 ) %/% 7 + 1 

today <- Sys.Date()
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

if ( ! exists( "owid" ) ) {
 
 owid <- read.csv( file = 'https://covid.ourworldindata.org/data/owid-covid-data.csv' )
 # owid <- read.csv( file = 'data/owid-covid-data.csv' )

}

locations <- unique( CoronaWaves$location )

owid$date <- as.Date( owid$date )
owid$Jahr <- year( owid$date )
owid$Kw <- isoweek( owid$date )
owid$Pw[year( owid$date ) == 2020] <- isoweek( owid$date[year( owid$date ) == 2020] )
owid$Pw[year( owid$date ) == 2021 & isoweek( owid$date ) == 53] <- isoweek( owid$date[year( owid$date ) == 2021 & isoweek( owid$date ) == 53] )
owid$Pw[year( owid$date ) == 2021 & isoweek( owid$date ) < 53] <- isoweek( owid$date[year( owid$date ) == 2021 & isoweek( owid$date ) < 53] ) + 53
owid$new_tests[is.na( owid$new_tests )] <- 0

tt <- function () { 
 theme_ta( base_family = 'Helvetica' ) +
 theme(  
       axis.text.y = element_text ( color = 'blue' )
     , axis.title.y = element_text ( color = 'blue' )
     , axis.text.y.right = element_text ( color = 'red' )
     , axis.title.y.right = element_text ( color = 'red' )
     )
}

for ( l in locations ) {
 
 print( l )
 
 locationdata <- data.table( 
  Pw = 1:PWeek
  , new_cases = rep( 0,PWeek )
  , new_deaths = rep( 0,PWeek )
  , weekly_hosp_admissions = rep( 0,PWeek )
  , new_tests = rep( 0,PWeek )
 )

 owid %>% filter( location == l ) -> rowid
 
 cases <- aggregate( new_cases ~ Pw, data = rowid, sum )
 deaths <- aggregate( new_deaths ~ Pw, data = rowid, sum )
 hosp <- aggregate( weekly_hosp_admissions ~ Pw, data = rowid, sum )
 tests <- aggregate( new_tests ~ Pw, data = rowid, sum )
 
 # b <- 60
 # c <- match(max(cases$new_cases[1:b]),cases$new_cases[1:b])
 # d <- match(max(deaths$new_deaths[1:b]),deaths$new_deaths[1:b])
 # 
 # delta <- deaths$Pw[d] - cases$Pw[c]
 # if ( delta < 0 ) {
 #   delta <- 3
 # }
 
 locationdata$new_cases[cases$Pw] <- cases$new_cases
 locationdata$new_deaths[deaths$Pw] <- deaths$new_deaths
 locationdata$weekly_hosp_admissions[hosp$Pw] <- hosp$weekly_hosp_admissions
 locationdata$new_tests[tests$Pw] <- tests$new_tests
 
 locationdata$Welle <- Welle( l,locationdata$Pw )
 
 max_cases  <- max( locationdata$new_cases, na.rm = TRUE )
 max_deaths <- max( locationdata$new_deaths, na.rm = TRUE )
 max_hosp   <- max( locationdata$weekly_hosp_admissions, na.rm = TRUE )
 max_tests  <- max( locationdata$new_tests, na.rm = TRUE )
 
 scl <- max_cases / max_deaths

locationdata %>% filter ( Pw < PWeek ) %>% ggplot() +
  geom_line( aes( x = Pw, y = new_cases ), color = 'blue' ) +
  geom_line( aes( x = Pw, y = new_deaths * scl ), color = 'red' ) +
  scale_y_continuous(  sec.axis = sec_axis( ~./scl, name = "Todesfälle pro Woche", labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) )
            , labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
 tt() +
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
  tt() +
  labs(  title = paste( "Todesfälle ~ Fälle" , l )
         , subtitle = paste( l, " Stand:", heute, "Offset", delta , "Wochen" )
         , x = "Fälle pro Woche"
         , y = "Todesfälle pro Woche" 
         , colour = "Wellen" ) -> p2


gg <- grid.arrange( p1, p2, ncol = 1 )

ggsave(  filename = paste( 'png/OWID/', l, '-deaths.png', sep = '' )
         , plot = gg
         , path = WD
         , device = 'png'
         , bg = "white"
         , width = 29.7 * 2
         , height = 21 * 2
         , units = "cm"
         , dpi = 300 
)

scl <- max_cases / max_hosp

locationdata %>% filter ( Pw < PWeek ) %>% ggplot() +
  geom_line( aes( x = Pw, y = new_cases ), color = 'blue' ) +
  geom_line( aes( x = Pw, y = weekly_hosp_admissions * scl ), color = 'red' ) +
  scale_y_continuous(  sec.axis = sec_axis( ~./scl, name = "Hospitalisierte pro Woche", labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) )
                       , labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
  tt() +
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
 tt() +
 labs(  title = paste( "Hospitalisierte ~ Fälle" , l )
     , subtitle = paste( l, " Stand:", heute )
     , x = "Fälle pro Woche"
     , y = "Hospitalisierte pro Woche" 
     , colour = "Wellen" ) -> p4


gg <- grid.arrange( p3,p4, ncol = 1 )

ggsave(  filename = paste( 'png/OWID/', l, '.png', sep = '' )
     , plot = gg
     , path = WD
     , device = 'png'
     , bg = "white"
     , width = 29.7 * 2
     , height = 21 * 2
     , units = "cm"
     , dpi = 300 
 )

 # ra <- lm( weekly_hosp_admissions_per_million ~ new_cases_smoothed_per_million, data = rowid %>% filter( ! is.na( new_cases_smoothed_per_million ) & ! is.na( weekly_hosp_admissions_per_million ) ) ) 

 #print( summary( ra ) )

}
