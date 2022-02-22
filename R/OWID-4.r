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

today <- Sys.Date()
PWeek <- PandemieWoche( today ) - 1
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

OWID %>% filter( location %in% locations ) -> ROWID
 
  cases = aggregate( new_cases ~ Pw + location, data = ROWID, sum )
  deaths = aggregate( new_deaths ~ Pw + location, data = ROWID, sum )

for  ( l in locations) {
  
  for ( Offset in 0:0) { 
  
    max_cases <- max( cases$new_cases[cases$location == l], na.rm = TRUE )
    max_deaths <- max( deaths$new_deaths[deaths$location == l], na.rm = TRUE )

    scl <- ceiling(max_cases / max_deaths)
    
    ggplot() +
      geom_line( data = cases %>% filter ( location == l), aes( x = Pw, y = new_cases ), color = 'black' ) +
      geom_line( data = deaths %>% filter ( location == l), aes( x = Pw, y = new_deaths * scl ), color = 'red' ) +
      scale_y_continuous(  sec.axis = sec_axis( ~./scl, name = "Todesfälle pro Woche", labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) )
            , labels = function ( x ) format( x, big.mark = ".", decimal.mark = ',', scientific = FALSE ) ) +
      theme_ta() +
      theme(  
        axis.text.y = element_text ( color = 'black' )
        , axis.title.y = element_text ( color = 'black' )
        , axis.text.y.right = element_text ( color = 'red' )
        , axis.title.y.right = element_text ( color = 'red' )
        
      )  +
      labs(  title = paste( "Fälle & Todesfälle", l )
        , subtitle = paste( "Stand:", heute )
        , x = "Pandemiewoche"
        , y = "Fälle pro Woche" 
        , colour = "Fälle" ) -> p1
    
    ggsave(  filename = paste( 'png/OWID/A_',l,'_', Offset,'.png', sep = '' )
           , plot = p1
           , path = WD
           , device = 'png'
           , bg = "white"
           , width = 29.7 * 2
           , height = 21 * 2
           , units = "cm"
           , dpi = 300 
  )

  } # End Offset
}
