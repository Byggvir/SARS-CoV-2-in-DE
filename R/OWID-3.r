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

OWID$date <- as.Date( OWID$date )
OWID$Jahr <- year( OWID$date )
OWID$Kw <- isoweek( OWID$date )
OWID$Pw <- PandemieWoche( OWID$date )

cases <- aggregate( new_cases ~ Pw + continent + location, data = OWID, sum )
write.csv( cases, file = 'data/OWID_Cases.csv', quote = FALSE )
