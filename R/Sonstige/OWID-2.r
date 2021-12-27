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

WD <- paste( SD[1:( length( SD )-2 )],collapse = '/' )

setwd( WD )


require( data.table )

source( "R/lib/myfunctions.r" )
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
  
 return( ( as.integer( d - as.Date( "2019-12-29" ) ) - 1 ) %/% 7 + 1 ) 

}

today <- Sys.Date()
heute <- format( today, "%d %b %Y" )

PWeek <- PandemieWoche(today)

CoronaWaves <- read_ods( path = "data/CoronaWaves.ods"
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

CoronaWaves$PeekWeek <- rep( 0, nrow( CoronaWaves ) )

for ( i in 1:nrow( CoronaWaves ) ) {
 
 cases <- aggregate( new_cases ~ Pw, data = owid %>% filter( location == CoronaWaves$location[i] & Pw >=  CoronaWaves$StartWeek[i] & CoronaWaves$EndWeek[i] >= Pw), sum )
 m <- match( max( cases$new_cases ), cases$new_cases )

 CoronaWaves$PeekWeek[i] <- cases$Pw[m[1]]
 
}

print( CoronaWaves )

write_ods( CoronaWaves
           , path = "data/CoronaWaves2.ods"
           , sheet = 'Tabelle1'
           , update = TRUE
            )
