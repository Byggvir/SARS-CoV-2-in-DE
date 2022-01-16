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

MyScriptName <- "Ferien.r"

library( tidyverse )
library( REST )
library( grid )
library( gridExtra )
library( gtable )
library( lubridate )
library( htmltab )
library( readODS )
library( XML )
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

today <- Sys.Date()
heute <- format( today, "%d %b %Y" )

tryAsNumeric = function(  node  ) {
  val = XML::xmlValue(  node  )
  ans = as.numeric(  gsub(  ",", "", val  )  )
  if(  is.na(  ans  ) )
    return( val )
  else
    return( "ans" )
}

bFun <- function(node) {
  x <- XML::xmlValue(node)
  gsub('\\+', '', x)
}

if ( ! exists( "Ferien" ) ) {
 
  url <- "https://www.schulferien.org/Schulferien_nach_Ferien/Weihnachtsferien/2021/weihnachtsferien_2021.html"
  Ferien = as.data.table( htmltab( url, which = 1, colNames = c( "Land","Zeitraum" ) ) )

}

Ferien$Land <- gsub('\\*','', Ferien$Land)
Ferien$Zeitraum <- gsub('\\*','', Ferien$Zeitraum)

print(Ferien)
write_ods( Ferien
           , path = "data/Ferien.ods"
           , sheet = 'Tabelle1'
           , update = TRUE
            )
