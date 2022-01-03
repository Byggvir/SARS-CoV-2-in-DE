#!/usr/bin/env Rscript

MyScriptName <- 'RKI-Omikron.r'

require( data.table  )

library( tidyverse  )
library( REST )
library( gtable )
library( lubridate )
library( ggplot2 )
library( grid )
library( gridExtra )
library( ggpubr )
library( viridis )
library( hrbrthemes )
library( scales )
library( Cairo )
library( htmltab )
library( readODS )
library( XML )

# library( extrafont )
# extrafont::loadfonts()

# Set Working directory to git root

if ( rstudioapi::isAvailable() ){
  
  # When executed in RStudio
  SD <- unlist( str_split( dirname( rstudioapi::getSourceEditorContext()$path ),'/' ) )
  
} else {
  
  #  When executing on command line 
  SD = ( function() return(  if( length( sys.parents() ) == 1 ) getwd() else dirname( sys.frame( 1 )$ofile )  ) )()
  SD <- unlist( str_split( SD,'/' ) )
  
}

WD <- paste( SD[1:( length( SD )-1 )],collapse='/' )

setwd( WD )

source( "R/lib/myfunctions.r" )
source( "R/lib/mytheme.r" )
source( "R/lib/sql.r" )
source( "R/lib/color_palettes.r" )

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021) -Tägliche Übersicht zu Omikron-Fällen"

options(  
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
 )

today <- Sys.Date()
heute <- format( today, "%d %b %Y" )

e <- function ( x, a = 0 , b = 1, xoffset = 0 ) {
  
  return ( exp( a + b *( x - xoffset ) ) )
  
}

s <- function ( y ) {
  
  return (  -log( y/( 1-y ) ) )
  
}

sigmoid <- function ( x,a=0,b=1 ) {

  return (  1/( 1+exp( a+b*x ) ) )
  
}

plotsigmoid <- function ( a, b, col, xlim, ylim ) {
  
  par(  new=TRUE  )
  
  curve(   sigmoid( x,a,b )
          , from=xlim[1]
          , to=xlim[2]
          , lty = 2
          , lwd = 5
          , xlim = xlim
          , ylim = ylim
          , col = col
          , xlab = ""
          , ylab = ""
          , xaxt = "n"
          , yaxt = "n"
          , cex.axis = 2
          , cex.lab = 3
          , cex.main = 5      
   )
  
}

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

url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/Omikron-Faelle/Omikron-Faelle.html?__blob=publicationFile"
RKI_Omikron = as.data.table( htmltab( url, which = 1, bodyFun = bFun, colNames = c( "Meldewoche","Anzahl","Differenz","Quote" ) ) )
RKI_OmikronAlter = as.data.table( htmltab( url, which = 2, bodyFun = bFun, colNames = c( "Art","A00-A04","A05-A14","A15-A34","A35-A59","A60-A79","A80+","Gesamt" ) ) )

write.csv(RKI_Omikron,file = paste( 'data/Omikron_', format( today, "%Y%m%d" ), '.csv',sep=''))
write.csv(RKI_OmikronAlter,file = paste( 'data/Omikron_Alter_', format( today, "%Y%m%d" ), '.csv',sep=''))

RKI_Omikron$Meldewoche <- as.numeric( RKI_Omikron$Meldewoche )
RKI_Omikron$Omikronwoche <- 1:nrow(RKI_Omikron) - 1
RKI_Omikron$Anzahl <- as.numeric( RKI_Omikron$Anzahl )
RKI_Omikron$Differenz <- as.numeric( RKI_Omikron$Differenz )
RKI_Omikron$Quote <- as.numeric( RKI_Omikron$Quote )

print( RKI_Omikron )

Anfang <- RKI_Omikron$Meldewoche[1]
BisWoche <- max(RKI_Omikron$Omikronwoche) - 1

CI <- 0.95

ra <- lm( log( Anzahl ) ~ Omikronwoche , data = RKI_Omikron %>% filter( Omikronwoche <= BisWoche )  )
ci <- confint( ra,level = CI )

print(summary(ra))

a <- c(  ci[1,1], ra$coefficients[1] , ci[1,2] )
b <- c(  ci[2,1], ra$coefficients[2] , ci[2,2] )

R <- exp( b*4/7 )

shaderibbon <- NULL
shaderibbon = data.table( 
  x = seq( min( RKI_Omikron$Meldewoche ),max( RKI_Omikron$Meldewoche ),length.out=100 )
 )

shaderibbon$ylower <- sapply( shaderibbon$x, FUN = function( x ){ e( x,a[1],b[1], xoffset = Anfang )} )
shaderibbon$yupper <- sapply( shaderibbon$x, FUN = function( x ){ e( x,a[3],b[3], xoffset = Anfang )} )


RKI_Omikron %>% ggplot(  aes(  x = Meldewoche  )  ) +
  geom_ribbon(  data = shaderibbon, aes( x = x, ymin = ylower, ymax = yupper ), color = 'grey',  alpha=0.1 ) +
  geom_function(  fun = e, args = list( a = a[1], b = b[1], xoffset = Anfang ), aes( colour= 'Untere Grenze' ), show.legend = TRUE, linetype = 'dotted', size = 1.5 ) +
  geom_function(  fun = e, args = list( a = a[2], b = b[2], xoffset = Anfang ), aes( colour= 'Mittelwert' ), show.legend = TRUE, linetype = 'dotted', size = 1.5 ) +
  geom_function(  fun = e, args = list( a = a[3], b = b[3], xoffset = Anfang), aes( colour= 'Obere Grenze' ), show.legend = TRUE, linetype = 'dotted', size = 1.5  ) +
  geom_line(  data = RKI_Omikron %>% filter( Omikronwoche <= BisWoche ), aes( y = Anzahl, colour = 'Werte' ), show.legend = TRUE, size = 2 ) +
  geom_line(  data = RKI_Omikron %>% filter( Omikronwoche >= BisWoche ), aes( y = Anzahl, colour = 'Werte (unsicher)' ), show.legend = TRUE, size = 2, linetype = 'dotted'  ) +
  geom_text(  aes(  x = Meldewoche, y = Anzahl, label = Anzahl  ) , size = 8  ) +
  # scale_y_continuous( labels = scales::percent ) +
  scale_y_continuous(  labels = function ( x ) format( x, big.mark = ".", decimal.mark= ',', scientific = FALSE  )  ) +
  expand_limits(  x = 51  ) +
  coord_cartesian(  ylim = limbounds( RKI_Omikron$Anzahl ) ) +
  theme_ta() +
  labs(   title = paste( 'Omikron R =' , round( R[2],2 ), 'CI 95 % [', round( R[1],2 ),'-', round( R[3],2 ),']' )
         , subtitle= paste( "Deutschland, Stand:", heute )
         , x = "Kalenderwoche"
         , y = "Anzahl" 
         , caption = citation
         , colour = 'Legende' ) -> p

ggsave(   filename = 'png/VoC-2.png'
         , plot = p
         , device = 'png'
         # , type = "cairo-png"
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 300  )
