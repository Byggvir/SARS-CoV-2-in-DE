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
  
  return ( -log( y / ( 1 - y ) ) )
  
}

sigmoid <- function ( x, a = 0, b = 1, N = 1 ) {

  return (  N / ( 1 + exp( a+b*x ) ) )
  
}

sigmoid1s <- function ( x, a = 0, b = 1 , N = 1 ) {
  
  return (  - N * b * exp( a + b * x ) / ( 1 + exp( a + b * x ) ) ^ 2 )
  
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
RKI_Omikron = as.data.table( htmltab( url, which = 1, bodyFun = bFun, colNames = c( "Meldewoche","Anzahl","Differenz","Inzidenz" ) ) )
RKI_OmikronAlter = as.data.table( htmltab( url, which = 2, bodyFun = bFun, colNames = c( "Art","A00-A04","A05-A14","A15-A34","A35-A59","A60-A79","A80+","Gesamt" ) ) )

write.csv(RKI_Omikron,file = paste( 'data/Omikron_', format( today, "%Y%m%d" ), '.csv',sep=''))
write.csv(RKI_OmikronAlter,file = paste( 'data/Omikron_Alter_', format( today, "%Y%m%d" ), '.csv',sep=''))

RKI_Omikron$Omikronwoche <- 1:nrow(RKI_Omikron) - 1
RKI_Omikron$Anzahl <- as.numeric( RKI_Omikron$Anzahl )
RKI_Omikron$Differenz <- as.numeric( RKI_Omikron$Differenz )
RKI_Omikron$Inzidenz <- as.numeric( RKI_Omikron$Inzidenz )
RKI_Omikron$KumAnzahl <- cumsum(RKI_Omikron$Anzahl )

print( RKI_Omikron )

Anfang <- 46
BisWoche <- max(RKI_Omikron$Omikronwoche) - 1

xticklabels <- 
  c(  paste('2021',Anfang:52,sep= '/')
      , paste('2022',1:(nrow(RKI_Omikron) - length(Anfang:52)+1),sep= '/')
  )

CI <- 0.95

N <- 82300000

ra1 <- lm( log( Anzahl ) ~ Omikronwoche , data = RKI_Omikron %>% filter( Omikronwoche <= BisWoche )  )
ci1 <- confint( ra1, level = CI )

# print(summary(ra))

a1 <- c(  ci1[1,1], ra1$coefficients[1] , ci1[1,2] )
b1 <- c(  ci1[2,1], ra1$coefficients[2] , ci1[2,2] )

R1 <- exp( b1*4/7 )

ra2 <- lm( log( KumAnzahl) ~ Omikronwoche , data = RKI_Omikron %>% filter( Omikronwoche <= BisWoche )  )
ci2 <- confint( ra2,level = CI )

a2 <- c(  ci2[1,1], ra2$coefficients[1] , ci2[1,2] )
b2 <- c(  ci2[2,1], ra2$coefficients[2] , ci2[2,2] )

R2 <- exp( b2*4/7 )

ra3 <- lm( s( KumAnzahl / N ) ~ Omikronwoche , data = RKI_Omikron %>% filter( Omikronwoche <= BisWoche )  )
ci3 <- confint( ra3,level = CI )

a3 <- c(  ci3[1,1], ra3$coefficients[1] , ci3[1,2] )
b3 <- c(  ci3[2,1], ra3$coefficients[2] , ci3[2,2] )

print(c(R1,R2))

shaderibbon <- NULL
shaderibbon = data.table( 
  x = seq( min( RKI_Omikron$Omikronwoche ),max( RKI_Omikron$Omikronwoche ),length.out=100 )
 )

shaderibbon$ylower <- sapply( shaderibbon$x, FUN = function( x ){ e( x,a1[1],b1[1], xoffset = 0 ) } )
shaderibbon$yupper <- sapply( shaderibbon$x, FUN = function( x ){ e( x,a1[3],b1[3], xoffset = 0 ) } )

RKI_Omikron %>% ggplot(  aes(  x = Omikronwoche  )  ) +
  geom_ribbon(  data = shaderibbon, aes( x = x, ymin = ylower, ymax = yupper ), color = 'grey',  alpha=0.1 ) +
  geom_function(  fun = e, args = list( a = a1[1], b = b1[1], xoffset = 0 ), aes( colour= 'Untere Grenze' ), show.legend = TRUE, linetype = 'dotted', size = 1.5 ) +
  geom_function(  fun = e, args = list( a = a1[2], b = b1[2], xoffset = 0 ), aes( colour= 'Mittelwert' ), show.legend = TRUE, linetype = 'dotted', size = 1.5 ) +
  geom_function(  fun = e, args = list( a = a1[3], b = b1[3], xoffset = 0 ), aes( colour= 'Obere Grenze' ), show.legend = TRUE, linetype = 'dotted', size = 1.5  ) +
  geom_line(  data = RKI_Omikron %>% filter( Omikronwoche <= BisWoche ), aes( y = Anzahl, colour = 'Werte' ), show.legend = TRUE, size = 2 ) +
  geom_line(  data = RKI_Omikron %>% filter( Omikronwoche >= BisWoche ), aes( y = Anzahl, colour = 'Werte (unsicher)' ), show.legend = TRUE, size = 2, linetype = 'dashed'  ) +
  geom_text(  aes(  x = Omikronwoche, y = Anzahl, label = Anzahl  ) , size = 8  ) +
  scale_x_continuous( breaks = RKI_Omikron$Omikronwoche, labels = RKI_Omikron$Meldewoche ) +
  scale_y_continuous( labels = function ( x ) format( x, big.mark = ".", decimal.mark= ',', scientific = FALSE  )  ) +
  coord_cartesian(  ylim = limbounds( RKI_Omikron$Anzahl ) ) +
  theme_ta() +
  labs(   title = paste( 'Omikron R =' , round( R1[2],2 ), 'CI 95 % [', round( R1[1],2 ),'-', round( R1[3],2 ),']' )
          , subtitle= paste( "Wöchentliche Fälle Deutschland, Stand:", heute )
          , x = "Kalenderwoche"
          , y = "Anzahl" 
          , caption = citation
          , colour = 'Legende' ) -> p

ggsave(   filename = 'png/VoC-2a.png'
          , plot = p
          , device = 'png'
          # , type = "cairo-png"
          , bg = "white"
          , width = 29.7
          , height = 21
          , units = "cm"
          , dpi = 300  )

shaderibbon$ylower <- sapply( shaderibbon$x, FUN = function( x ){ e( x, a2[1], b2[1], xoffset = 0 ) } )
shaderibbon$yupper <- sapply( shaderibbon$x, FUN = function( x ){ e( x, a2[3], b2[3], xoffset = 0 ) } )

RKI_Omikron %>% ggplot(  aes(  x = Omikronwoche  )  ) +
  geom_ribbon(  data = shaderibbon, aes( x = x, ymin = ylower, ymax = yupper ), color = 'grey',  alpha=0.1 ) +
  geom_function(  fun = e, args = list( a = a2[1], b = b2[1], xoffset = 0 ), aes( colour= 'Untergrenze' ), show.legend = TRUE, linetype = 'dotted', size = 1.5 ) +
  geom_function(  fun = e, args = list( a = a2[2], b = b2[2], xoffset = 0 ), aes( colour= 'Mittelwert' ), show.legend = TRUE, linetype = 'dotted', size = 1.5 ) +
  geom_function(  fun = e, args = list( a = a2[3], b = b2[3], xoffset = 0 ), aes( colour= 'Obergrenze' ), show.legend = TRUE, linetype = 'dotted', size = 1.5  ) +
  geom_function(  fun = sigmoid, args = list( a = a3[1], b = b3[1], N = N ), aes( colour= 'Sigmoid Untergrenze' ), show.legend = TRUE, linetype = 'dashed', size = 1 ) +
  geom_function(  fun = sigmoid, args = list( a = a3[2], b = b3[2], N = N ), aes( colour= 'Sigmoid Mittelwert' ), show.legend = TRUE, linetype = 'dashed', size = 1 ) +
  geom_function(  fun = sigmoid, args = list( a = a3[3], b = b3[3], N = N ), aes( colour= 'Sigmoid Obergrenze' ), show.legend = TRUE, linetype = 'dashed', size = 1 ) +
  geom_line(  data = RKI_Omikron %>% filter( Omikronwoche <= BisWoche ), aes( y = KumAnzahl, colour = 'Werte' ), show.legend = TRUE, size = 2 ) +
  geom_line(  data = RKI_Omikron %>% filter( Omikronwoche >= BisWoche ), aes( y = KumAnzahl, colour = 'Werte (unsicher)' ), show.legend = TRUE, size = 2, linetype = 'dashed'  ) +
  geom_text(  aes(  x = Omikronwoche, y = KumAnzahl, label = KumAnzahl  ) , size = 8  ) +
  scale_x_continuous( breaks = RKI_Omikron$Omikronwoche, labels = RKI_Omikron$Meldewoche ) +
  scale_y_continuous( labels = function ( x ) format( x, big.mark = ".", decimal.mark= ',', scientific = FALSE  )  ) +
  coord_cartesian(  ylim = limbounds( RKI_Omikron$KumAnzahl ) ) +
  theme_ta() +
  labs(   title = paste( 'Omikron R =' , round( R2[2],2 ), 'CI 95 % [', round( R2[1],2 ),'-', round( R2[3],2 ),']' )
         , subtitle= paste( "Kumulative Fälle Deutschland, Stand:", heute )
         , x = "Kalenderwoche"
         , y = "Anzahl" 
         , caption = citation
         , colour = 'Legende' ) -> p

ggsave(   filename = 'png/VoC-2b.png'
         , plot = p
         , device = 'png'
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 300  )

