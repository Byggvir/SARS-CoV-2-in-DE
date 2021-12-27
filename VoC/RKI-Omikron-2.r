#!/usr/bin/env Rscript

MyScriptName <- 'RKI-Omikron.r'

require(data.table)

library(tidyverse)
library(REST)
library(gtable)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(viridis)
library(hrbrthemes)
library(scales)
library(Cairo)
library(htmltab)
library(readODS)
library(XML)

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

WD <- paste(SD[1:(length(SD)-1)],collapse='/')

setwd(WD)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)"

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

e <- function (x, a,b) {
  
  return (exp(a+b*(x-46)))
  
}

s <- function (y) {
  
  return ( -log(y/(1-y)))
  
}

sigmoid <- function (x,a=0,b=1) {

  return ( 1/(1+exp(a+b*x)))
  
}

plotsigmoid <- function (a, b, col, xlim, ylim) {
  
  par( new=TRUE )
  
  curve(  sigmoid(x,a,b)
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

tryAsNumeric = function( node ) {
  val = XML::xmlValue( node )
  ans = as.numeric( gsub( ",", "", val ) )
  if( is.na( ans ))
    return(val)
  else
    return("ans")
}

url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/Omikron-Faelle/Omikron-Faelle.html?__blob=publicationFile"
daten = as.data.table(htmltab(url, which = 1, colNames = c("Meldewoche","Anzahl","Differenz","Quote")))

daten$Meldewoche <- as.numeric(daten$Meldewoche)
daten$OmikronWoche <- as.numeric(daten$Meldewoche - 46)
daten$Anzahl <- as.numeric(daten$Anzahl)
daten$Differenz <- as.numeric(daten$Differenz)
daten$Quote <- as.numeric(daten$Quote)

CI <- 0.95

ra <- lm(log(Anzahl) ~ OmikronWoche , data = daten %>% filter( Meldewoche < isoweek(today) - 1 ) )
ci <- confint(ra,level = CI)

a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
b <- c( ci[2,1], ra$coefficients[2] , ci[2,2])

R <- exp(b*4/7)

shaderibbon <- NULL
shaderibbon = data.table(
  x = seq(min(daten$Meldewoche),max(daten$Meldewoche),length.out=100)
)

shaderibbon$ylower <- sapply(shaderibbon$x, FUN = function(x){ e(x,a[1],b[1])})
shaderibbon$yupper <- sapply(shaderibbon$x, FUN = function(x){ e(x,a[3],b[3])})

daten %>% ggplot( aes( x = Meldewoche ) ) +
  geom_ribbon( data = shaderibbon, aes( x=x, ymin = ylower, ymax = yupper), color= 'grey', alpha=0.1) +
  geom_function( fun = e, args = list( a = a[1], b = b[1] ), aes(colour= 'Untere Grenze'), linetype = 'dotted', size = 1, show.legend = TRUE) +
  geom_function( fun = e, args = list( a = a[2], b = b[2] ), aes(colour= 'Mittelwert'), linetype = 'dotted', size = 1, show.legend = TRUE) +
  geom_function( fun = e, args = list( a = a[3], b = b[3] ), aes(colour= 'Obere Grenze'), linetype = 'dotted', size = 1 , show.legend = TRUE) +
  geom_line( data = daten %>% filter( Meldewoche < isoweek(today) - 1), aes( y = Anzahl, colour = 'Reale Werte' ), show.legend = TRUE, size = 2) +
  geom_line( data = daten %>% filter( Meldewoche >= isoweek(today) - 2), aes( y = Anzahl, colour = 'Unsichere Werte' ), show.legend = TRUE, size = 2, linetype = 'dotted' ) +
  geom_text( aes( x = Meldewoche, y = Anzahl, label = Anzahl ) , size = 8 ) +
  # scale_y_continuous(labels = scales::percent) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  expand_limits( x = 51 ) +
  coord_cartesian( ylim = c(0, 10000) ) +
  theme_ipsum() +
  theme(  plot.title = element_text( size=48 )
          , plot.subtitle = element_text( size=48 )
          , axis.text.x  = element_text ( color = 'black', size = 24 )
          , axis.title.x = element_text ( color = 'black', size = 24 )
          , axis.text.y  = element_text ( color = 'black', size = 24 )
          , axis.title.y = element_text ( color = 'black', size = 24 )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
          ) ) + 
  labs(  title = paste('Omikron R =' , round(R[2],1), 'CI 95 % [', round(R[1],1),'-', round(R[3],1),']')
         , subtitle= paste("Deutschland, Stand:", heute)
         , x = "Kalenderwoche"
         , y = "Anzahl" 
         , caption = citation
         , colour = 'Legende') -> p

ggsave(  filename = 'png/VoC-2.png'
         , plot = p
         , device = 'png'
         # , type = "cairo-png"
         , bg = "white"
         , width = 29.7 * 2
         , height = 21 * 2
         , units = "cm"
         , dpi = 300 )
