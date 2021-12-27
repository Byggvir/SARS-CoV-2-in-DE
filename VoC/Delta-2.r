#!/usr/bin/env Rscript

# Regressionsanalyse

MyScriptName <- 'VoC-DK'

library(tidyverse)
library(REST)
library(gtable)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
# library(ggpubr)
library(viridis)
library(hrbrthemes)
library(scales)
library(Cairo)
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

fPrefix <- "Fallzahlen_Wo_"

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: ssi.dk (2021)"

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

e <- function (x, a,b) {
  
  return (exp(a+b*x))
  
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


# Define theme 

tt <- function() {
theme_ipsum() +
  theme(  plot.title = element_text( size = 24 )
          , plot.subtitle = element_text( size = 18 )
  
          , legend.position = 'bottom'          
          , axis.text.x  = element_text ( color = 'black', size = 12 )
          , axis.title.x = element_text ( color ='black', size = 12 )
          , axis.text.y  = element_text ( color = 'black', size = 12 )
          , axis.title.y = element_text ( color ='black', size = 12 )
          , strip.text.x = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic"
          ) )
}

daten <- read_csv( 
  'data/DK.csv'
  , col_types = cols(
    Datum  = 'D'
    , Tag = 'i'
    , Summe ='i'
    , Omikron  ='i'
    
  )
)

daten %>% filter(Omikron>0) -> daten
daten$Datum <- as.Date(daten$Datum)

CI <- 0.95

O_ra <- lm(s( Omikron / Summe ) ~ Tag, data=daten)
O_ci <- confint(O_ra,level = CI)

O_a <- c( O_ci[1,1], O_ra$coefficients[1] , O_ci[1,2])
O_b <- c( O_ci[2,1], O_ra$coefficients[2] , O_ci[2,2])

print(summary(O_ra))

shaderibbon = data.table(
  x = seq(0,50,length.out=100)
)
shaderibbon$ylower <- sapply(shaderibbon$x, FUN = function(x){ sigmoid(x,O_a[1],O_b[1])})
shaderibbon$yupper <- sapply(shaderibbon$x, FUN = function(x){ sigmoid(x,O_a[3],O_b[3])})



daten %>% ggplot(
  aes( x = Tag )) +
  geom_ribbon( data = shaderibbon, aes(x=x, ymin = ylower,ymax = yupper), color= 'grey', alpha=0.1) +
  geom_line(aes(y = Omikron / Summe, colour = 'Reale Werte' ), show.legend = TRUE, size = 2) +
  geom_function( fun = sigmoid, args = list( a = O_a[1], b = O_b[1] ), aes( colour = 'Untere Grenze'), show.legend = TRUE, linetype = 'dotted', size = 1 ) +
  geom_function( fun = sigmoid, args = list( a = O_a[2], b = O_b[2] ), aes( colour = 'Mittelwert'), show.legend = TRUE, linetype = 'dotted', size = 1 ) +
  geom_function( fun = sigmoid, args = list( a = O_a[3], b = O_b[3] ), aes( colour = 'Obere Grenze'), show.legend = TRUE, linetype = 'dotted', size = 1  ) +
  scale_y_continuous(labels = scales::percent) +
  # scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  expand_limits( x = 50 ) +
  tt() +
  labs(  title = "Dänemark: Anteil Omikron an den VoC"
         , subtitle= paste('Stand:', heute, 'Zeitraum:', daten$Datum[1], 'bis', daten$Datum[nrow(daten)]) 
         , x = "Tag"
         , y = "Anteil [%]" 
         , caption = '' 
         , colour = 'Legende' ) -> p1

ra <- lm(log(Omikron ) ~ Tag, data=daten)
ci <- confint(ra,level = CI)

a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
b <- c( ci[2,1], ra$coefficients[2] , ci[2,2])

R <- exp(b*4)

shaderibbon <- NULL
shaderibbon = data.table(
  x = seq(0,max(daten$Tag),length.out=100)
)
shaderibbon$ylower <- sapply(shaderibbon$x, FUN = function(x){ e(x,a[1],b[1])})
shaderibbon$yupper <- sapply(shaderibbon$x, FUN = function(x){ e(x,a[3],b[3])})


daten %>% ggplot(
 aes( x = Tag )) +
   geom_ribbon( data = shaderibbon, aes( x=x, ymin = ylower, ymax = yupper), color= 'grey', alpha=0.1) +
   geom_line(aes(y = Omikron, colour = 'Reale Werte' ), show.legend = TRUE, size = 2) +
   geom_function( fun = e, args = list( a = a[1],b = b[1] ), aes(colour= 'Untere Grenze'), linetype = 'dotted', size = 1, show.legend = TRUE) +
   geom_function( fun = e, args = list( a = a[2],b = b[2] ), aes(colour= 'Mittelwert'), linetype = 'dotted', size = 1, show.legend = TRUE) +
   geom_function( fun = e, args = list( a = a[3],b = b[3] ), aes(colour= 'Obere Grenze'), linetype = 'dotted', size = 1 , show.legend = TRUE) +
  
   # scale_y_continuous(labels = scales::percent) +
   scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
   tt() +
   labs(  title = paste('Dänemark: Omikron R =' , round(R[2],1), 'CI 95 % [', round(R[1],1),'-', round(R[3],1),']')
          , subtitle= paste('Stand:', heute, 'Zeitraum:', daten$Datum[1], 'bis', daten$Datum[nrow(daten)]) 
          , x = "Tag"
          , y = "Anzahl"
          , caption = citation 
          , colour = 'Legende'
          ) -> p2


p <- grid.arrange( p1, p2, ncol = 2, nrow = 1 )

ggsave(  filename = 'png/VoC-DK.png'
         , plot = p
         , device = 'png'
         , type = "cairo-png"
         , bg = "white"
         , width = 29.7 * 2
         , height = 21 * 2
         , units = "cm"
         , dpi = 300 )

