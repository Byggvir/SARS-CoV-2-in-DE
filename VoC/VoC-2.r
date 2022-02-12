#!/usr/bin/env Rscript

# Regressionsanalyse

MyScriptName <- 'VoC'

library(tidyverse)
library(REST)
library(gtable)
library(lubridate)
library(ggplot2)
library(ggtext)
library(grid)
library(gridExtra)
# library(ggpubr)
library(viridis)
library(hrbrthemes)
library(scales)
library(ragg)
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

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/mytheme.r")
source("R/lib/sql.r")

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)"

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

e <- function (x, a= 0,b = 1, s = 0 ) {
  
  return (exp( a + b * ( x - s ) ) )
  
}

s <- function (y) {
  
  return ( -log(y/(1-y)))
  
}

sigmoid <- function (x, a = 0, b = 1, s = 0 ) {

  return ( 1/( 1 + exp( a + b * ( x - s ) ) ) )
  
}

daten <- read_csv( 
  'data/VoC.csv'
  , col_types = cols(
        Kw    = 'i'
      , Alpha ='i'
      , Beta  ='i'
      , Gamma ='i'
      , Delta ='i'
      , Omikron  ='i'
      , Summe = 'i'
      , Faelle = 'i'
  )
)

daten$Woche <- 0:(nrow(daten)-1)
StartWeek <- daten$Kw[1]
UntilWeek <- 60

CI <- 0.95

diagramm <- function (daten) { 
  
O_ra <- lm( s( Omikron / (Omikron + Delta) ) ~ Woche, data=daten)
O_ci <- confint( O_ra, level = CI )

O_a <- c( O_ci[1,1], O_ra$coefficients[1], O_ci[1,2] )
O_b <- c( O_ci[2,1], O_ra$coefficients[2], O_ci[2,2] )

print('Verhältnis Omikron zu Delta')
print(exp(-O_b))

D_ra <- lm( s( Delta / (Omikron + Delta) ) ~ Woche, data=daten)
D_ci <- confint(D_ra,level = CI)

D_a <- c( D_ci[1,1], D_ra$coefficients[1] , D_ci[1,2] )
D_b <- c( D_ci[2,1], D_ra$coefficients[2] , D_ci[2,2] )

print('Verhältnis Omikron zu Delta')
print(exp(D_b))

# print(summary(O_ra))

O_shaderibbon = data.table(
  x = seq(StartWeek,UntilWeek,length.out=(UntilWeek-StartWeek)*7)
)

O_shaderibbon$ylower <- sapply( O_shaderibbon$x, FUN = function(x) { sigmoid( x, a = O_a[1], b = O_b[1], s = StartWeek ) } )
O_shaderibbon$yupper <- sapply( O_shaderibbon$x, FUN = function(x) { sigmoid( x, a = O_a[3], b = O_b[3], s = StartWeek ) } )

D_shaderibbon = data.table(
  x = seq(StartWeek,UntilWeek,length.out=(UntilWeek-StartWeek)*7)
)

D_shaderibbon$ylower <- sapply( D_shaderibbon$x, FUN = function(x) { sigmoid( x, a = D_a[1], b = D_b[1], s = StartWeek ) } )
D_shaderibbon$yupper <- sapply( D_shaderibbon$x, FUN = function(x) { sigmoid( x, a = D_a[3], b = D_b[3], s = StartWeek ) } )

xticklabels <- 
  c(  paste('2021',StartWeek:52,sep= '/')
    , paste('2022',1:(UntilWeek-52),sep= '/')
    )

daten %>% ggplot(
  aes( x = StartWeek + Woche )) +
  geom_ribbon(  data = O_shaderibbon, aes( x = x, ymin = ylower, ymax = yupper ), color = 'grey',  alpha=0.1 ) +
  geom_ribbon(  data = D_shaderibbon, aes( x = x, ymin = ylower, ymax = yupper ), color = 'lightblue',  alpha=0.1 ) +
  
  geom_line(aes(y = Omikron / Summe, colour = 'Omikron'), size = 2) +
  geom_line(aes(y = Delta / Summe , colour = 'Delta'), size = 2) +
  
  geom_function( fun = sigmoid, args = list( a = O_a[1], b = O_b[1], s = StartWeek ), aes(colour = 'Omikron Untergrenze'), linetype = 'dotted', size = 1 ) +
  geom_function( fun = sigmoid, args = list( a = O_a[2], b = O_b[2], s = StartWeek ), aes(colour = 'Omikron Mittelwert'), linetype = 'dotted', size = 1 ) +
  geom_function( fun = sigmoid, args = list( a = O_a[3], b = O_b[3], s = StartWeek ), aes(colour = 'Omikron Obergrenze'), linetype = 'dotted', size = 1 ) +

  geom_function( fun = sigmoid, args = list( a = D_a[1], b = D_b[1], s = StartWeek ), aes(colour = 'Delta Untergrenze'), linetype = 'dotted', size = 1 ) +
  geom_function( fun = sigmoid, args = list( a = D_a[2], b = D_b[2], s = StartWeek ), aes(colour = 'Delta Mittelwert'), linetype = 'dotted', size = 1 ) +
  geom_function( fun = sigmoid, args = list( a = D_a[3], b = D_b[3], s = StartWeek ), aes(colour = 'Delta Obergrenze'), linetype = 'dotted', size = 1 ) +
  geom_richtext( aes( 
                  x = UntilWeek
                , y = 0.5 
                , label = paste( 'R-Wert Omikron : Delta = \n'
                              
                    , round(exp(D_b[2]),1)
                    , '['
                    , round(exp(D_b[3]),1)
                    , '-'
                    , round(exp(D_b[1]),1)
                    , ']'
                    )
                )
                , hjust = 1
                , size = 3 ) +
  scale_x_continuous(breaks = seq(StartWeek, UntilWeek, by = 1), labels = xticklabels ) +
  scale_y_continuous(labels = scales::percent) +
  # scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  expand_limits( x = 60 ) +
  theme_ta() +
  labs(  title = "Anteil Omikron und Delta an den VoC"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x = "Kalenderwoche"
         , y = "Anteil [%]" 
         , colour = 'Variante'
         , caption = '' ) -> p

 return(p) 

}

p1 <- diagramm(daten)
p2 <- diagramm(daten %>% filter(Woche > 2 ))


p <- grid.arrange(p1, p2, nrow = 1)
  
ggsave(  filename = 'png/VoC-1a.png'
         , plot = p
         , device = 'png'
         , bg = "white"
         , width = 1920 * 2
         , height = 1080 * 2
         , units = "px"
)
