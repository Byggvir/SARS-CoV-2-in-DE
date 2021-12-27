#!/usr/bin/env Rscript

# Regressionsanalyse

MyScriptName <- 'VoC'
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

daten$Kw <- daten$Kw

CI <- 0.95

O_ra <- lm(s( Omikron / (Omikron + Delta) ) ~ Kw, data=daten)
O_ci <- confint(O_ra,level = CI)

O_a <- c( O_ci[1,1], O_ra$coefficients[1] , O_ci[1,2])
O_b <- c( O_ci[2,1], O_ra$coefficients[2] , O_ci[2,2])

D_ra <- lm(s( Delta / (Omikron + Delta) ) ~ Kw, data=daten)
D_ci <- confint(D_ra,level = CI)

D_a <- c( D_ci[1,1], D_ra$coefficients[1] , D_ci[1,2])
D_b <- c( D_ci[2,1], D_ra$coefficients[2] , D_ci[2,2])

print(summary(O_ra))
shaderibbon = data.table(
  x = seq(0,50,length.out=100)
)
shaderibbon$ylower <- sapply(shaderibbon$x, FUN = function(x){ sigmoid(x,O_a[1],O_b[1])})
shaderibbon$yupper <- sapply(shaderibbon$x, FUN = function(x){ sigmoid(x,O_a[3],O_b[3])})

daten %>% ggplot(
  aes( x = Kw )) +
  geom_line(aes(y = Omikron / (Omikron + Delta) ), colour = 'black', size = 2) +
  # geom_line(aes(y = Omikron / (Omikron + Delta) ), color = 'darkgrey', size = 2) +
  
  #  geom_function( fun = sigmoid, args = list( a = a[1], b = b[1] ), color ='yellow', linetype = 'dotted', size = 1 ) +
  geom_function( fun = sigmoid, args = list( a = O_a[2], b = O_b[2] ), color ='blue', linetype = 'dotted', size = 1 ) +
  #  geom_function( fun = sigmoid, args = list( a = a[3], b = b[3] ), color ='red', linetype = 'dotted', size = 1  ) +
  
  # geom_function( fun = sigmoid, args = list( a = D_a[1], b = D_b[1] ), color ='yellow', linetype = 'dotted', size = 1 ) +
  # geom_function( fun = sigmoid, args = list( a = D_a[2], b = D_b[2] ), color ='blue', linetype = 'dotted', size = 1 ) +
  # geom_function( fun = sigmoid, args = list( a = D_a[3], b = D_b[3] ), color ='red', linetype = 'dotted', size = 1 ) +

  scale_y_continuous(labels = scales::percent) +
  # scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  expand_limits( x = 60 ) +
  theme_ipsum() +
  theme(  plot.title = element_text( size=48 )
          , axis.text.x  = element_text ( color = 'black', size = 24 )
          , axis.title.x = element_text ( color ='black', size = 24 )
          , axis.text.y  = element_text ( color = 'black', size = 24 )
          , axis.title.y = element_text ( color ='black', size = 24 )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
          ) ) + 
  labs(  title = "Anteil Omikron an den VoC"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x = "Kalenderwoche"
         , y = "Anteil [%]" 
         , caption = '' ) -> p1

ra <- lm(log(Omikron/Summe*Faelle ) ~ Kw, data=daten)
ci <- confint(ra,level = CI)

a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
b <- c( ci[2,1], ra$coefficients[2] , ci[2,2])

R <- exp(b*4/7)

shaderibbon <- NULL
shaderibbon = data.table(
  x = seq(min(daten$Kw),max(daten$Kw),length.out=100)
)
shaderibbon$ylower <- sapply(shaderibbon$x, FUN = function(x){ e(x,a[1],b[1])})
shaderibbon$yupper <- sapply(shaderibbon$x, FUN = function(x){ e(x,a[3],b[3])})


daten %>% ggplot(
  aes( x = Kw )) +
  # geom_ribbon( data = shaderibbon, aes( x=x, ymin = ylower, ymax = yupper), color= 'grey', alpha=0.1) +
  geom_line(aes(y = Omikron / Summe * Faelle, colour = 'Reale Werte' ), show.legend = TRUE, size = 2) +
  #geom_function( fun = e, args = list( a = a[2], b = b[1] ), aes(colour= 'Untere Grenze'), linetype = 'dotted', size = 1, show.legend = TRUE) +
  geom_function( fun = e, args = list( a = a[2], b = b[2] ), aes(colour= 'Mittelwert'), linetype = 'dotted', size = 1, show.legend = TRUE) +
  #geom_function( fun = e, args = list( a = a[2], b = b[3] ), aes(colour= 'Obere Grenze'), linetype = 'dotted', size = 1 , show.legend = TRUE) +
  # scale_y_continuous(labels = scales::percent) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  expand_limits( x = 51 ) +
  coord_cartesian( ylim = c(0, 10000) ) +
  theme_ipsum() +
  theme(  plot.title = element_text( size=48 )
          , axis.text.x  = element_text ( color = 'black', size = 24 )
          , axis.title.x = element_text ( color = 'black', size = 24 )
          , axis.text.y  = element_text ( color = 'black', size = 24 )
          , axis.title.y = element_text ( color = 'black', size = 24 )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
          ) ) + 
  labs(  title = 'Geschätzte Omikron Fälle'
         , subtitle= paste('R =' , round(R[2],1), 'CI 95 % [', round(R[1],1),'-', round(R[3],1), ']\nDeutschland, Stand:', heute )
         , x = "Kalenderwoche"
         , y = "Anzahl" 
         , caption = citation
         , colour = 'Legende') -> p2

p <- grid.arrange(p1, p2, ncol = 1, nrow = 2)

ggsave(  filename = 'png/VoC.png'
         , plot = p
         , device = 'png'
         , type = "cairo-png"
         , bg = "white"
         , width = 29.7 * 2
         , height = 21 * 2
         , units = "cm"
         , dpi = 300 )
