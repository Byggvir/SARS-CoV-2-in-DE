#!/usr/bin/env Rscript

# Regressionsanalyse

MyScriptName <- 'VoC'
library(tidyverse)
library(REST)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
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

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

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

f <- function (y) {
  
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
  )
)

daten$Kw <- daten$Kw -daten$Kw[1]
CI <- 0.95

ra <- lm(log(Omikron) ~ Kw, data=daten)
ci <- confint(ra,level = CI)

a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
b <- c( ci[2,1], ra$coefficients[2] , ci[2,2])

print(a)
print(b)
print(exp(b*4/7))

daten %>% ggplot(
  aes( x = Kw )) +
  geom_line(aes(y = Omikron )) +
  geom_function(fun = e, args = list(a=a[2],b=b[2]),color='blue') +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  theme_ipsum() +
  theme(  plot.title = element_text( size=48 )
          , axis.text.y  = element_text ( color = 'black', size = 24)
          , axis.title.y = element_text ( color='black', size = 24)
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
          ) ) + 
  labs(  title = "VoC: Omikron ~ Andere"
         , subtitle= paste("Deutschland, Stand:", heute)
         , x = "Kalenderwoche"
         , y = "Anteil [%]" 
         , caption = citation )

ggsave(  'png/VoC.png'
         , device = 'png'
         , type = "cairo-png"
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 300 )
