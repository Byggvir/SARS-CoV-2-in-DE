#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# last Change: 2021-06-25
#
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <- "RegZeitreihe"

library(tidyverse)
library(REST)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(ggtext)
library(ggrepel)
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

options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

set.seed(42)

CI = 0.95
N <- 100

Zeitreihe <- data.table(
  t = seq(0,N, by = 1)
  , x = rnorm(N+1,0,0.1)
  , y = rnorm(N+1,0,0.2)
  
)

plot_zeitreihen <- function ( data, intercept = c(0,0), slope= c(0,0) ) {

if ( identical(slope,c(0,0)) ) {
  
  Tendenz <- 'ohne'

} else {
  
  Tendenz <- 'mit'

}
  
  
  
ZR <- data.table (
  t = data$t
  , x = intercept[1] + slope[1] * data$t +data$x 
  , y = intercept[2] + slope[2] * data$t + data$y
)

ra <- lm (data = ZR, formula = y ~ x)

ci <- confint(ra,level = CI)
a <- c( ci[1,1], ra$coefficients[1] , ci[1,2] )
b <- c( ci[2,1], ra$coefficients[2] , ci[2,2] )

data %>% ggplot() + 
  geom_line( aes( x = t, y = x + slope[1] * t + intercept[1] , colour = 'f(t)') ) +
  geom_line( aes( x = t, y = y + slope[2] * t + intercept[2] , colour = 'g(t)' ) ) +
  geom_smooth( aes( x = t, y = x + slope[1] * t + intercept[1] , colour = 'f(t)' ), method = 'lm' ) +
  geom_smooth( aes( x = t, y = y + slope[2] * t + intercept[2] , colour = 'g(t)' ), method = 'lm' ) +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ta() +
  theme(  plot.title = element_text( size = 24 )
          , legend.position="right"
          , axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          , strip.text.x = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic"
          ) ) +
  labs(  title = paste( 'Zeitreihen', Tendenz, 'Tendenz' )
         , subtitle = paste( 'f(t) =', intercept[1], '+',slope[1] , '* t + rnorm()',
                             '\ng(t) =', intercept[2], '+',slope[2] , '* t + rnorm()' )
         , x = "t"
         , y = "y"
         , colour = 'Zeitreihe' 
  ) -> P1

data %>% ggplot() + 
  geom_point(  aes( x = x + slope[1] * t + intercept[1], y = y + slope[2] * t + intercept[2]) ) +
  geom_smooth( aes( x = x + slope[1] * t + intercept[1], y = y + slope[2] * t + intercept[2]), method = 'lm' ) +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ta() +
  theme(  plot.title = element_text( size = 24 )
          , legend.position="right"
          , axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          , strip.text.x = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic"
          ) ) +
  labs(  title = paste( 'Scatterplot g(t) ~ f(t) ' )
         , subtitle = paste(
              'intercept =', round(a[2],3)
            , '; slope =', round(b[2],2)
            , '\nR² =', round(summary(ra)$r.squared,4)
            , ', P-Value =', format(get_p_value(ra),nsmall = 4, scientific = 6)
         )                
         , x = "f(t)"
         , y = "g(t)"
         ) -> P2

P <- grid.arrange( P1, P2, nrow = 1)

ggsave(  paste( 
  file = 'png/', MyScriptName, slope[1], "-",  slope[2], '.png', sep='')
  , plot = P
  , device = 'png'
  , bg = "white"
  , width = 1920 * 2
  , height = 1080 * 2
  , units = "px"
)

}


plot_zeitreihen( data = Zeitreihe, intercept = c(3,2) )
plot_zeitreihen( data = Zeitreihe, intercept = c(3,2), slope = c( 0.01, 0.02))
plot_zeitreihen( data = Zeitreihe, intercept = c(3,2), slope = c( -0.01, 0.02))
plot_zeitreihen( data = Zeitreihe, intercept = c(3,2), slope = c( 0.1, 0.2))
