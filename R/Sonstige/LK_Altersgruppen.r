#!/usr/bin/env Rscript
#
#
# Script: LK_Altersgruppen.r
#
# Stand: 2022-03-01
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"LK_Altersgruppen"

library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(viridis)
library(hrbrthemes)
library(scales)
library(ragg)

# Set Working directory to git root

if (rstudioapi::isAvailable()){
  
  # When executed in RStudio
  SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
  
} else {
  
  #  When executing on command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-2)],collapse='/')

setwd(WD)

fPrefix <- "Ausprobieren_"

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/mytheme.r")
source("R/lib/sql.r")

args = commandArgs(trailingOnly=TRUE)

if (length(args) == 0) {
  
  UntilDay <- Sys.Date() - 4

} else if (length(args) >= 1) {
  UntilDay <- as.Date(args[1])
  
}

FromDay <- as.Date("2022-01-18")

citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nGitHub SARS-CoV-2 Infektionen"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

outdir <- 'png/Altersgruppen/'
dir.create( outdir , showWarnings = FALSE, recursive = FALSE, mode = "0777")

maxlimit <- rep(0,each=17)

for ( d in UntilDay:UntilDay ) {
  
u <- as.Date(d,"1970-01-01")
z1 <- format(u, "%Y-%m-%d")
z2 <- format(u-6, "%Y-%m-%d")
z3 <- format(u-7, "%Y-%m-%d")
z4 <- format(u-13, "%Y-%m-%d")

print(u)

SQL <- paste(
  'call Bundesland714("',u,'");'
  , sep = ' ')
BL <- RunSQL(SQL)

SQL <- paste(
    'call Altersgruppe714("',u,'");'
  , sep = ' ')

Altersgruppen <- RunSQL(SQL = SQL)

for ( i in 1:17 ) {

M <- Altersgruppen %>% filter( IdBundesland == BL[i,1] )

xylimit <- limbounds( c( M$Vorwoche / M$EW_insgesamt * 100000, M$Woche / M$EW_insgesamt * 100000 ) )
xylimit[2] <- max( c( xylimit[2] , maxlimit[i] ) )

print(maxlimit[i])

maxlimit[i] <- xylimit[2]

print(xylimit)

Altersgruppen %>% filter(IdBundesland == BL[i,1]) %>% ggplot() +
#  stat_ellipse(aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000), color= 'darkgrey', type = "t", geom = "polygon", alpha = 0.1 ) +
  geom_abline( intercept = 0, slope = 1.5, color = 'red' ) +
  geom_abline( intercept = 0, slope = 1, color = 'yellow' ) +
  geom_abline( intercept = 0, slope = 0.75, color = 'green' ) +
  geom_point( aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000, colour = Altersgruppe ), size = 6, alpha = 0.8) +
  geom_point( data = BL %>% filter( IdBundesland == BL[i,1])
                , aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000)
                , shape = 3, stroke = 4, fill = "blue", color = "darkblue", alpha = 1, size = 5 ) +
  geom_text_repel( aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000, label = Altersgruppe), vjust=0, hjust=1) + 
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  expand_limits( x = 0 , y = 0 ) +
  expand_limits( x = xylimit[2] , y = xylimit[2] ) +
  coord_fixed ( xlim = xylimit, ylim = xylimit, expand = TRUE, clip = 'on' ) +
  scale_fill_viridis(discrete = TRUE) +
#  facet_wrap(vars(Bundesland)) +
  
  theme_ipsum() +
  theme(  plot.title = element_text( size = 24 )
          , legend.position="right"
         , strip.text.x = element_text (
          size = 12
          , color = "black"
          , face = "bold.italic"
        ) ) +
  labs(  title= paste('Altersgruppen in' , BL[i,2], 'bis Meldedatum', u)
       , subtitle = "Änderung Fallzahlen pro 100.000 Einwohner"
       , x = paste( 'Fälle von', z4, 'bis', z3, 'pro 100.000' )
       , y = paste( 'Fälle von', z2, 'bis', z1, 'pro 100.000' )
       , caption = citation )

ggsave(  paste( outdir, BL[i,2],'-',u,'.png', sep='')
       , device = 'png'
       , bg = "white"
       , width = 1920 * 2
       , height = 1080 * 2
       , units = "px"
       )
}
}
