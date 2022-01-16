#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"LK_Altersgruppen"

library(tidyverse)
library(REST)
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

WD <- paste(SD[1:(length(SD)-2)],collapse='/')

setwd(WD)

fPrefix <- "Ausprobieren_"

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

args = commandArgs(trailingOnly=TRUE)

if (length(args) == 0) {
  
  untilday <- Sys.Date() - 4

} else if (length(args) >= 1) {
  untilday <- as.Date(args[1])
  
}

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021)\nGitHub SARS-CoV-2 Infektionen"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )
# untilday <- as.Date("2021-11-27")
z1 <- format(untilday, "%Y-%m-%d")
z2 <- format(untilday-6, "%Y-%m-%d")
z3 <- format(untilday-7, "%Y-%m-%d")
z4 <- format(untilday-13, "%Y-%m-%d")

SQL <- paste(
  'call Bundesland714("',untilday,'");'
  , sep = ' ')
Bundesland <- RunSQL(SQL)

SQL <- paste(
    'call Altersgruppe714("',untilday,'");'
  , sep = ' ')

Altersgruppen <- RunSQL(SQL = SQL)

for ( B in Bundesland[,2]) {

M <- Altersgruppen %>% filter(Bundesland == B)
xylimit <- limbounds(c(M$Vorwoche/M$EW_insgesamt ,M$Woche/M$EW_insgesamt)) * 100000
  
Altersgruppen %>% filter(Bundesland == B) %>% ggplot() +
#  stat_ellipse(aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000), color= 'darkgrey', type = "t", geom = "polygon", alpha = 0.1 ) +
  geom_abline( intercept = 0, slope = 1, color ='red' ) +
  geom_abline( intercept = 0, slope = 0.75, color ='green' ) +
  geom_point( aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000, colour = Altersgruppe ), size = 6, alpha = 0.8) +
  geom_point( data = Bundesland %>% filter(Bundesland == B)
                , aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000)
                , shape = 3, stroke = 4, fill = "blue", color = "darkblue", alpha = 1, size = 5 ) +
  geom_text_repel( aes( x = Vorwoche / EW_insgesamt * 100000, y = Woche / EW_insgesamt * 100000, label = Altersgruppe), vjust=0, hjust=1) + 
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  coord_fixed ( xlim = xylimit, ylim = xylimit ) +
  scale_fill_viridis(discrete = TRUE) +
#  facet_wrap(vars(Bundesland)) +
  expand_limits( x = 0 , y = 0 ) +
  theme_ipsum() +
  theme(  plot.title = element_text( size = 24 )
          , legend.position="right"
         , strip.text.x = element_text (
          size = 12
          , color = "black"
          , face = "bold.italic"
        ) ) +
  labs(  title= paste('Altersgruppen in' ,B, 'bis Meldedatum', untilday)
       , subtitle = "Änderung Fallzahlen pro 100.000 Einwohner"
       , x = paste( 'Fälle von', z4, 'bis', z3, 'pro 100.000' )
       , y = paste( 'Fälle von', z2, 'bis', z1, 'pro 100.000' )
       , caption = citation )

ggsave(  paste('png/LK/Altersgruppen-Tendenz-',B,'.png', sep='')
       , device = 'png'
       , bg = "white"
       , width = 1920 * 2
       , height = 1080 * 2
       , units = "px"
       )
}
