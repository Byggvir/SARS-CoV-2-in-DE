#!/usr/bin/env Rscript
#
#
# Script: Landkreise im Plus
#
# Stand: 2021-08-03
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

# Zeichnet ein Diagramm mit den Zahlen der Landkreise,
# die in einer Woche mehr Fälle als in der Vorwoche 
# oder keinen Fall gemeldet haben

options(OutDec=',')

MyScriptName <- "RKI_Landkreis.r"

require(data.table)
library(REST)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(tidyverse)

# Set Working directory to git root

if (rstudioapi::isAvailable()){
  
  # When executed in RStudio
  SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
  
} else {
  
  #  When executi on command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-1)],collapse='/')

setwd(WD)

source("R/lib/copyright.r")
source("R/lib/sql.r")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")
To_Kw <- isoweek(today) + 53

LandkreisePlus <- function (From_Kw = 0, To_Kw = 53) {

data <- RunSQL(paste('call Landkreise(', From_Kw , ',', To_Kw, ');' )) 

print(data)
labs <- data$Kw
j20 <- data$Kw < 54
j21 <- data$Kw > 53

labs[labs>53] <- labs[j21] - 53
labs[j20] <- paste(labs[j20],20,sep='/')
labs[j21] <- paste(labs[j21],21,sep='/')


p <- ggplot(data, aes( x = sKw)) +
  geom_line(aes(y = GT, colour="> Fälle"), show.legend = TRUE) +
#  geom_line(aes(y = LEQ, colour="<= Fälle"), show.legend = TRUE) +
  geom_line(aes(y = Zero, colour="Keine Fälle") ,show.legend = TRUE) +
  scale_colour_manual(""
                      , breaks = c("Keine Fälle", "<= Fälle", "> Fälle")
                      , values=c("darkblue","darkgreen","darkred")) +
  scale_x_continuous("Kalenderwoche"
                   , breaks = c(20,40,60,80)
                   , labels = c("20/20","40/20","7/21","27/21" )) +
  
  geom_text(aes(label=GT,y=GT), angle = 0, vjust=0, hjust=1, color="red", size=2) +
#  geom_text(aes(label=LEQ,y=LEQ), angle = 0, vjust=1, hjust=1, color="blue", size=2) +
  geom_text(aes(label=Zero,y=Zero), angle = 0, vjust=1, hjust=0, color="blue", size=2) +
  ggtitle( paste('Corona: Zahl der Landkreise ohne oder mit mehr Fällen als in der Vorwoche' )) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position="bottom") +
  xlab("Kalenderwoche") +
  ylab("Fallzahlen pro Kw")

gg <- grid.arrange(p, ncol=1)

plot(gg)

ggsave(plot = gg, file = paste('png/Landkreise-', From_Kw,"-",To_Kw,".png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.1, height = 21, units = "cm", dpi = 150)
}

# Main program

kw <- isoweek(Sys.Date()) + 52
  
LandkreisePlus(9,kw)
