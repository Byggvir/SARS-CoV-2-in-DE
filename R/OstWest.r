#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2021-12-10
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#
MyScriptName <-"OstWest"

library(tidyverse)
#library(REST)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
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
  
  #  When executi on command line 
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

citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

BL <- c( "West", "Ost")
Bev <- RunSQL(SQL = "select BLWestEast(IdBundesland) as Bundesland,sum(EW_Insgesamt) as Anzahl from Bundesland group by BLWestEast(IdBundesland);")

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL = paste ('select * from CasesPerWeekWE;', sep ="")

weekly <- RunSQL(SQL = SQL)

m <- length(weekly[1,])
reported <- weekly$Kw[m]

weekly$AnzahlFall[weekly$Bundesland=='Ost'] <- weekly$AnzahlFall[weekly$Bundesland=='Ost'] / Bev[1,2] * 100000
weekly$AnzahlFall[weekly$Bundesland=='West'] <- weekly$AnzahlFall[weekly$Bundesland=='West'] / Bev[2,2] * 100000

lo <- length(weekly$AnzahlFall[weekly$Bundesland=='Ost'])
lw <- length(weekly$AnzahlFall[weekly$Bundesland=='West'])

# weekly$AnzahlFall[weekly$Bundesland=='Bund'] <- weekly$AnzahlFall[weekly$Bundesland=='Bund'] / (Bev[1,2]+Bev[2,2]) * 100000

weekly$AnzahlTodesfall[weekly$Bundesland=='Ost'] <- weekly$AnzahlTodesfall[weekly$Bundesland=='Ost'] / Bev[1,2] * 100000
weekly$AnzahlTodesfall[weekly$Bundesland=='West'] <- weekly$AnzahlTodesfall[weekly$Bundesland=='West'] / Bev[2,2] * 100000
# weekly$AnzahlTodesfall[weekly$Bundesland=='Bund'] <- weekly$AnzahlTodesfall[weekly$Bundesland=='Bund'] / (Bev[1,2]+Bev[2,2]) * 100000

wc <- c(  weekly$AnzahlFall[weekly$Bundesland=='Ost'][lo-1]
        , weekly$AnzahlFall[weekly$Bundesland=='West'][lw-1] )

p1 <- ggplot(weekly, aes(fill=Bundesland, y=AnzahlFall, x=Kw)) +
  geom_bar(position="dodge", stat="identity") +
  geom_hline(  yintercept = wc
             , linetype ="dashed"
             , color = c("black","yellow")
             , size=0.5) +
  # geom_hline(yintercept=weekly$AnzahlFall[weekly$Bundesland=='Ost'][lo-1]
  #            , linetype="dashed", 
  #            color = "black", size=0.5) +
  # geom_hline(yintercept=weekly$AnzahlFall[weekly$Bundesland=='West'][lw-1]
  #            , linetype="dashed", 
  #            color = "black", size=0.5) +
  scale_fill_viridis(discrete = T) +
  ggtitle("Corona: Inzidenz Bundesländer Ost - West nach Kalenderwoche des Meldedatums") +
  theme_ipsum() +
  xlab("Kalenderwoche") +
  ylab("Gemeldete Fälle pro 100.000 pro Woche")


p2 <- ggplot(weekly, aes(fill=Bundesland, y=AnzahlTodesfall, x=Kw)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Corona: Todesfälle Bundesländer Ost - West nach Kalenderwoche des Meldedatums") +
  theme_ipsum() +
  xlab("Kalenderwoche") +
  ylab("Gemeldete Todesfälle pro 100.000 pro Woche")

gg <- grid.arrange(p1,p2, ncol=1)

plot(gg)

ggsave(  plot = gg
       , filename = paste('png/', MyScriptName,".png", sep="")
       , device = "png"
,  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150)

summary(warnings())

