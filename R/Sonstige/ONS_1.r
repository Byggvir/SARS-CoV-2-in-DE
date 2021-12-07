#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZBundesland"

library(readODS)
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

WD <- paste(SD[1:(length(SD)-2)],collapse='/')

setwd(WD)

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: ONS"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date() - 1
heute <- format(today, "%d %b %Y")

st <- c(
  "for deaths involving COVID-19, per 100,000 people, England, deaths occurring between 2 January 2021 and 24 September 2021"
  , "for all deaths, per 100,000 people, England, deaths occurring between 2 January 2021 and 24 September 2021"
  
)

ci95 <- function ( k , n, l = -1 ) {
  
  return (k/n + l * 1.96* sqrt(k*(n-k)/n)/n)
  
}

for ( t in 3:4) {
  
  daten <- read_ods('data/ONS.ods',sheet= 8+t)
  AG <- unique(daten$Agegroup)
  
stats <- c (
  'Unvaccinated'
  , 'Within 21 days of first dose'
  , '21 days or more after first dose'
  , 'Second dose'
)
    daten$Nr <- factor(daten$Status,levels = stats
    , labels = 1:4
  )
  
for ( A in AG ) {
  
daten %>% filter(Agegroup == A) %>% ggplot( 
  aes(x = Week, y = Deaths/Population * 100000), group = Nr) +

  geom_line( data = daten %>% filter(Nr == 1 , Agegroup == A), aes(colour= 'Unvaccinated' ), size = 2 )  +
#    geom_pointrange( data = daten %>% filter(Nr == 1 , Agegroup == A), aes(ymin=ci95(Deaths,Population,-1)*100000, ymax=ci95(Deaths,Population,1)*100000, colour= 'Unvaccinated' ), size = 0.5 )  +
  geom_line( data = daten %>% filter(Nr == 2 , Agegroup == A), aes(colour= 'Within 21 days of first dose' ) )  +
#    geom_pointrange( data = daten %>% filter(Nr == 2 , Agegroup == A), aes(ymin=ci95(Deaths,Population,-1)*100000, ymax=ci95(Deaths,Population,1)*100000, colour= 'Within 21 days of first dose' ), size = 0.5 )  +
  geom_line( data = daten %>% filter(Nr == 3 , Agegroup == A), aes(colour= '21 days or more after first dose' ), size = 3)  +
#    geom_pointrange( data = daten %>% filter(Nr == 3 , Agegroup == A), aes(ymin=ci95(Deaths,Population,-1)*100000, ymax=ci95(Deaths,Population,1)*100000, colour= '21 days or more after first dose' ), size = 0.5 )  +
  geom_line( data = daten %>% filter(Nr == 4 , Agegroup == A), aes(colour= 'Second dose' ), size = 2 )  +
#    geom_pointrange( data = daten %>% filter(Nr == 4 , Agegroup == A), aes(ymin=ci95(Deaths,Population,-1)*100000, ymax=ci95(Deaths,Population,1)*100000, colour= 'Second dose' ), size = 0.5 )  +
  scale_y_continuous(labels = function (x) format(x, big.mark = ".", decimal.mark= ',', ) ) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  theme(  plot.title = element_text(size=24)
          , plot.subtitle = element_text(size=16)
         , strip.text.x = element_text (
          size = 12
          , color = "black"
          , face = "bold.italic"
        ) ) +
  labs(  title = paste('Table ',t ,': Weekly age-specific mortality rates by vaccination status, Age = ', A, sep='')
       , subtitle= st[t-2]
       , x = "Week"
       , y = "Age-specific rate per 100,000"
       , colour = 'Vaccination status' 
       , caption = citation ) -> p

ggsave(  paste('png/ONS_Tab',t,'_Alter',A,'.png', sep='')
       , type = "cairo-png"
       , bg = "white"
       , width = 29.7
       , height = 21
       , units = "cm"
       , dpi = 300 )
} # end A
} # end t
