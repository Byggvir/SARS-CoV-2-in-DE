#!/usr/bin/env Rscript
#
#
# Script: ONS2.r
#
# Stand: 2021-12-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"ONS2"

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

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

# Source: Deaths involving COVID-19 by vaccination status, England: deaths occurring between 2 January and 24 September 2021
# Download: https://www.ons.gov.uk/

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

daten <- read_ods('data/ONS2.ods',sheet= 12)
AG <- unique(daten$Age_group)
  
stats <- c (
  'Unvaccinated'
  , 'Within 21 days of first dose'
  , '21 days or more after first dose'
  , 'Within 21 days of second dose'
  , '21 days or more after second dose'
)

daten$Nr <- factor(daten$Status,levels = stats

  )
for (A in AG) {
  
daten %>% filter(Age_group == A) %>% ggplot() +
  geom_line( aes( x = as.Date(Month), y = Deaths/Person_years * 100000, colour = Nr), size = 1 )  +
  scale_y_continuous(labels = function (x) format(x, big.mark = ".", decimal.mark= ',', ) ) +
  scale_fill_viridis(discrete = T) +
  
  # facet_wrap(vars(Age_group)) +
  theme_ipsum() +
  theme(  plot.title = element_text(size=24)
          , plot.subtitle = element_text(size=16)
          , strip.text.x = element_text (
              size = 12
              , color = "black"
              , face = "bold.italic"
            ) 
          , axis.text.x = element_text(angle = 90)
          ) +
  labs(  title = paste('Monthy mortality rates by vaccination status, 2021')
       , subtitle= paste ('Age group', A)
       , x = "Month"
       , y = "Age-specific rate per 100,000 person years"
       , colour = 'Vaccination status' 
       , caption = citation ) -> p

ggsave(  
        paste('png/ONS_2_',A,'.png', sep='')
      , bg = "white"
      , width = 29.7
      , height = 21
      , units = "cm"
      , dpi = 300 )

}
