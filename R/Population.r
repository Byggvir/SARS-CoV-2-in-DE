#!usr/bin/env Rscript

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
    
    #  When executi on command line 
    SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
    SD <- unlist(str_split(SD,'/'))
    
}

WD <- paste(SD[1:(length(SD)-1)],collapse='/')

setwd(WD)
require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: DESTATIS"

options(scipen=10)

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- 'select year(Stichtag) as Jahr, sum(Male) as Male, sum(Female) as Female from DEU where Age >= 80 group by year(Stichtag)'
germany <- RunSQL (SQL)

germany %>% ggplot(
    aes( x = Jahr ) ) +
    geom_line(aes( y = Male, col = "Männer" )) +
    geom_line(aes( y = Female, col = "Frauen" )) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE)) +
    # geom_bar(aes(y = Female, col = "red"),position="dodge", stat="identity") +
    theme_ipsum() +
    labs(  title = "Bevölkerung älter als 80 Jahre"
           , subtitle= paste("Deutschland, Stand:", heute)
           , colour = "Geschlecht"
           , fill = "Geschlecht"
           , legend = "Geschlecht"
           , x ="Jahr"
           , y = "Anzahl"
           , caption = citation ) -> pp

ggsave("png/DEPopulationAge.png"
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150
)

SQL <- 'select year(Stichtag) as Jahr, Age , Male as Male, Female as Female from DEU where Age >= 80 and Age < 85 and Stichtag >= "2015-12-31";'
germany_alter <- RunSQL (SQL)

germany_alter %>% ggplot(
    aes( x = Age ) ) +
    geom_line(aes( y = Male, col = "Männer" )) +
    geom_line(aes( y = Female, col = "Frauen" )) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE)) +
    facet_wrap(vars(Jahr)) +
    theme_ipsum() +
    labs(  title = "Bevölkerung älter als 80 - 84 Jahre"
           , subtitle= paste("Deutschland, Stand:", heute)
           , x ="Alter"
           , y = "Anzahl"
           , colour = 'Geschlecht'
           , caption = citation ) -> pp

ggsave("png/DEPopulationAge2.png"
       , type = "cairo-png",  bg = "white"
       , width = 29.7, height = 21, units = "cm", dpi = 150
)
