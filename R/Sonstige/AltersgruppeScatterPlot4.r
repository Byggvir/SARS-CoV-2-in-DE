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

library(tidyverse)
#library(REST)
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

fSQL <- 'select * from Bundesland where IdBundesland<>0;'
Bundesland <- RunSQL(SQL = fSQL)

SQL <- paste(
  'select * from RVergleich where Meldedatum < adddate(now(),-4) and Meldedatum > "2012-07-15";'
  , sep = '')

RWerte <- RunSQL(SQL = SQL)

for (B in Bundesland[,2] ) {
  
      RWerte %>% filter( Bundesland == B ) %>% ggplot() +
        geom_point( aes( x = R1, y = R2), size = 1, alpha = 0.8) +
        geom_smooth( aes( x = R1, y = R2 )) +
#        geom_abline(slope = ra$coefficients[2], intercept = ra$coefficients[1], size = 1, color = 'red' ) +
        # annotate( 
        #   "text"
        #   , x = min(RWerte$R1)
        #   , y = min(RWerte$R2) - 0.05
        #   , label = paste( 
        #     'R² ='
        #     , round(summary(ra)$r.squared,4)
        #     , '/ Adj. R² = '
        #     , round(summary(ra)$adj.r.squared,4)
        #   )
        #   , hjust = 0
        #   , vjust = 0
        #   , color = 'red'
        # ) +
        # annotate( 
        #   "text"
        #   , x = min(RWerte$R1)
        #   , y = max(RWerte$R2) - 0.05
        #   , label = paste( 
        #     'R2 ='
        #     , round(ra$coefficients[1],4)
        #     , '+'
        #     , round(ra$coefficients[2],4)
        #     , ' * R1'
        #   )
        #   , hjust = 0
        #   , vjust = 0
        #   , color = 'red'
        # ) +
        scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
        scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
        scale_fill_viridis(discrete = T) +
        facet_wrap(vars(Altersgruppe1, Altersgruppe2),ncol = 6) +
        # theme_ipsum() +
        theme(  
          plot.title = element_text( size = 24 )
          , legend.position="none"
          , strip.text.x = element_text (
            size = 12
            , color = "black"
            , face = "bold.italic"
          ) 
        ) +
        labs(  
          title= paste('Korrelation der R-Werte in den Altersgruppen in', B)
          , subtitle = paste('Bis Meldedatum', untilday)
          , x = paste( 'R-Wert Altersgruppe 1' )
          , y = paste( 'R-Wert Altersgruppe 2' )
          , caption = citation 
        )
      
      ggsave(  
        paste('png/Altersgruppen-RWerte_',B,'.png', sep='')
        , device = 'png'
        , bg = "white"
        , width = 29.7 * 2
        , height = 21 * 2
        , units = "cm"
        , dpi = 300
      )
      
} # Bundesland


