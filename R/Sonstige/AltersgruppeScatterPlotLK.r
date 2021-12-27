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
#untilday <- as.Date("2021-11-15")
z1 <- format(untilday, "%Y-%m-%d")
z2 <- format(untilday-6, "%Y-%m-%d")
z3 <- format(untilday-7, "%Y-%m-%d")
z4 <- format(untilday-13, "%Y-%m-%d")

SQL <- 'select * from FallAltersgruppen where Altersgruppe <> "unbekan";'
AG <- RunSQL(SQL = SQL)

for ( AG1 in AG[,1] ) {
  
  for ( AG2 in AG[,1] ) {
    
    if ( AG1 < AG2 ) {

      
      SQL <- paste(
        'call Altersgruppe714LKVergleich("',untilday,'","',AG1,'","',AG2,'");'
        , sep = '')
      
      Altersgruppen <- RunSQL(SQL = SQL)

      ra <- lm(R2 ~ R1, data = Altersgruppen)
      print(summary(ra))
      
      Altersgruppen %>% ggplot() +
        geom_point( aes( x = R1, y = R2), size = 6, alpha = 0.8) +
        geom_smooth( aes( x = R1, y = R2 )) +
        geom_abline(slope = ra$coefficients[2], intercept = ra$coefficients[1], size = 1, color = 'red' ) +
        annotate( 
          "text"
          , x = min(Altersgruppen$R1)
          , y = min(Altersgruppen$R2) - 0.05
          , label = paste( 
            'R² ='
            , round(as.numeric(summary(ra)$r.squared),4)
            , '/ Adj. R² = '
            , round(as.numeric(summary(ra)$adj.r.squared),4)
          )
          , hjust = 0
          , vjust = 0
          , color = 'red'
        ) +
        scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
        scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
        scale_fill_viridis(discrete = T) +
        theme_ipsum() +
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
            title= paste('R-Wert Vergleich Landkreise',AG2,'~',AG1)
          , subtitle = paste('Meldedatum', untilday)
          , x = paste( 'R-Wert', AG1 )
          , y = paste( 'R-Wert', AG2 )
          , caption = citation 
        )

      ggsave(  
          paste('png/Altersgruppen-Tendenz-R_LK_',format(untilday, "%Y-%m-%d"),'_',AG1,'_',AG2,'.png', sep='')
        , device = 'png'
        , bg = "white"
        , width = 29.7
        , height = 21
        , units = "cm"
        , dpi = 300
      )

    } # it then
  }
}
