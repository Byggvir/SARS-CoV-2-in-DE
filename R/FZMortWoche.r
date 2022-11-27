#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"FZWoche"

library(tidyverse)
#library(REST)
library(grid)
library(gridExtra)
library(magick)
library(gtable)
library(lubridate)
library(gganimate)
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
  
  #  When executi on command line 
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
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

citation <- "© 2022 by Thomas Arend\nQuellen: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153\n© Statistisches Bundesamt (Destatis) Sonderauswertung"

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- 'select * from FallAltersgruppen where AlterVon >=15;'
Altersgruppen <- RunSQL(SQL = SQL)

for ( i in 1:nrow(Altersgruppen)) {
  
  SQL <- paste(
    'select PandemieWoche, F.Jahr, F.Kw, AnzahlTodesfall as CoronaTodesfall, sum(Gestorbene) as Todesfall '
    , ' from FaelleProWocheAltersgruppe as F join DESTATIS.SterbefaelleWoche as S'
    , ' on F.Jahr = S.Jahr and F.Kw = S.Kw'
    , ' where F.Altersgruppe = "', Altersgruppen[i,1], '"'
    , ' and S.AlterVon >= ', Altersgruppen[i,2]
    , ' and S.AlterBis <= ', Altersgruppen[i,3]
    , ' and PandemieWoche > 8'
    , ' group by PandemieWoche;'
    , sep='')
  
  weekly <- RunSQL(SQL = SQL)
  weekly$Jahre <- factor(weekly$Jahr, levels = unique(weekly$Jahr), labels = unique(weekly$Jahr))
  
  w <- weekly
  w$CoronaTodesfall <- c( w$CoronaTodesfall[1:(nrow(w)- 2)],0,0)
  
  m <- length(weekly[,1])
  reported <- weekly$Kw[m]
  
  scl <- max(weekly$Todesfall)/max(weekly$CoronaTodesfall) 
  
  w %>% filter ( Jahr == 2022 & PandemieWoche < 146 ) %>% ggplot(
    aes( x = PandemieWoche )) +
    geom_line(aes(y = Todesfall, colour = 'Gesamt' )) +
    geom_line(aes(y = CoronaTodesfall , colour = 'SARS-CoV-2' )) +
    geom_line(aes(y = Todesfall - CoronaTodesfall, colour = 'Todesfälle ohne Corona' )) +
    # scale_color_manual( name = 'Todesfälle', values = c('Gesamt' = 'blue', 'SARS-CoV-2' = 'red')) +
    # scale_y_continuous(  sec.axis = sec_axis(~./scl, name = "SARS-CoV-2-Todesfälle", labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ))
    #                      , labels = function (x) format(x, big.mark = '.', decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = '.', decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Gesamt und Corona-Todesfälle\nAltersgruppe', Altersgruppen[i,1], sep=' ')
           , subtitle = paste ('Deutschland, Stand:', heute, sep = ' ')
           , x = 'Pandemiewoche'
           , y = 'Fälle' 
           , colour = 'Todesfälle'
           , caption = citation 
    ) +
    theme_ipsum() +
    theme(  axis.text.y  = element_text ( color = 'blue' )
            , axis.title.y = element_text ( color = 'blue' )
            , axis.text.y.right = element_text ( color = 'red' )
            , axis.title.y.right = element_text ( color = 'red' )

    ) -> pp1
  
  ggsave( paste('png/FZMortWoche-', Altersgruppen[i,1], '-1.png', sep = '')
          , plot =pp1
          , device = 'png'
          , bg = "white"
          , width = 3840
          , height = 2160
          , units = "px"
  )
  
  w %>% filter (Jahr == 2022 & PandemieWoche < 146  ) %>% ggplot(
    aes( x = Todesfall, y = CoronaTodesfall, group = Jahre, colour = Jahre ) 
    ) +
    geom_point() +
    geom_text_repel( aes(label = Kw), vjust = 0, hjust = 0) +
    geom_smooth( method = "lm", formula = y ~ x ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    # facet_wrap(vars(Jahr)) +
    labs(  title = paste('SARS-CoV-2 Todesfälle ~ Todesfälle pro Kalenderwoche\nAltersgruppe', Altersgruppen[i,1], sep=' ') 
           , subtitle = paste ("Deutschland, Stand:", heute, sep ='')
           , x = "Alle Todesfälle"
           , y = "SARS-CoV-2"
           , colour = "Jahr"
           , caption = citation ) +
    theme_ta() +
    theme(  axis.text.y  = element_text ( color = 'blue' )
            , axis.title.y = element_text ( color = 'blue' )
            , axis.text.y.right = element_text ( color = 'red' )
            , axis.title.y.right = element_text ( color = 'red' )
    ) -> pp2
  
  ggsave( paste( 'png/FZMortWoche-', Altersgruppen[i,1], '-2.png', sep = '')
          , plot = pp2        
          , device = 'png'
          , bg = "white"
          , width = 3840
          , height = 2160
          , units = "px"
          )
  
  corona_sterbefaelle <- data.frame (
    PandemieWoche = c(weekly$PandemieWoche, weekly$PandemieWoche)
    , Art = c(rep('Corona',nrow(weekly)), rep('Normal',nrow(weekly)))
    , AnzahlTodesfall = c(weekly$CoronaTodesfall,weekly$Todesfall-weekly$CoronaTodesfall)
  )
      
  corona_sterbefaelle %>% ggplot(
    aes( x = PandemieWoche, y = AnzahlTodesfall , fill = Art)) +
    geom_bar(stat="identity") +
    # scale_color_manual( name = 'Todesfälle', values = c('Gesamt' = 'blue', 'SARS-CoV-2' = 'red')) +
    scale_y_continuous( labels = function (x) format(x, big.mark = '.', decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Gesamt und Corona-Todesfälle\nAltersgruppe', Altersgruppen[i,1], sep=' ')
           , subtitle = paste ('Deutschland, Stand:', heute, sep = ' ')
           , x = 'Pandemiewoche'
           , y = 'Fälle' 
           , colour = 'Todesfälle'
           , caption = citation 
           , legend.position="bottom"
    ) +
    theme_ta() +
    theme(  axis.text.y  = element_text ( color = 'blue' )
            , axis.title.y = element_text ( color = 'blue' )
            , axis.text.y.right = element_text ( color = 'red' )
            , axis.title.y.right = element_text ( color = 'red' )
    ) -> pp3
  
  ggsave( paste('png/FZMortWoche-', Altersgruppen[i,1], '-3.png', sep = '')
          , plot = pp3
          , device = 'png'
          , bg = "white"
          , width = 3840
          , height = 2160
          , units = "px"
          )
  w %>%  ggplot(
    aes( x = PandemieWoche ) ) +
    geom_line( aes( y = CoronaTodesfall / Todesfall ), color = 'blue') +
    # scale_x_date() +
    scale_y_continuous( labels = scales::percent  ) +
    labs(  title = paste('Anteil der Corona-Todesfälle an allen Todesfällen', sep=' ')
           , subtitle = paste (' Altersgruppe', Altersgruppen[i,1], ' Stand:', heute, sep = ' ')
           , x = 'Monat'
           , y = 'Anteil' 
           , caption = citation
    ) +
    theme_ta() -> pp4
  
  ggsave( paste('png/FZMortWoche-', Altersgruppen[i,1], '-4.png', sep = '')
          , plot = pp4
          , device = 'png'
          , bg = "white"
          , width = 3840
          , height = 2160
          , units = "px"
  )

}
