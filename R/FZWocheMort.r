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
library(REST)
library(grid)
library(gridExtra)
library(magick)
library(gtable)
library(lubridate)
library(gganimate)
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
options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

citation <- "© 2021 by Thomas Arend\nQuellen: Robert Koch-Institut (2021)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153\nQuelle: © Statistisches Bundesamt (Destatis) Sonderauswertung, 2021"

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- 'select * from FallAltersgruppen where AlterVon >=35;'
Altersgruppen <- RunSQL(SQL = SQL)

for ( i in 1:nrow(Altersgruppen)) {
  
SQL <- paste(
  'select PandemieWoche, F.Jahr, F.Kw, AnzahlTodesfall as CoronaTodesfall, sum(Male)+sum(Female) as Todesfall '
  , ' from FaelleProWocheAltersgruppe as F join DESTATIS.SterbefaelleWoche as S'
  , ' on F.Jahr = S.Jahr and F.Kw = S.Kw'
  , ' where F.Altersgruppe = "', Altersgruppen[i,1], '"'
  , ' and S.AlterVon >= ', Altersgruppen[i,2]
  , ' and S.AlterBis <= ', Altersgruppen[i,3]
  , ' and PandemieWoche > 8'
  , ' group by PandemieWoche;'
  , sep='')

weekly <- RunSQL(SQL = SQL)

m <- length(weekly[,1])
reported <- weekly$Kw[m]

scl <- max(weekly$Todesfall)/max(weekly$CoronaTodesfall) 

weekly %>%  ggplot(
  aes( x = PandemieWoche )) +
  geom_line(aes(y = Todesfall, colour = 'Gesamt' ), color = 'blue') +
  geom_line(aes(y = CoronaTodesfall * scl , colour = 'SARS-CoV-2' ), color = 'red') +
  scale_color_manual( name = 'Todesfälle', values = c('Gesamt' = 'blue', 'SARS-CoV-2' = 'red')) +
  scale_y_continuous(  sec.axis = sec_axis(~./scl, name = "SARS-CoV-2-Todesfälle", labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ))
                       , labels = function (x) format(x, big.mark = '.', decimal.mark= ',', scientific = FALSE ) ) +
  labs(  title = paste('Gesamt und Corona-Todesfälle\nAltersgruppe', Altersgruppen[i,1], sep=' ')
         , subtitle = paste ('Deutschland, Stand:', heute, sep = ' ')
         , x = 'Pandemiewoche'
         , y = 'Fälle' 
         , colour = 'Todesfälle'
         , caption = citation 
         , legend.position="bottom"
  ) +
  theme_ipsum() +
  theme(  axis.text.y  = element_text ( color = 'blue' )
          , axis.title.y = element_text ( color = 'blue' )
          , axis.text.y.right = element_text ( color = 'red' )
          , axis.title.y.right = element_text ( color = 'red' )
          , strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "bold.italic"
            ) ) +
  theme(plot.title=element_text(size = 24, hjust = 0.5, face = "italic", color = "black" )) +
  theme(plot.subtitle=element_text(size = 18, hjust = 0.5, face = "italic", color = "black" )) -> pp

ggsave( paste('png/FZBundMort-', Altersgruppen[i,1], '-1.png', sep = '')
        , bg = "white"
        , width = 29.7
        , height = 21
        , units = "cm"
        , dpi = 300 )


weekly %>% ggplot(
  aes( x = Todesfall, y = CoronaTodesfall ) 
  ) +
  geom_point(aes(x = Todesfall, y = CoronaTodesfall , colour = factor(Jahr) ) ) +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_color_manual( breaks = c(2020,2021), values=c( "orange", "blue") ) +
  geom_smooth( method = "lm", data = weekly %>% filter ( Jahr == 2020 ), formula = y ~ x , colour = "orange" ) +
  geom_smooth( method = "lm", data = weekly %>% filter ( Jahr == 2021 ), formula = y ~ x , colour = "blue" ) +
  # facet_wrap(vars(Jahr)) +
  labs(  title = paste('SARS-CoV-2 Todesfälle ~ Todesfälle pro Kalenderwoche\nAltersgruppe', Altersgruppen[i,1], sep=' ') 
         , subtitle = paste ("Deutschland, Stand:", heute, sep ='')
         , x = "Alle Todesfälle"
         , y = "SARS-CoV-2"
         , colour = "Jahr"
         , caption = citation ) +
  theme_ipsum() +
  theme(  strip.text.x = element_text (
            size = 24
            , color = "black"
            , face = "italic"
          ) ) + 
  theme(plot.title=element_text( size = 24, hjust = 0.5, face = "bold.italic", color = "black" )) +
  theme(plot.subtitle=element_text( size = 18, hjust = 0.5, face = "italic", color = "black" )) -> pp2

ggsave( paste('png/FZBundMort-', Altersgruppen[i,1], '-2.png', sep = '')
         , bg = "white"
         , width = 29.7
         , height = 21
         , units = "cm"
         , dpi = 150 )

corona_sterbefaelle <- data.frame (
  PandemieWoche = c(weekly$PandemieWoche, weekly$PandemieWoche)
  , Art = c(rep('Corona',nrow(weekly)), rep('Normal',nrow(weekly)))
  , AnzahlTodesfall = c(weekly$CoronaTodesfall,weekly$Todesfall-weekly$CoronaTodesfall)
)
    
corona_sterbefaelle %>% ggplot(
  aes( x = PandemieWoche, y = AnzahlTodesfall , fill = Art)) +
  geom_bar(stat="identity") +
  scale_color_manual( name = 'Todesfälle', values = c('Gesamt' = 'blue', 'SARS-CoV-2' = 'red')) +
  scale_y_continuous( labels = function (x) format(x, big.mark = '.', decimal.mark= ',', scientific = FALSE ) ) +
  labs(  title = paste('Gesamt und Corona-Todesfälle\nAltersgruppe', Altersgruppen[i,1], sep=' ')
         , subtitle = paste ('Deutschland, Stand:', heute, sep = ' ')
         , x = 'Pandemiewoche'
         , y = 'Fälle' 
         , colour = 'Todesfälle'
         , caption = citation 
         , legend.position="bottom"
  ) +
  theme_ipsum() +
  theme(plot.title=element_text(size = 24, hjust = 0.5, face = "italic", color = "black" )) +
  theme(plot.subtitle=element_text(size = 18, hjust = 0.5, face = "italic", color = "black" )) -> pp

ggsave( paste('png/FZBundMort-', Altersgruppen[i,1], '-3.png', sep = '')
        , bg = "white"
        , width = 29.7
        , height = 21
        , units = "cm"
        , dpi = 300 )

}
