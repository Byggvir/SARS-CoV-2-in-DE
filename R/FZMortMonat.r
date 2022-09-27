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

citation <- "© 2022 by Thomas Arend\nQuellen: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153\nQuelle: © Statistisches Bundesamt (Destatis) Sonderauswertung, 2021"

today <- Sys.Date()
heute <- format(today, "%d %b %Y")


PlotAG <- function ( SQL, Altersgruppe)  {
  
Monate <- RunSQL(SQL = SQL)

m <- length(Monate[,1])
reported <- Monate$Datum[m]

scl <- max(Monate$Todesfall)/max(Monate$CoronaTodesfall) 

Monate %>%  ggplot(
  aes( x = Datum )) +
  geom_line(aes(y = Todesfall, colour = 'Gesamt' ), color = 'blue') +
  geom_line(aes(y = CoronaTodesfall * scl , colour = 'SARS-CoV-2' ), color = 'red') +
  scale_color_manual( name = 'Todesfälle', values = c('Gesamt' = 'blue', 'SARS-CoV-2' = 'red')) +
  scale_y_continuous(  sec.axis = sec_axis(~./scl, name = "SARS-CoV-2-Todesfälle", labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ))
                       , labels = function (x) format(x, big.mark = '.', decimal.mark= ',', scientific = FALSE ) ) +
  labs(  title = paste('Gesamt und Corona-Todesfälle\nAltersgruppe', Altersgruppe, sep=' ')
         , subtitle = paste ('Deutschland, Stand:', heute, sep = ' ')
         , x = 'Monat'
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
  ) -> pp

ggsave( paste('png/FZMortMonat-', Altersgruppe, '-1.png', sep = '')
        , device = 'png'
        , bg = "white"
        , width = 3840
        , height = 2160
        , units = "px"
)


Monate %>% ggplot(
  aes( x = Todesfall, y = CoronaTodesfall ) 
) +
  geom_point(aes(x = Todesfall, y = CoronaTodesfall , colour = factor(Jahr) ) ) +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_color_manual( breaks = c(2020,2021,2022), values=c( "orange", "blue","green") ) +
  geom_smooth( method = "lm", data = Monate , formula = y ~ x , aes(colour = factor(Jahr)) ) +
  # facet_wrap(vars(Jahr)) +
  labs(  title = paste('SARS-CoV-2 Todesfälle ~ Todesfälle pro Monat\nAltersgruppe', Altersgruppe, sep=' ') 
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

ggsave( paste('png/FZMortMonat-', Altersgruppe, '-2.png', sep = '')
        , device = 'png'
        , bg = "white"
        , width = 3840
        , height = 2160
        , units = "px"
)

corona_sterbefaelle <- data.frame (
  Datum  = c(Monate$Datum, Monate$Datum)
  , Art = c(rep('Corona',nrow(Monate)), rep('Normal',nrow(Monate)))
  , AnzahlTodesfall = c(Monate$CoronaTodesfall,Monate$Todesfall-Monate$CoronaTodesfall)
)

corona_sterbefaelle %>% ggplot(
  aes( x = Datum, y = AnzahlTodesfall , fill = Art)) +
  geom_bar(stat="identity") +
  scale_color_manual( name = 'Todesfälle', values = c('Gesamt' = 'blue', 'SARS-CoV-2' = 'red')) +
  scale_y_continuous( labels = function (x) format(x, big.mark = '.', decimal.mark= ',', scientific = FALSE ) ) +
  labs(  title = paste('Gesamt und Corona-Todesfälle\nAltersgruppe', Altersgruppe, sep=' ')
         , subtitle = paste ('Deutschland, Stand:', heute, sep = ' ')
         , x = 'Monat'
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

ggsave( paste('png/FZMortMonat-', Altersgruppe, '-3.png', sep = '')
        , plot = pp3
        , device = 'png'
        , bg = "white"
        , width = 3840
        , height = 2160
        , units = "px"
)

Monate %>%  ggplot(
  aes( x = Datum )) +
  geom_line( aes( y = CoronaTodesfall / Todesfall ), color = 'blue') +
  scale_x_date() +
  scale_y_continuous( labels = scales::percent  ) +
  labs(  title = paste('Anteil der Corona-Todesfälle an allen Todesfällen', sep=' ')
         , subtitle = paste (' Altersgruppe', Altersgruppe, ' Stand:', heute, sep = ' ')
         , x = 'Monat'
         , y = 'Anteil' 
         , caption = citation
  ) +
  theme_ta() -> pp4

ggsave( paste('png/FZMortMonat-', Altersgruppe, '-4.png', sep = '')
        , plot = pp4
        , device = 'png'
        , bg = "white"
        , width = 3840
        , height = 2160
        , units = "px"
)

}

SQL <- 'select * from FallAltersgruppen;'
Altersgruppen <- RunSQL(SQL = SQL)

SQL <- paste(
  ' select date(concat(F.Jahr, "-", F.Monat, "-01")) as Datum, F.Jahr, F.Monat, sum(AnzahlTodesfall) as CoronaTodesfall, sum(Gestorbene) as Todesfall '
, ' from FaelleProMonatAltersgruppe as F join DESTATIS.SterbefaelleMonat as S'
, ' on F.Jahr = S.Jahr and F.Monat = S.Monat'
, ' where ( F.Altersgruppe = "A00-A04" or F.Altersgruppe = "A05-A14" ) '
, ' and S.AlterVon >= 0 '
, ' and S.AlterBis <= 14'
, ' group by F.Jahr, F.Monat'
, ';'
, sep='')

PlotAG (SQL, "A00-A14")

for ( i in 3:nrow(Altersgruppen)) {
  
SQL <- paste(
  'select date(concat(F.Jahr, "-", F.Monat, "-01")) as Datum, F.Jahr, F.Monat, AnzahlTodesfall as CoronaTodesfall, sum(Gestorbene) as Todesfall '
  , ' from FaelleProMonatAltersgruppe as F join DESTATIS.SterbefaelleMonat as S'
  , ' on F.Jahr = S.Jahr and F.Monat = S.Monat'
  , ' where F.Altersgruppe = "', Altersgruppen[i,1], '"'
  , ' and S.AlterVon >= ', Altersgruppen[i,2]
  , ' and S.AlterBis <= ', Altersgruppen[i,3]
  , ' group by F.Jahr, F.Monat'
  , ';'
  , sep='')

PlotAG (SQL, Altersgruppen[i,1])


}

