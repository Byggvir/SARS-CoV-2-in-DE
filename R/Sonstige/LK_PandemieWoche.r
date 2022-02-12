#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"LK_PandemieWoche"

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
  SD = (function() return( if(length(sys.parents()) == 1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-2)],collapse='/')

setwd(WD)

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")
source("R/lib/color_palettes.r")

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021), GitHub SARS-CoV-1 Infektionen"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date() - 1
heute <- format(today, "%d %b %Y")

WeekLabel <- function (P) {
  
  PLabel <- rep('', each = length(P))
  PLabel[P>105] <- paste(2022,'-W',P[P>105] - 105, sep = '')
  PLabel[P<=105 & P>53] <- paste(2021,'-W',P[P<=105 & P>53] - 53, sep = '')
  PLabel[P < 54] <- paste(2020,'-W',P[P<54], sep = '')
  return(PLabel)
  
}

PWoche <- ( as.integer(today - as.Date('2019-12-29')) - 4 ) %/% 7 + 1

SQL <- 'select * from Bundesland order by IdBundesland;'
BL <- RunSQL(SQL)

ExecSQL(SQL='call LandkreisePw;')
ExecSQL(SQL='call BundeslandPw;')

SQL <- paste(
    '
    select 
      A.IdLandkreis as IdLandkreis
    , L.Landkreis as Landkreis
    , A.IdLandkreis div 1000 as IdBundesland
    , A.Jahr as Jahr
    , A.Kw as Kw
    , A.Pw as Pw
    , A.AnzahlFall as Anzahl
    , B.AnzahlFall as AnzahlVorwoche
    , L.EW_insgesamt from FaelleLandkreisPw as A 
    
    join FaelleLandkreisPw as B 
    on A.IdLandkreis = B.IdLandkreis and A.Pw = B.Pw + 1 
    join Landkreis as L
    on A.IdLandkreis = L.IdLandkreis
    where ', PWoche,' >= A.Pw and A.Pw >= ', PWoche - 5, ';'
  , sep = ' ')

Landkreise <- RunSQL(SQL = SQL)

SQL <- paste(
  '
    select 
      A1.IdBundesland as IdBundesland
    , B.Bundesland as Bundesland
    , A1.Jahr as Jahr
    , A1.Kw as Kw
    , A1.Pw as Pw
    , A1.AnzahlFall as Anzahl
    , A2.AnzahlFall as AnzahlVorwoche
    , B.EW_insgesamt
    from FaelleBundeslandPw as A1 
    
    join FaelleBundeslandPw as A2
    on A1.IdBundesland = A2.IdBundesland and A1.Pw = A2.Pw + 1 
    join Bundesland as B
    on A1.IdBundesland = B.IdBundesland
    where ', PWoche,' >= A1.Pw and A1.Pw >=', PWoche - 5,';'
  , sep = ' ')

Bundesland <- RunSQL(SQL = SQL)

Landkreise$PwLabel <- WeekLabel(Landkreise$Pw)
Bundesland$PwLabel <- WeekLabel(Bundesland$Pw)

for ( B in 1:16) {
  
  L <- Landkreise %>% filter(IdBundesland == B )
  max_Inzidenz <- max(c(L$Anzahl/L$EW_insgesamt,L$AnzahlVorwoche/L$EW_insgesamt)) * 100000
  
  L %>% ggplot() +
  stat_ellipse( aes( x = AnzahlVorwoche / EW_insgesamt * 100000, y = Anzahl / EW_insgesamt * 100000), type = "t", geom = "polygon", alpha = 0.1 ) +
    geom_abline( intercept = 0, slope = 1.5, color = 'red' ) +
    geom_abline( intercept = 0, slope = 1, color = 'yellow' ) +
    geom_abline( intercept = 0, slope = 0.75, color = 'green' ) +
  geom_point( aes( x = AnzahlVorwoche / EW_insgesamt * 100000, y = Anzahl / EW_insgesamt * 100000, colour=Landkreis), size = 2 ) +
  geom_point( data = Bundesland %>% filter( IdBundesland == B )
                , aes( x = AnzahlVorwoche / EW_insgesamt * 100000, y = Anzahl / EW_insgesamt * 100000 )
                , shape = 3, stroke = 2, fill = "blue", color = "darkblue", alpha = 0.5, size = 5 ) +
    
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_fill_viridis( discrete = TRUE ) +

  coord_fixed( xlim = limbounds( c( 0, max_Inzidenz ) ), ylim = limbounds( c( 0, max_Inzidenz ) ) ) +
  
  facet_wrap( vars(PwLabel) ) +
  
  theme_ipsum() +
  theme(  plot.title = element_text( size = 24 )
          , legend.position="none"
         , strip.text.x = element_text (
          size = 12
          , color = "black"
          , face = "bold.italic"
        ) ) +
  labs( title= paste("Landkreise",BL[B+1,2]) 
       , subtitle = "Fallzahlen pro 100.000 Einwohner\nWoche ~ Vorwoche"
       , x = "Fälle Vorwoche pro 100.000"
       , y = "Fälle Woche pro 100.000"
       , caption = citation )

ggsave(  paste('png/LK/PandemieWoche-',BL[B+1,2],'.png', sep='')
       , bg = "white"
       , width = 29.7
       , height = 21
       , units = "cm"
       , dpi = 300 )
}

