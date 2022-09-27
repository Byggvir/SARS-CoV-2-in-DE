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

fPrefix <- "Ausprobieren_"

require(data.table)

source("R/lib/myfunctions.r")
source("R/lib/mytheme.r")
source("R/lib/sql.r")

citation <- "© 2021 by Thomas Arend\nQuelle: Robert Koch-Institut (2021), GitHub SARS-CoV-1 Infektionen"

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

today <- Sys.Date() - 1
heute <- format(today, "%d %b %Y")

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
    , A.Pw as Pw
    , A.AnzahlFall as Anzahl
    , B.AnzahlFall as AnzahlVorwoche
    , L.EW_insgesamt
    from FaelleLandkreisPw as A 
    
    join FaelleLandkreisPw as B 
    on A.IdLandkreis = B.IdLandkreis and A.Pw = B.Pw + 1 
    join Landkreis as L
    on A.IdLandkreis = L.IdLandkreis
    order by
      A.IdLandkreis, A.Pw

    ; '
  , sep = ' ')

Landkreise <- RunSQL(SQL = SQL)

SQL <- paste(
  '
    select 
      A1.IdBundesland as IdBundesland
    , B.Bundesland as Bundesland
    , A1.Pw as Pw
    , A1.AnzahlFall as Anzahl
    , A2.AnzahlFall as AnzahlVorwoche
    , B.EW_insgesamt
    from FaelleBundeslandPw as A1 
    
    join FaelleBundeslandPw as A2
    on A1.IdBundesland = A2.IdBundesland and A1.Pw = A2.Pw + 1 
    join Bundesland as B
    on A1.IdBundesland = B.IdBundesland 
    order by
      A1.IdBundesland, A1.Pw
  ;'
  , sep = ' ')

Bundesland <- RunSQL(SQL = SQL)


for ( B in 1:16) {
  
  print(BL[B+1,2])
  
  L <- Landkreise %>% filter(IdBundesland == B )
  max_Inzidenz <- max(L$Anzahl/L$EW_insgesamt) * 100000
  
  for (P in unique(Bundesland$Pw)) {
    
    Landkreise %>% filter(IdBundesland == B & Pw == P ) %>% ggplot() +
        stat_ellipse(aes( x = AnzahlVorwoche / EW_insgesamt * 100000, y = Anzahl / EW_insgesamt * 100000), type = "t", geom = "polygon", alpha = 0.1 ) +
        geom_abline(intercept = 0,slope = 1, color ='red') +
        geom_point( aes( x = AnzahlVorwoche / EW_insgesamt * 100000, y = Anzahl / EW_insgesamt * 100000, colour=Landkreis), show.legend = FALSE, size = 2) +
        geom_point( data = Bundesland %>% filter(IdBundesland == B & Pw == P)
                    , aes( x = AnzahlVorwoche / EW_insgesamt * 100000, y = Anzahl / EW_insgesamt * 100000)
                    , shape = 3, stroke = 2, fill = "blue", color = "darkblue", alpha = 0.5, size = 5, show.legend = FALSE ) +
    
        scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
        scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
        scale_fill_viridis(discrete = T) +
        coord_fixed(xlim=limbounds(max_Inzidenz), ylim = limbounds(max_Inzidenz),expand = FALSE ) +
        theme_ta() +
        labs( title= paste(BL[B+1,2],'Woche', P) 
            , subtitle = "Fallzahlen je Landkreis pro 100.000 Einwohner\nWoche ~ Vorwoche"
            , x = "Fälle Vorwoche pro 100.000"
            , y = "Fälle Woche pro 100.000"
            , caption = citation )

        ggsave( paste('png/Entwicklung/Pw-', BL[B+1,2],'-',str_pad(P, 4, pad = "0"),'.png', sep='')
              , bg = "white"
              , device = 'png'
              , width = 3840
              , height = 2160
              , units = "px"
        )
  }
}
