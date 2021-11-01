#!/usr/bin/env Rscript
#
#
# Script: Infektionsvergleiche.r
#
# Stand: 2021-01-29
# (c) 2021 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

options(OutDec=',')
MyScriptName <- "Infektionsvergleiche"

require(data.table)
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
library(extrafont)
extrafont::loadfonts()

# Set Working directory to git root

if (rstudioapi::isAvailable()){
  
  # When called in RStudio
  SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
  
} else {
  
  #  When called from command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-1)],collapse='/')
setwd(WD)

source("R/lib/copyright.r")
source("R/lib/sql.r")

today <- Sys.Date()
heute <- format(today, "%Y%m%d")


SQL <- 'select F.IdLandkreis div 1000 as IdBundesland,B.Bundesland,F.Altersgruppe,sum(AnzahlTodesFall) as AnzahlTodesfall,sum(AnzahlFall) as AnzahlFall, Bev from Faelle as F join Bundesland as B on B.IdBundesland = F.IdLandkreis div 1000 join DESTATIS.StdBevRKIAG as S on F.Altersgruppe = S.Altersgruppe and F.IdLandkreis div 1000 = S.IdBundesland group by F.IdLandkreis div 1000, F.Altersgruppe;'
if ( ! exists("data") ) {
  data <- RunSQL(SQL)
  
}

a <- data %>% filter (Altersgruppe == "A05-A14")
b <- data %>% filter (Altersgruppe == "A35-A59")
c <- data %>% filter (Altersgruppe == "A80+")

infectrate <- data.frame(
      Bundesland = a$Bundesland
    , A = a$AnzahlFall / a$Bev
    , B = b$AnzahlFall / b$Bev
    , C = c$AnzahlFall / c$Bev
)

infectrate %>% ggplot( aes( x = A * 100 )) +
  geom_point( aes( y = B * 100, colour = Bundesland), shape = 21, size = 3, stroke = 1) +
  geom_point( aes( y = C * 100, colour = Bundesland), shape = 23, size = 3, stroke = 1) +
  # facet_wrap(vars(Altersgruppe)) +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Inzifiziert - CFR") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Anteil Infizierter an der Bevölkerung [%]") +
  ylab("CFR [%]")+
  theme(plot.title=element_text(size=48, hjust=0.5, face="italic", color="black")) +
  theme(plot.subtitle=element_text(size=36, hjust=0.5, face="italic", color="black")) -> p

  ggsave( plot = p, 
          file = paste( 
            "png/"
            , MyScriptName
            , '.png'
            , sep = ""
          )
          , type = "cairo-png",  bg = "white"
          , width = 29.7, height = 21, units = "cm", dpi = 150)
  
