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
#library(REST)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(scales)
library(ragg)
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

outdir <- 'png/'
dir.create( outdir , showWarnings = TRUE, recursive = FALSE, mode = "0777")

today <- Sys.Date()
heute <- format(today, "%Y%m%d")


SQL <- 'select F.IdLandkreis div 1000 as IdBundesland,B.Bundesland,F.Altersgruppe,sum(AnzahlTodesFall) as AnzahlTodesfall,sum(AnzahlFall) as AnzahlFall, Bev from Faelle as F join Bundesland as B on B.IdBundesland = F.IdLandkreis div 1000 join DESTATIS.StdBevRKIAG as S on F.Altersgruppe = S.Altersgruppe and F.IdLandkreis div 1000 = S.IdBundesland and F.Altersgruppe >= "A35" group by F.IdLandkreis div 1000, F.Altersgruppe;'

if ( ! exists("result") ) {
  
  result <- RunSQL(SQL)
  
}


result %>% ggplot( aes( x = AnzahlFall/Bev * 100, y = AnzahlTodesfall/Bev * 100 )) +
  geom_point( aes( y = AnzahlTodesfall/Bev * 100, colour = Bundesland), shape = 21, size = 3, stroke = 1) +
  geom_smooth (method = "lm", data = result, formula = y ~ x )  +
  facet_wrap(vars(Altersgruppe)) +
  scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Verstorben ~ Inzifiziert") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Anteil Infizierter an der Bevölkerung [%]") +
  ylab("Anteil Verstorbener an der Bevölkerung [%]")+
  theme(plot.title=element_text(size=48, hjust=0.5, face="italic", color="black")) +
  theme(plot.subtitle=element_text(size=36, hjust=0.5, face="italic", color="black")) -> p

  ggsave( plot = p, 
          file = paste( 
            outdir
            , MyScriptName
            , '.png'
            , sep = ""
          )
          ,  bg = "white"
          , width = 29.7
          , height = 21
          , units = "cm"
          , dpi = 150)
  
