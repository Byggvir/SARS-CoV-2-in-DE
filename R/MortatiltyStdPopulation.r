#!/usr/bin/env Rscript
#
#
# Script:  MortalityStdPopulation.r
#
# Stand: 2021-01-29
#
# (c) 2021 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

options(OutDec=',')

MyScriptName <- "MortalityStdPopulation"

require(data.table)
library(tidyverse)
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

today <- Sys.Date()
heute <- format(today, "%Y%m%d")
citation <- "© 2022 by Thomas Arend\nQuelle: Robert Koch-Institut (2022)\nSARS-CoV-2 Infektionen in Deutschland, Berlin\nZenodo. DOI:10.5281/zenodo.4681153"

Mortality <- RunSQL('call MortalityBundeslandStdBev();')

Mortality[,3] <- round(Mortality[,3],2)

print(Mortality)

png( paste( 
      "png/"
    ,  heute
    , "-Mortality-1.png"
    , sep = ""
)
, width = 1920
, height = 1080
)

vp <- viewport(
      x = 0.5
    , y = 0.5
    , width = 14
    , height = 10
    )

tt <- ttheme_default(
  base_size = 32
  , padding = unit(c(12,12), "mm")
  , core=list(
    fg_params=list( 
        hjust = 1
      , x = 0.99
    )
  )
)

table <- tableGrob(
  Mortality
  , theme = tt
  , cols = c("Rang", "Bundesland", "Tote pro 100k" )
  , vp = vp
)

title <- textGrob('Todesfälle Bundesländer',gp=gpar(fontsize=50))
footnote <- textGrob(paste('Altersstandadisierte Todesfälle pro 100.000 Einwohner. Stand:', heute), x=0, hjust=0,
                     gp=gpar( fontface="italic"))

padding <- unit(0.5,"line")

table <- gtable_add_rows(table, 
                         heights = grobHeight(title) + padding,
                         pos = 0)
table <- gtable_add_rows(table, 
                         heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title,footnote),
                         t=c(1, nrow(table)), l=c(1,2), 
                         r=ncol(table))

grid.draw(table)

dev.off()

Mortality %>% ggplot( aes( x = reorder(Bundesland, -Mortality), y = Mortality, fill = Bundesland)) +
  geom_bar( position = position_dodge(), stat="identity") +
  geom_text( aes( label = Mortality )
             , size=5
             , color = 'white'
             , position=position_dodge( width = 0.9 )
             , vjust= 0.5
             , hjust = 1
             , angle = 90 ) +
  
  scale_fill_viridis( discrete = TRUE ) +
  labs(  title = "Corona: Standardisierte Todesfälle pro 100.000 Einwohner"
         , subtitle = paste ("Deutschland, Stand:", heute, sep =' ')
         , x = "Bundesländer"
         , y = "Standardisierte Todesfälle pro 100.000"
         , colour = "Bundesland"
         , caption = citation ) +
  theme_ipsum() +
  theme( axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 12 )) -> p

ggsave( plot = p
        , filename = paste( 
          "png/"
          ,  heute
          , "-Mortality-2.png"
          , sep = ""
        )
        , device = 'png'
        , bg = "white"
        , width = 1920
        , height = 1080
        , units = "px"
        , dpi = 144)

# dev.off()
