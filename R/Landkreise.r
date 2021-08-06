#!/usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

options(OutDec=',')

MyScriptName <- "Landkreis.r"

require(data.table)
library(REST)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(tidyverse)

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

source("R/lib/copyright.r")
source("R/lib/sql.r")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

lk <- RunSQL('select * from Landkreis order by IdLandkreis;')

LandkreisTabelle <- function (i) {

data <- RunSQL(paste('call LandkreisTable(', lk[i,1] , ');') )

print( paste('--',lk[i,],'---'))

print(data)

# pdf( file = paste( 'png/LK/LK-', lk[i,1],"-",lk[i,2],"-1.pdf", sep="")
#     , paper = 'a4'
#     , height = 291/2.54, width = 210/2.54)
#
# b_size <- 12

png( paste( 'png/LK/LK-', lk[i,1],"-",lk[i,2],"-1.png", sep="")
, width = 1920
, height = 1080
)

b_size <- 36


vp <- viewport(
  x = 0.5
  , y = 0.5
  , width = 14
  , height = 10
)

tt <- ttheme_default(
  base_size = b_size
  , padding = unit(c(4,4), "mm")
  , core=list(
    fg_params=list( 
      hjust = 1
      , x = 0.99
    )
  )
)

title <- textGrob(paste( lk[i,1],"-",lk[i,2], sep=""),gp=gpar(fontsize=50))
footnote <- textGrob(paste('Stand:', heute), x=0, hjust=0,
                     gp=gpar( fontface="italic"))

table <- tableGrob(
  data
  , theme = tt
  , cols = colnames(data)
  , vp = vp
)

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

}

LandkreisDiagramAltersgruppe <- function (i) {
  
data <- RunSQL(paste('call LandkreisWoche(', lk[i,1] , ');') )

p <- ggplot(data, aes(fill=Altersgruppe, y=Anzahl, x=Kw)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle( paste( 'Corona: Fallzahlen im', lk[i,1], "-", lk[i,2])) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Kalenderwoche") +
  ylab("Fallzahlen pro Kw")

gg <- grid.arrange(p, ncol=1)

plot(gg)

ggsave(plot = gg, file = paste('png/LK/LK-', lk[i,1],"-",lk[i,2],"-2.png", sep="")
       , type = "cairo-png",  bg = "white"
       , width = 29.1, height = 21, units = "cm", dpi = 150)
}

LandkreisDiagram <- function (i) {
  
  data <- RunSQL(paste('call LandkreisWoche(', lk[i,1] , ');') )
  
  
  p <- ggplot(data, aes(y=Anzahl, x=Kw)) +
    geom_bar(stat="identity") +
    scale_fill_viridis(discrete = T) +
    ggtitle( paste( 'Corona: Fallzahlen im', lk[i,1], "-", lk[i,2])) +
    theme_ipsum() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("Kalenderwoche") +
    ylab("Fallzahlen pro Kw")
  
  gg <- grid.arrange(p, ncol=1)
  
  plot(gg)
  
  ggsave(plot = gg, file = paste('png/LK/LK-', lk[i,1],"-",lk[i,2],"-2.png", sep="")
         , type = "cairo-png",  bg = "white"
         , width = 29.1, height = 21, units = "cm", dpi = 150)
}

lastlk <- length(lk[,1])
# lastlk <- 5

for ( i in 1:lastlk) {

#  LandkreisTabelle(i)
  LandkreisDiagram(i)
  
}
