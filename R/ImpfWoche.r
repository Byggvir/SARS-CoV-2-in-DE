#!/usr/bin/env Rscript
#
#
# Script: RKI.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

MyScriptName <-"ImpfWoche"

require(data.table)
library(REST)
library(gridExtra)
library(grid)
library(lubridate)
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

fPNG <- "png/Impfungen_Wo_Bund.png"

f <- function (y) {
  
  return ( -log(y/(0.85-y)) )
  
}

sigmoid <- function (x,a=0,b=1) {
  
  return ( 0.85/(1+exp(a+b*x)))
  
}
plotsigmoid <- function (a, b, col, xlim, ylim) {
  
  par( new=TRUE )
  
  curve(  sigmoid(x,a,b)
          , from=xlim[1]
          , to=xlim[2]
          , lty = 2
          , lwd = 5
          , xlim = xlim
          , ylim = ylim
          , col = col
          , xlab = ""
          , ylab = ""
          , xaxt = "n"
          , yaxt = "n"
          , cex.axis = 2
          , cex.lab = 3
          , cex.main = 5      
  )
  
}

require(data.table)

source("R/lib/copyright.r")
source("R/lib/myfunctions.r")
source("R/lib/sql.r")

options( 
    digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
  )

AG <- c(60,100)

SQL <- paste('select sum(Insgesamt) as Anzahl from Bevoelkerung.DEU where Stichtag ="2019-12-31" and Age >= ', AG[1], ' and Age <= ', AG[2],';')
Bev <- RunSQL( SQL = SQL)

SQL <- paste('call ImpfungenAlter(',AG[1],',', AG[2],');')
ipw <- RunSQL(SQL = SQL)

write.csv(ipw,file="data/Impfungen_Wo_Bund.csv")

m <- length(ipw[,1])

reported <- ipw$Kw[m]

png(  fPNG
    , width = 1920
    , height = 1080
    )

par( 
    mar = c(10,10,10,10)
  , mfcol = c(1,1)
  )

colors <-c( "red", "yellow", "green", "blue", "black" )

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

df <- data.table(
  x = 1:m
  , y = cumsum(ipw[,5])/Bev[1,1]
)

CI <- 0.95

ra <- lm(f(y) ~ x, data=df)
ci <- confint(ra,level = CI)

a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
b <- c( ci[2,1], ra$coefficients[2] , ci[2,2])


labs <- paste(ipw$Jahr,ipw$Kw, sep='/')

xlim <- c(1,m)
ylim <- c(0,1)

pbp1 <- plot(
           df
         , xlim = xlim
         , ylim = ylim 
         , main = paste("Impfquote"
                        , " von", ipw$Jahr[1], 'Kw' , ipw$Kw[1]
                        ,  "bis", ipw$Jahr[m], 'Kw' , ipw$Kw[m]) 
         , sub = ""
         , xlab = ""
         , col = "lightblue"
         , ylab = "%"
         , las = 2
         , type = 'l'
         , lwd = 5
)


title ( sub = paste("Created:", heute ), line = 4, cex.sub = 1)

text( df$x
      , df$y
      , round(df$y*100,2)
  )

# plotsigmoid(
#   a[3]
#   , b[1]
#   , col = "green"
#   , xlim
#   , ylim
# )
# 
# plotsigmoid(
#   a[2]
#   , b[2]
#   , col = "black"
#   , xlim
#   , ylim
# )
# 
# plotsigmoid(
#   a[1]
#   , b[3]
#   , col = "red"
#   , xlim
#   , ylim
# )
# 
grid()

copyright()

dev.off()

# SQL <- 'call CasesPerWeekAgeGroup("A60-A79");'
# cpw <- RunSQL(SQL = SQL)
