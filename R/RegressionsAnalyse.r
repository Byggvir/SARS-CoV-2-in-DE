#!/usr/bin/env Rscript
#
#
# Script: RKI_RegressionAnalysis.r
#
# Stand: 2021-02-22
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

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

MyScriptName <- "RegressionsAnalyse"

source("R/lib/myfunctions.r")
source("R/lib/copyright.r")
source("R/lib/ta_regressionanalysis.r")
source("R/lib/sql.r")

today <- Sys.Date()
heute <- format(today, "%d %b %Y")


args = commandArgs(trailingOnly=TRUE)

if (length(args) == 0) {
  ThisDay <- today - 2
  
} else if (length(args) == 1) {
  ThisDay <- as.Date(args[1])
  
} else if (length(args) >= 2){
  ThisDay <- as.Date(args[1])
}

print(ThisDay)

                     
options( 
  digits=7
  , scipen=7
  , Outdec="."
  , max.print = 3000
)

SQLWTag <- 
  paste('
select 
 WTag
 , avg(AnteilAnWoche) as AnteilAnWoche
 , avg(AnteilAnWoche) * 7 as KorFaktor
 , stddev(AnteilAnWoche) as StdAbweichung
 from (
select 
  F.Meldedatum
  , weekday(F.Meldedatum) as WTag
  , sum(F.AnzahlFall) / W.FallWoche as AnteilAnWoche
from Faelle F 
join ( 
  select 
    week(Meldedatum,3) as Kw
    , sum(AnzahlFall) as FallWoche
  from Faelle 
  where Meldedatum >'
        , '"2020-05-03"'
        , 'and Meldedatum < adddate("'
        , ThisDay
        , '",-weekday("'
        , ThisDay
        , '"))
  group by Kw 
  ) as W 
on 
  week(F.Meldedatum,3) = W.Kw
where Meldedatum >'
        , '"2020-05-03"'
        , 'and Meldedatum < adddate("'
        , ThisDay
        , '",-weekday("'
        , ThisDay
        , '"))
group by F.Meldedatum
) as T 
group by WTag;'
        , sep= ' '
  )

Kor <- RunSQL(SQLWTag)

# Function execute a regression analysis 

CI <- 0.95

#---
# 
# Regressionsanalyse über
#
#   * bis zum angegebenen Datum
#   * die vergangenen Tage
#   * und Prognose
#
#---

regression_analysis <- function (
  ThisDate
  , DaysBack
  , DaysAhead
) {

  SQL <- paste (
  'select 
      Meldedatum as Meldedatum
      , week(Meldedatum,3) as Kw
      , dayofweek(Meldedatum) as WTag
      , sum(AnzahlFall) as AnzahlFall
  from Faelle
where 
  Meldedatum >= adddate("'
  , ThisDate
  , '",'
  , - DaysBack
  , ') and Meldedatum <= adddate("'
  , ThisDate
  ,'",'
  , DaysAhead
  , ') 
  group by Meldedatum
  order by Meldedatum;
'
  , sep=''
  )
  
  data <- RunSQL( SQL=SQL, prepare = "set @i:=-1;" )
  
  FromTo <- 0:(DaysBack)
  
  ra <- lm(log(as.numeric(data$AnzahlFall[FromTo + 1])) ~ FromTo)
  ci <- confint(ra,level = CI)
  
  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <-  c( ci[2,1], ra$coefficients[2] , ci[2,2])
  
  print(RZahl(b))
  
  FromDay <- ThisDate-days(DaysBack)
  ToDay <- ThisDate+days(DaysAhead)
  
  Tage <- as.Date.numeric(FromDay:ToDay,"1970-01-01")
  
  sTage <- 0:(DaysBack+DaysAhead)
  
  PrognoseTab <- data.table (
    Date = Tage
    , Day = sTage
    , WTag = Wochentage[wday(Tage,week_start = 1)]
    , WTag2 = wday(Tage, week_start = 1)
    , Kor = Kor[wday(Tage, week_start = 1),3]
    , assumed = round(exp(a[2] + b[2] * sTage) * Kor[wday(Tage, week_start = 1),3])
    , lower = round(exp(a[1] + b[1] * sTage) * Kor[wday(Tage, week_start = 1),3])
    , upper = round(exp(a[3] + b[3] * sTage) * Kor[wday(Tage, week_start = 1),3])
  )
  
  # print(PrognoseTab)
  
  png( paste( "png/Prognose"
              , "_"
              , as.character(ThisDate)
              , "_"
              , DaysBack
              , "_"
              , DaysAhead
              , ".png"
              , sep = ""
  )
  , width = 1920
  , height = 1080
  )

  par (   mar = c(10,5,10,5) 
          , bg = "white")
  
  zr <- data$Meldedatum <= ThisDate
  
  xlim = c(data$Meldedatum[1],data$Meldedatum[1]+days(1+DaysBack+DaysAhead))
  ylim <- limbounds(
    c( exp(a)
       , exp(a+b*as.numeric(DaysBack+DaysAhead))
    )
  )

  plot(  data$Meldedatum[zr]
       , data$AnzahlFall[zr]
       , main = ""
       , sub = paste("Vom", ThisDate - DaysBack, "bis", ThisDate )
       , xlab = "Datum"
       , ylab = "Anzahl"
       , ylim = ylim
       , type = "l"
       , lwd = 3
       , xlim = xlim
       , col = "black"

  )
  
  
  t <- title ( 
    main = paste( "Tägliche Fallzahl in DEU mit Prognose bis", as.character(ThisDate+ DaysAhead)) 
    , cex.main = 4
  )
  copyright(c("RKI","TA"))
  
  lines ( PrognoseTab$Date
          , PrognoseTab$assumed
          , col = "blue"
          , lwd = 3
          , lty = 4
  )
  
  zr <- data$Meldedatum >= ThisDate
  
  lines ( data$Meldedatum[zr]
          , data$AnzahlFall[zr]
          , col = "gray"
          , lwd = 3
  )
  
  abline(
    v = ThisDate
    , col = "blue"
    , lwd = 3
    , lty = 4
  )
  
  text ( ThisDate -1
         , 0
         , "Regressionsanalyse"
         , adj = 1 
         , cex = 2
         , col = "blue"
  )
  
  text ( ThisDate + 1
         , 0
         , "Prognose"
         , adj = 0
         , cex = 2 
         , col = "blue"
  )
  
  text ( ThisDate + 1
         , ylim[2]/2
         , as.character(ThisDate)
         , adj = 0
         , cex = 3 
         , col = "blue"
         , las = 3

  )

  grid()
  
  
  plotregression(a, b, xlim= c(0, DaysBack + DaysAhead +1 ), ylim = ylim )
  
  lr <- ifelse (b[2] > 0 ,"topleft", "topright")
  
  legend ( lr
           , legend = c( 
             "Fallzahl innerhalb des Intervalls der RA"
             , "Fallzahl außerhalb "
             , "Model Fallzahl"
             , paste("Obere Grenze des CI ",  CI * 100, "%", sep="")
             , "Mittelwert"
             , paste("Untere Grenze des CI ",  CI * 100, "%", sep="")
           )
           , col = c(
             "black"
             , "gray"
             , "blue"
             , "red"
             , "orange"
             , "green"
           )
           , lwd = 2
           , cex = 2
           , inset = 0.05
           , lty = c(1,1,4,1,1,1)
  )
  
  legend(
    "top"
    , inset = 0.02
    , title = paste( "Tägliche Steigerung CI  ",  CI * 100, "%", sep="")
    , legend = c( 
      paste(round((exp(ci[2,1])-1)*100,2),"% / R =", RZahl(ci[2,1]), ", 2x in", round(log(2)/ci[2,1],1), "Tagen")
      , paste(round((exp(ra$coefficients[2])-1)*100,2),"% / R =", RZahl(ra$coefficients[2]), ", 2x in", round(log(2)/ra$coefficients[2],1), "Tagen")
      , paste(round((exp(ci[2,2])-1)*100,2),"% / R =",RZahl(ci[2,2]), ", 2x in", round(log(2)/ci[2,2],1), "Tagen")
    )
    , col = c(
        "green"
      , "orange"
      , "red"
    )
    , lty = 3 
    , lwd = 3
    , cex = 2)

  dev.off()
  return(ra)
  
}

# Wann <- as.Date("2020-04-01")

for (j in c(21)) {
for (i in c(20,27,34,41)) {
    
  ra2 <- regression_analysis (
      ThisDate = ThisDay
    , DaysBack = i
    , DaysAhead = j
   
  )
  
} # End for i
} # End for j
