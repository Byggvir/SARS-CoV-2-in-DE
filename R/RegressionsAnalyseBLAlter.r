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

SQL <- 'select distinct Altersgruppe from Faelle where Altersgruppe <> "unbekan";'
Altersgruppen <- RunSQL(SQL)

SQL <- 'select * from Bundesland where IdBundesland <> 0 order by IdBundesland;'
Bundesland <- RunSQL(SQL = SQL)

# Function execute a regression analysis 

CI <- 0.95

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
        , F.AnzahlFall / W.AnzahlFall as AnteilAnWoche
    from FaelleProTag as F
    join
        FaelleProWoche as W  
    on  
        week(F.Meldedatum,3) = W.Kw 
        and W.Jahr = (case when week(F.Meldedatum,3) = 53 then 2020 else year(F.Meldedatum) end)
    ) as T
    
where 
    T.Meldedatum > "2020-05-03" 
    and T.Meldedatum < adddate("', ThisDay, '" ,-weekday( "', ThisDay, '" )) 
group by WTag;'
        , sep= ''
  )

Kor <- RunSQL(SQLWTag)

# print(Kor)

#---
# 
# Regressionsanalyse ??ber
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
  , IdBundesland
  , Altersgruppe
) {

SQL <- paste (
  'select 
      Meldedatum as Meldedatum
      , datediff(Meldedatum,"', ThisDate, '") + ', DaysBack, ' as Day 
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
  , ') and IdLandkreis div 1000 = '
  , IdBundesland 
  , ' and Altersgruppe = "'
  , Altersgruppe
  , '" group by Meldedatum;
'
  , sep=''
  )
  
  data <- RunSQL( SQL=SQL, prepare = "set @i:=-1;" )
  
  FromTo <- data$Day[ data$Meldedatum <= ThisDate ]
  
  y <- data$AnzahlFall[data$Meldedatum <= ThisDate]
  s <- y > 0
  
  ra <- lm(log(y[s]) ~ FromTo[s])
  ci <- confint(ra,level = CI)
  
  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <-  c( ci[2,1], ra$coefficients[2] , ci[2,2])
  UpdateSQL <- paste ('insert into RZahl values ('
                      , IdBundesland
                      ,',"'
                      , Altersgruppe
                      , '","'
                      , ThisDate
                      , '",'
                      , DaysBack 
                      , ','
                      , exp(4*b[2])
                      , ','
                      , exp(4*b[1])
                      , ','
                      , exp(4*b[3])
                      , ') ON DUPLICATE KEY UPDATE R = '
                      , exp(4*b[2])
                      , ', Rlow = '
                      , exp(4*b[1])
                      , ', Rhigh ='
                      , exp(4*b[3])
                      , ' ;' 
                      , sep=''
  )
  
  ExecSQL(UpdateSQL)
  
  xlim <- c(0,DaysBack+DaysAhead)
  
  ylim <- limbounds(
    c( exp(a)
     , exp(a+b*as.numeric(DaysBack+DaysAhead))
    ) 
  )
  
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
  zr <- data$Meldedatum <= ThisDate

  xlim = c(data$Meldedatum[1],data$Meldedatum[1]+1+DaysBack+DaysAhead)
  
  plot(  data$Meldedatum[zr]
       , data$AnzahlFall[zr]
       , main = ""
       , sub = ""
       , xlab = paste("Vom", ThisDate - DaysBack, "bis", ThisDate )
       , ylab = "Anzahl"
       , ylim = ylim
       , type = "l"
       , lwd = 3
       , xlim = xlim
       , col = "black"
       , cex.axis = 4

  )

  t <- title ( 
    main = "Bestimmung der R-Zahl mittels Regressionsanalyse" 
    , cex.main = 4
  )
  
  t <- title ( 
    sub = paste(Bundesland[IdBundesland,2], Altersgruppe)
    , cex.sub = 3
    , line = 6
  )

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
  
  text ( ThisDate - 1
         , ylim[2]/2
         , as.character(ThisDate)
         , adj = 1
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
             , "Fallzahl au??erhalb "
             , "model Fallzahl"
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
           , cex = 1
           , inset = 0.05
           , lty = c(1,1,4,1,1,1)
  )
  
  legend(
    "top"
    , inset = 0.02
    , title = paste( "T??gliche Steigerung CI  ",  CI * 100, "%", sep="")
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
    , cex = 4)

  return(ra)
  
}

# Wann <- as.Date("2020-04-01")
FerienEnde <- c(
  
    "2021-07-31" # "2020-08-08"
  , "2021-08-04" # "2020-08-05"
  , "2020-08-26"
  , "2020-08-26"
  , "2020-08-11"
  , "2020-08-14"
  , "2020-08-14"
  , "2020-09-12"
  , "2020-09-07"
  , "2020-08-14"
  , "2020-08-07"
  , "2020-08-08"
  , "2021-07-31" # "2020-08-01"
  , "2020-08-28"
  , "2020-08-26"
  , "2020-08-29"
  
)
options( 
  digits=4
)
for (j in c(7)) {
  for (i in c(20,41)) {
    for (b in Bundesland[,1]) { 
      
      png( paste( "png/Prognosen/"
                  , "Alter_"
                  , Bundesland[b,2]
                  , "_"
                  , as.character(ThisDay)
                  , "_"
                  , i
                  , "_"
                  , j
                  , ".png"
                  , sep = ""
      )
      , width = 3840
      , height = 2160
      )
      
      par (   mar = c(10,5,10,5)
              , mfrow = c(2,3)
              , bg = "white")

      for (a in Altersgruppen[,1]) {
      
      ra <- regression_analysis (
          ThisDate = ThisDay 
        #  ThisDate = as.Date(FerienEnde[b])
        , DaysBack = i
        , DaysAhead = j
        , IdBundesland = b
        , Altersgruppe = a
      )
      print(RZahl(ra$coefficients[2]))
      
      } # End a
      
      copyright(c("RKI","TA"))
      
      dev.off()
      
    } # End for b
  } # End for i
} # End for j
