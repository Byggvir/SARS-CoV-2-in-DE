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
  digits=5
  , scipen=5
  , Outdec="."
  , max.print = 3000
)

SQL <- 'select distinct Altersgruppe, 0 as R from Faelle where Altersgruppe<>"unbekan";'
Altersgruppen <- RunSQL(SQL)

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
  , Altersgruppe
)
{
  
# Berechnen des durchschnittlichen Anteils eines Wochentages an den Meldungen
  
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
          , ThisDate
          , '",-weekday("'
          , ThisDate
          , '"))
  group by Kw 
  ) as W 
on 
  week(F.Meldedatum,3) = W.Kw
where Meldedatum >'
          , '"2020-05-03"'
          , 'and Meldedatum < adddate("'
          , ThisDate
          , '",-weekday("'
          , ThisDate
          , '"))
group by F.Meldedatum
) as T 
group by WTag;'
          , sep= ' '
    )
  
  Kor <- RunSQL(SQLWTag)
  # print(Kor)
  
  SQL <- paste (
  'select 
      Meldedatum as Meldedatum
      , (@i:=@i+1) as Day
      , week(Meldedatum,3) as Kw
      , dayofweek(Meldedatum) as WTag
      , sum(AnzahlFall) as AnzahlFall
      , sum(AnzahlTodesfall) as AnzahlTodesfall
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
  , ') and Altersgruppe = "'
  , Altersgruppe 
  , '" group by Meldedatum;
'
  , sep=''
  )

  data <- RunSQL( SQL=SQL, prepare = "set @i:=-1;" )
  
  write.csv(data,
            file = paste(
              'data/Prognose_'
              , Altersgruppe
              , '_'
              , ThisDate
              , '_'
              , DaysBack
              , '_'
              , DaysAhead
              , '.csv'
              , sep=''
            )
  )
  
  FromTo <- 0:DaysBack 
  y <- data$AnzahlFall[FromTo+1]
  s <- y > 0

  ra <- lm(log(y[s]) ~ FromTo[s])
  ci <- confint(ra,level = CI)

  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <-  c( ci[2,1], ra$coefficients[2] , ci[2,2])
  
  xlim <- c(0,DaysBack+DaysAhead)
  
  ylim <- limbounds(
    c( exp(a)
     , exp(a+log(1.5)/7*sign(b)*(DaysBack+DaysAhead))
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
  
  # png( paste( "png/Prognose"
  #             , "_"
  #             , Altersgruppe
  #             , "_"
  #             , as.character(ThisDate)
  #             , "_"
  #             , DaysBack
  #             , "_"
  #             , DaysAhead
  #             , ".png"
  #             , sep = ""
  # )
  # , width = 1920
  # , height = 1080
  # )
  # 
  # par (   mar = c(10,5,10,5) 
  #         , bg = "white")

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
       , cex.axis = 1.5

  )

  t <- title ( 
      main = paste("R-Zahl", ThisDate)
    , cex.main = 4
  )
  t <- title ( 
    sub = paste("Altersgruppe", Altersgruppe)
    , cex.sub = 3
    , line = 6
  )

  zr <- data$Meldedatum <= ThisDate
  
  
  # lines ( data$Meldedatum[zr]
  #         , data$AnzahlFall[zr]
  #         , col = "black"
  #         , lwd = 3
  # )
  # 
  
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
         , 0
         , as.character(ThisDate)
         , adj = 1
         , cex = 2
         , col = "blue"
         , las = 3

  )

  grid()
  
  plotregression(a, b, xlim= c(0, DaysBack + DaysAhead + 1), ylim = ylim )
  
  lr <- ifelse (b[2] > 0 ,"left", "right")
  
  legend ( paste("top",lr,sep='')
           , legend = c( 
             "Fallzahl innerhalb des Intervalls der RA"
             , "Fallzahl außerhalb "
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
           , cex = 2
           , inset = 0.05
           , lty = c(1,1,4,1,1,1)
  )
  
  legend(
    lr
    , inset = 0.02
    , title = paste( "Tägliche Steigerung CI  ",  CI * 100, "%", sep="" )
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
    , lty = 1 
    , lwd = 3
    , cex = 3
    )
  
  par(new=FALSE)
  return(ra)
  
} # End Regressionsanalyse


for (j in c(0)) {
  
  for (i in c(41)) {
  
    png( paste( "png/Prognose"
                , "_Alter_"
                , as.character(ThisDay)
                , "_"
                , j
                , "_"
                , i
                , ".png"
                , sep = ""
    )
    , width = 3840
    , height = 2160
    )
    
    par (   mar = c(10,5,10,5) 
            , bg = "white"
            , mfrow = c(2,3)
        )
    
    for (AG in Altersgruppen[,1]) {
      
      R <- regression_analysis (
          ThisDate = ThisDay 
        , DaysBack = i
        , DaysAhead = j
        , Altersgruppe = AG
      )
      
      Altersgruppen[Altersgruppen[,1]==AG,2] <- RZahl(R$coefficients[2])
      par (new=FALSE)
      
    } # End for AG
    copyright()
    dev.off()
    
    print(Altersgruppen)
    
    } # End for i
} # End for j

print((1-Altersgruppen[,2]/Altersgruppen[1,2])/0.50)

