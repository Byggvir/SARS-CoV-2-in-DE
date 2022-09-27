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
#library(REST)
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


options( 
  digits=5
  , scipen=5
  , Outdec="."
  , max.print = 3000
)

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

SQL <- 'select distinct Meldedatum from Faelle where Meldedatum >= "2021-01-01" order by Meldedatum;'
Meldedatum <- RunSQL(SQL = SQL)

args = commandArgs(trailingOnly=TRUE)

if (length(args) == 0) {
  ForDays <- today - 2
  
} else if (length(args) == 1) {
  if ( args[1] == '-a' ) {
    ForDays <- Meldedatum[,1]
  }
  else {
    ForDays <- as.Date(args[1])
  }
} else if (length(args) >= 2){
  ForDays <- as.Date(args[1])
}


SQL <- 'select distinct * from Bundesland order by IdBundesland;'
Bundesland <- RunSQL(SQL = SQL)

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
  , IdBundesland
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
  , ' group by Meldedatum;
'
  , sep=''
  )
  data <- RunSQL( SQL=SQL, prepare = "set @i:=-1;" )
  
  if (nrow(data) > 0.6 * DaysBack) {
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
                      , 'A0+'
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
  
  print(RZahl(ra$coefficients[2]))
  
  return(ra)
  }
  else return (NA)

}

for (M in ForDays) {
  
  for (i in c(10,20,41)) {
    
    for (b in Bundesland[,1]) { 

      ra <- regression_analysis (
          ThisDate = as.Date(M,format="%F",origin = "1970-01-01") 
        , DaysBack = i
        , DaysAhead = 0
        , IdBundesland = b
      )
      
    } # End for b
  
  } # End for i

} # End Meldedatum
