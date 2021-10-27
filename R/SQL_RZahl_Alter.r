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
library(lubridate)
library(tidyverse)

# Set Working directory to git root

if (rstudioapi::isAvailable()){

    # When executed in RStudio
    SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
} else {
  
    #  When executing on command line 
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

SQL <- 'select distinct Meldedatum from Faelle where Meldedatum >= "2021-01-01" order by Meldedatum;'
Meldedatum <- RunSQL(SQL = SQL)

# Function execute a regression analysis 

CI <- 0.95

# Berechnen des durchschnittlichen Anteils eines Wochentages an den Meldungen

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
  
  SQL <- paste (
  'select 
      Meldedatum as Meldedatum
      , datediff(Meldedatum,"', ThisDate, '") + ', DaysBack, ' as Day 
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
  
  FromTo <- data$Day[ data$Meldedatum <= ThisDate ]
  
  y <- data$AnzahlFall[data$Meldedatum <= ThisDate]
  s <- y > 0
  
  ra <- lm(log(y[s]) ~ FromTo[s])
  ci <- confint(ra,level = CI)

  a <- c( ci[1,1], ra$coefficients[1] , ci[1,2])
  b <-  c( ci[2,1], ra$coefficients[2] , ci[2,2])

  UpdateSQL <- paste ('insert into RZahl values (0,"'
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


  return(ra)
  
} # End Regressionsanalyse

for (M in c(ThisDay) ) { # Meldedatum[,1]) {
  
  for (j in c(0)) {
  
    for (i in c(20,41)) {
  
      for (AG in Altersgruppen[,1]) {
      
        R <- regression_analysis (
          ThisDate = as.Date(M,format="%F",origin = "1970-01-01") 
          , DaysBack = i
          , DaysAhead = j
          , Altersgruppe = AG
        )
      
      } # End for AG

    } # End for i
  } # End for j
} # End Meldedatum
