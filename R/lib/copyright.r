library(data.table)
#
#
# Script: copyright.r
#
# Stand: 2020-10-21
# (c) 2020 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

copyrights <- data.table (
  shortname = c("CDC","JHU","RKI","TAr")
  , notice = c(
    " CDC; https://covid.cdc.gov/covid-data-tracker/"
    , "Daten: JHU CSSE COVID-19 Data https://github.com/CSSEGISandData/COVID-19"
    , "Robert Koch-Institut (RKI), dl-de/by-2-0"
    , "(c) 2020 by Thomas Arend, Rheinbach"
  )
)

copyright <- function ( holders = c( "RKI", "TAr" )
                       , ScriptName = NA  ) {

 
  h <- pmatch(holders,copyrights$shortname)
  
  mtext( text = "Quellen:"
         , side = 1
         , adj = 0
         , line  = 3
         , outer = FALSE 
  )
  for (i in 1:length(h)) {
    mtext( text = copyrights$notice[h[i]]
           , side = 1
           , adj = 0
           , line  = 3+i
           , outer = FALSE 
    )
  }
  
  if (is.na(ScriptName))
  { if (! is.na(MyScriptName))
    { ScriptName <- MyScriptName
  }
    }
  if (! is.na(ScriptName)) {
    mtext( text = paste("Script", ScriptName)
           , side = 1
           , adj = 0
           , line  = 4+length(h)
           , outer = FALSE 
  )
    }

}

copyright_text <- function (holders = c( "RKI", "TAr" )) {
  
  h <- pmatch(holders,copyrights$shortname)
  print(h)
  return(copyrights$notice[h])
}
