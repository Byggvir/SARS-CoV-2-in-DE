library(bit64)
library(RMariaDB)
library(data.table)

RunSQL <- function (
  SQL = 'select * from Faelle;'
  , prepare="set @i := 1;") {
  
  rmariadb.settingsfile <- "/data/git/SARS-CoV-2-in-DE/SQL/rki.cnf"
  
  rmariadb.db <- "RKI"
  
  DB <- dbConnect(
    RMariaDB::MariaDB()
    , default.file=rmariadb.settingsfile
    , group=rmariadb.db
    , bigint="numeric"
  )
  dbExecute(DB, prepare)
  rsQuery <- dbSendQuery(DB, SQL)
  dbRows<-dbFetch(rsQuery)

  # Clear the result.
  
  dbClearResult(rsQuery)
  
  dbDisconnect(DB)
  
  return(dbRows)
}
