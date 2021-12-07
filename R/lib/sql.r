library(bit64)
library(RMariaDB)
library(data.table)

RunSQL <- function (
  SQL = 'select * from Faelle;'
  , prepare= c("set @i := 1;")) {
  
  rmariadb.settingsfile <- "/data/git/SARS-CoV-2-in-DE/SQL/rki.cnf"
  
  rmariadb.db <- "RKI"
  
  DB <- dbConnect(
    RMariaDB::MariaDB()
    , default.file=rmariadb.settingsfile
    , group=rmariadb.db
    , bigint="numeric"
  )
  for ( P in prepare ){
    dbExecute(DB, P)
  }
  rsQuery <- dbSendQuery(DB, SQL)
  dbRows<-dbFetch(rsQuery)

  # Clear the result.
  
  dbClearResult(rsQuery)
  
  dbDisconnect(DB)
  
  return(dbRows)
}

ExecSQL <- function (
  SQL 
) {
  
  rmariadb.settingsfile <- "/data/git/SARS-CoV-2-in-DE/SQL/rki.cnf"
  
  rmariadb.db <- "RKI"
  
  DB <- dbConnect(
    RMariaDB::MariaDB()
    , default.file=rmariadb.settingsfile
    , group=rmariadb.db
    , bigint="numeric"
  )
  
  count <- dbExecute(DB, SQL)

  dbDisconnect(DB)
  
  return (count)
  
}
