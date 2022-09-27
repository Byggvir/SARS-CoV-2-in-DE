use RKI;

drop table if exists Hospitalisierung;
create table if not exists Hospitalisierung
    ( Datum	date default NULL
    , Bundesland char(32) default NULL
    , Bundesland_Id int(11) default 0
    , Altersgruppe char(8) default "unbekant"
    , Faelle bigint(20) default 0
    , Inzidenz double default 0
    , primary key (Datum, Bundesland_Id, Altersgruppe)
    );
    
LOAD DATA LOCAL 
INFILE '/tmp/Hospitalisierung.csv'      
INTO TABLE `Hospitalisierung`
FIELDS TERMINATED BY ','
IGNORE 1 ROWS;


