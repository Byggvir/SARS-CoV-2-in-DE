USE RKI;

DROP TABLE IF EXISTS FaelleAltersverteilung;

CREATE TABLE IF NOT EXISTS FaelleAltersverteilung (
      Jahr INT
    , Kw INT
    , Pw INT
    , AlterVon INT
    , AlterBis INT
    , Anzahl BIGINT(20) 
    , PRIMARY KEY(Jahr,Kw,AlterVon)
);

LOAD DATA LOCAL INFILE '/tmp/FaelleAltersverteilung.csv' 
    INTO TABLE FaelleAltersverteilung
    FIELDS TERMINATED BY ',' 
    IGNORE 1 ROWS;
