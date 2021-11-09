USE RKI;

DROP TABLE IF EXISTS `ImpfD`;

CREATE TABLE `ImpfD` (
-- Montag der Kalenderwoche des Berichtes
  `Woche` date DEFAULT NULL,
-- Alter Von bis 5-11, 12-17, 18-59, 60+ = 60 - 100
  `AlterVon` int(11) DEFAULT NULL,
  `AlterBis` int(11) DEFAULT NULL,
-- Outcome: Symptomatisch = S, Hospitalisiert = H , Intensivstation = I , Gestorben = T
  `IdOutcome` char(1) DEFAULT NULL,
-- Gruppe: A = Alle, G = Geimpft , U = Ungeimpft
  `IdGruppe` char(1) DEFAULT NULL,
-- Kumulierte Anzahl ab Kw 5 2021
  `AnzahlKum` double DEFAULT NULL,
-- Anzahl der letzten 4 Wochen
  `Anzahl4W` double DEFAULT NULL,
  PRIMARY KEY (`Woche`,`AlterVon`,`IdOutcome`,`IdGruppe`),
  KEY `AlterBis` (`AlterBis`),
  KEY `IdOutcome` (`IdOutcome`),
  KEY `IdGruppe` (`IdGruppe`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 ;

DROP TABLE IF EXISTS `ImpfDOutcome`;

CREATE TABLE `ImpfDOutcome` (
  `IdOutcome` char(1) DEFAULT NULL,
  `Outcome` char(20) DEFAULT NULL,
  PRIMARY KEY (`IdOutcome`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 ;

DROP TABLE IF EXISTS `ImpfDGruppe`;

CREATE TABLE `ImpfDGruppe` (
  `IdGruppe` char(1) DEFAULT NULL,
  `Gruppe` char(20) DEFAULT NULL,
  PRIMARY KEY (`IdGruppe`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 ;

INSERT INTO `ImpfDOutcome` VALUES 
      ( "Q", "Impfquote" )  
    , ( "S", "Symptomatisch" )
    , ( "H", "Hospitalisiert" )
    , ( "I", "Intensivstation" )
    , ( "T", "Gestorben" )
    ;

INSERT INTO `ImpfDGruppe` VALUES
      ( "A", "Alle" )
    , ( "G", "Geimpft" )
    , ( "U", "Ungeimpft" )
    ;

