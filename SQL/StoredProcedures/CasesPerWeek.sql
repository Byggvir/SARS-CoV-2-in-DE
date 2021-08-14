USE RKI;

delimiter //

DROP FUNCTION IF EXISTS is_westbl //

CREATE FUNCTION is_westbl (n INT , we BOOL) returns BOOL DETERMINISTIC
BEGIN
  
  set @r := 0;
  if we then set @r := (n < 12);
  else set @r := (n > 11);
  end if;
  return @r;
  
end
//

DROP FUNCTION IF EXISTS BLWestEast //

CREATE FUNCTION BLWestEast (n INT )
RETURNS varchar(20) DETERMINISTIC

BEGIN

  DECLARE dir varchar(20);
  
  if n < 12 then set dir = 'West';
  else set dir = 'Ost';
  end if;
  return dir;
  
END
//

DROP PROCEDURE IF EXISTS CasesPerWeek //

CREATE PROCEDURE CasesPerWeek ()
BEGIN

   SELECT 
      ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) AS Kw
    , sum(AnzahlFall) AS AnzahlFall
    , sum(AnzahlTodesfall) AS AnzahlTodesfall
    FROM Faelle
    WHERE ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) > 8
    GROUP BY Kw ;
end
//

DROP PROCEDURE IF EXISTS CasesPerWeekAgeGroup //

CREATE PROCEDURE CasesPerWeekAgeGroup (AgeGroup CHAR(8))
BEGIN

   SELECT 
      ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) AS Kw
    , sum(AnzahlFall) AS AnzahlFall
    , sum(AnzahlTodesfall) AS AnzahlTodesfall
    FROM Faelle
    WHERE ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) > 8
    and Altersgruppe = AgeGroup
    GROUP BY Kw ;
end
//


DROP PROCEDURE IF EXISTS CasesPerWeekBL //

CREATE PROCEDURE CasesPerWeekBL (IdBL INT)
BEGIN

   SELECT 
      IdLandkreis div 1000 AS BL
    , ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) AS Kw
    , sum(AnzahlFall) AS AnzahlFall
    , sum(AnzahlTodesfall) AS AnzahlTodesfall
    FROM Faelle
    WHERE IdLandkreis div 1000 = IdBL
    and ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) > 8
    GROUP BY BL, ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) ;
end
//

DROP PROCEDURE IF EXISTS CasesPerWeekBL21 //

CREATE PROCEDURE CasesPerWeekBL21 (IdBL INT)
BEGIN

   SELECT 
      IdLandkreis div 1000 AS BL
    , week(Meldedatum,3) AS Kw
    , sum(AnzahlFall) AS AnzahlFall
    , sum(AnzahlTodesfall) AS AnzahlTodesfall
    FROM Faelle
    WHERE IdLandkreis div 1000 = IdBL
    and Meldedatum > "2021-01-03"
    and week(Meldedatum,3)
    GROUP BY IdBL, Kw ;
end
//


DROP PROCEDURE IF EXISTS CasesPerWeekBLWE //

CREATE PROCEDURE CasesPerWeekBLWE (West BOOL)
BEGIN

   SELECT 
       ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) AS Kw
      , sum(AnzahlFall) AS AnzahlFall
      , sum(AnzahlTodesfall) AS AnzahlTodesfall
    FROM Faelle
    WHERE is_westbl(IdLandkreis div 1000,West)
    and  ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) > 8
    GROUP BY  ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) 
    ;
END
//

DROP PROCEDURE IF EXISTS CasesPerWeekWE //

CREATE PROCEDURE CasesPerWeekWE ()
BEGIN

    SELECT 
        ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) AS Kw
        , BLWestEast(IdLandkreis div 1000) AS Bundesland
        , sum(AnzahlFall) AS AnzahlFall
        , sum(AnzahlTodesfall) AS AnzahlTodesfall
        , round(sum(AnzahlTodesfall) / sum(AnzahlFall)*100,1) AS CFR
    FROM Faelle
    WHERE ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) > 9
    GROUP BY BLWestEast(IdLandkreis div 1000), Kw
--     UNION
--     SELECT 
--         ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) AS Kw
--         , 'Bund' AS Bundesland
--         , sum(AnzahlFall) AS Cases
--         , sum(AnzahlTodesfall) AS Deaths
--         , round(sum(AnzahlTodesfall) / sum(AnzahlFall)*100,1) AS CFR
--     FROM Faelle
--     WHERE ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) > 9
--     GROUP BY Kw
    
    
  ;
END
//

DROP PROCEDURE IF EXISTS MinMaxCasesPerWeek //

CREATE PROCEDURE MinMaxCasesPerWeek (minW INT, maxW INT)
BEGIN
    
    DROP TABLE IF EXISTS cpw;
    
    CREATE TEMPORARY TABLE cpw ( IdBundesland INT, Kw INT, Cases BIGINT, Deaths BIGINT, PRIMARY KEY (IdBundesland,Kw) )
        SELECT 
            IdLandkreis div 1000 AS IdBundesland
            , ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) AS Kw
            , sum(AnzahlFall) AS AnzahlFall
            , sum(AnzahlTodesfall) AS AnzahlTodesfall
        FROM Faelle
        WHERE ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end )
        GROUP BY IdBundesland, Kw 
    ;
    
    DROP TABLE IF EXISTS minmaxcpw;
    
    CREATE TEMPORARY TABLE minmaxcpw ( 
        IdBundesland INT
        , minCases BIGINT
        , minDeaths BIGINT
        , maxCases BIGINT
        , maxDeaths BIGINT
        , PRIMARY KEY (IdBundesland) )
    SELECT 
        IdBundesland AS IdBundesland
        , min(Cases) AS minCases
        , min(Deaths) AS minDeaths
        , max(Cases) AS maxCases
        , max(Deaths) AS maxDeaths
    FROM cpw
    WHERE Kw >= minW and Kw <= maxW
    GROUP BY IdBundesland
    ;
    
    SELECT 
        M.IdBundesland AS BL
        , Bundesland AS Bundesland
        , M.minCases AS 'Min'
        , max(W1.Kw) AS 'minKw'
        , M.maxCases AS 'Max'
        , max(W2.Kw) AS 'maxKw'
        , max(W2.Kw)-max(W1.Kw) AS 'Wochen'
        , round(exp(log(M.maxCases / M.minCases) / (max(W2.Kw)-max(W1.Kw))),4)  AS Ratio
    FROM minmaxcpw AS M
    JOIN cpw AS W1
        on M.IdBundesland = W1.IdBundesland
        and M.minCases = W1.Cases
    JOIN cpw AS W2
        on M.IdBundesland = W2.IdBundesland
        and M.maxCases = W2.Cases
    JOIN Bundesland AS B
        on
            M.IdBundesland = B.IdBundesland
    GROUP BY M.IdBundesland
    ;
end
//

DROP PROCEDURE IF EXISTS MinMaxCasesPerWeekAgeGroup //

CREATE PROCEDURE MinMaxCasesPerWeekAgeGroup (minW INT, maxW INT)
BEGIN
    
    DROP TABLE IF EXISTS minmaxAG;
    
    CREATE TEMPORARY TABLE minmaxAG ( 
        AgeGroup INT
        , minCount INT
        , maxCount INT
        , PRIMARY KEY (AgeGroup) )
    SELECT 
        AgeGroup AS AgeGroup
        , min(Count) AS minCount
        , max(Count) AS maxCount
    FROM RKI_CasesByAge
    WHERE (Jahr-2020)*53 + Kw >= minW and (Jahr-2020)*53 + Kw  <= maxW
    GROUP BY AgeGroup
    ;
    
    SELECT 
        M.AgeGroup AS AgeGroup
        , M.minCount AS 'Min'
        , max(W1.Kw) AS 'minKw'
        , M.maxCount AS 'Max'
        , max(W2.Kw) AS 'maxKw'
        , max(W2.Kw)-max(W1.Kw) AS 'Wochen'
        , round(exp(log(M.maxCount / M.minCount) / (max(W2.Kw)-max(W1.Kw))),4)  AS Ratio
    FROM minmaxAG AS M
    JOIN RKI_CasesByAge AS W1
        on M.AgeGroup = W1.AgeGroup
        and M.minCount = W1.Count
    JOIN RKI_CasesByAge AS W2
        on M.AgeGroup = W2.AgeGroup
        and M.maxCount = W2.Count
    GROUP BY M.AgeGroup
    ;
end
//
delimiter ;
