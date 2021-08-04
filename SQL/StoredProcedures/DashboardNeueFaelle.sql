use COVID19;

delimiter //

drop procedure if exists Neuefaelle //

create procedure Neuefaelle ()
begin

    SELECT distinct A.Meldedatum as Meldedatum, weekday(A.Meldedatum) as WTag,B.Infizierte as Infizierte
    FROM RKIFaelle  AS A
    LEFT JOIN (SELECT Meldedatum, sum(AnzahlFall) AS Infizierte
    FROM RKIFaelle  AS C WHERE NeuerFall <> 0 GROUP BY Meldedatum) AS B
    ON A.Meldedatum=B.Meldedatum where A.Meldedatum > CURRENT_DATE() - 14 ;

end
//

drop procedure if exists FallzahlenGestern //

create procedure FallzahlenGestern ()
begin

  set @MD := (select max(Meldedatum) from RKIFaelle where NeuerFall <> 0);
  SELECT
    @MD  AS Meldedatum  
    ,  max(case when F.Outcome ='Cases' then F.Counts else 0 end )  AS Cases
    , max(case when F.Outcome ='Deaths' then F.Counts else 0 end )  AS Deaths    
  from (
    SELECT 'Cases'  AS Outcome, sum(AnzahlFall) AS Counts
    FROM RKIFaelle  AS C WHERE NeuerFall <> 0
    UNION ALL
    SELECT 'Deaths'  AS Outcome, sum(AnzahlTodesfall) AS Counts
    FROM RKIFaelle  AS C WHERE NeuerTodesfall <> 0
  )  AS F
  ;

end
//
drop procedure if exists UpdateYesterday //

create procedure UpdateYesterday ()
MODIFIES SQL DATA
begin

  SET @MD := (select max(Meldedatum) from RKIFaelle);
  INSERT INTO rki SELECT
    @MD 
    , max(case when F.Outcome ='Cases' then F.Counts else 0 end )
    , max(case when F.Outcome ='Deaths' then F.Counts else 0 end )    
  FROM (
    SELECT
        'Cases'  AS Outcome
        , sum(AnzahlFall) AS Counts
    FROM RKIFaelle  AS C WHERE NeuerFall > -1
    UNION ALL
    SELECT 
        'Deaths'  AS Outcome
        , sum(AnzahlTodesfall) AS Counts
    FROM RKIFaelle  AS C 
    WHERE NeuerTodesfall > -1
  ) AS F
  ;

end
//

delimiter ;
 
