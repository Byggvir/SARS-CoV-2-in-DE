use RKI;

delimiter //

drop procedure if exists CasesPerDay //

create procedure CasesPerDay ()
begin
    set @i := 0 ;
    select 
    t1.date as Date
    , (@i:=@i+1) as Day
    , WEEK(t1.date,3) as Kw
    , WEEKDAY(t1.date) as WTag
    , t1.cases as Cases
    , t1.deaths as Deaths
    , t1.cases - t2.cases as incCases
    , t1.deaths - t2.deaths as incDeaths
    from rki as t1 
    inner join rki as t2 
    on t1.date=adddate(t2.date,1)
    where t1.cases > t2.cases;
end
//

drop procedure if exists CasesPerDayShort //


create procedure CasesPerDayShort ()
begin
    set @i := 0 ;
    select 
    t1.date as Date
    , (@i:=@i+1) as Day
    , t1.cases - t2.cases as incCases
    , t1.deaths - t2.deaths as incDeaths
    from rki as t1 
    inner join rki as t2 
    on t1.date=adddate(t2.date,1)
    where t1.cases > t2.cases;
end
//

drop procedure if exists CasesPerDayOffset //

create procedure CasesPerDayOffset ( n INT )
begin

  drop table if exists cpd;
  
  create temporary table cpd 
    ( Date DATE primary key, Cases INT, Deaths INT )
  select 
      Refdatum as Date
    , sum(AnzahlFall) as Cases
    , sum(AnzahlTodesfall) as Deaths
  from Faelle
  where Altersgruppe = 'A80+' -- or Altersgruppe = 'A60-A79'
  group by Refdatum
  order by Refdatum
  ;
  
  select 
      a.Date as Date
    , a.Cases as Cases
    , b.Deaths as Deaths
  from cpd as a 
  join cpd as b
  on a.Date = b.Date - n
  ;

end
//

DROP PROCEDURE IF EXISTS CasesPerDayBL //

CREATE PROCEDURE CasesPerDayBL (IdBL INT)
BEGIN

   SELECT 
      IdLandkreis div 1000 AS BL
    , Meldedatum AS Kw
    , sum(AnzahlFall) AS Cases
    , sum(AnzahlTodesfall) AS Deaths
    FROM Faelle
    WHERE IdLandkreis DIV 1000 = IdBL
    GROUP BY IdBL, Meldedatum ;
end
//

delimiter ;
 
