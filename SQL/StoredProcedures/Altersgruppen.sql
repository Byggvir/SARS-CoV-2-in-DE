USE RKI;

delimiter //

DROP PROCEDURE IF EXISTS AltersgruppePw //

CREATE PROCEDURE AltersgruppePw ()
BEGIN
 
    DROP TABLE IF EXISTS FaelleAltersgruppePw;
    
    CREATE TABLE FaelleAltersgruppePw
    ( Altersgruppe CHAR(8) 
    , Pw INT 
    , EW_insgesamt BIGINT(20)
    , AnzahlFall BIGINT(20)
    , AnzahlTodesfall BIGINT(20)
    , PRIMARY KEY (Altersgruppe,Pw) )
    (
    SELECT
        F.Altersgruppe as Altersgruppe
        , PandemieWoche(Meldedatum) as Pw
        , B.Anzahl as EW_insgesamt
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle as F 
    join InfektBev as B
    on F.Altersgruppe = B.Altersgruppe
    group by 
        Altersgruppe, Pw
    )
    ;    

END
//

DROP PROCEDURE IF EXISTS Altersgruppe714 //

CREATE PROCEDURE Altersgruppe714 ( MD DATE)
BEGIN

drop table if exists a7;

create temporary table a7 (
    select 
          F.IdLandkreis div 1000 as IdBundesland    
        , F.Altersgruppe as Altersgruppe
        , sum(F.AnzahlFall) as Woche
    from Faelle as F 
    where 
        F.Meldedatum <= MD 
        and F.Meldedatum > adddate(MD, -7) 
        and F.Altersgruppe <> 'unbekan'
    group by 
        F.IdLandkreis div 1000, F.Altersgruppe
    );
    
drop table if exists a14;
create temporary table a14 (
    select
          F.IdLandkreis div 1000 as IdBundesland    
        , F.Altersgruppe as Altersgruppe
        , sum(F.AnzahlFall) as Woche
    from Faelle as F
    where 
        F.Meldedatum <= adddate(MD, - 7) 
        and F.Meldedatum > adddate(MD, -14)
        and F.Altersgruppe <> 'unbekan'
    group by 
        F.IdLandkreis div 1000, F.Altersgruppe
    );
        
select 
      B.IdBundesland as IdBundesland
    , B.Bundesland as Bundesland  
    , B.Abk as Abk  
    , F1.Altersgruppe as Altersgruppe
    , A.EW_insgesamt as EW_insgesamt
    , F1.Woche as Woche
    , F2.Woche as Vorwoche
    , round(power(F1.Woche/F2.Woche,4/7),2) as R
from a7 as F1 
join a14 as F2
on  F1.IdBundesland = F2.IdBundesland
    and F1.Altersgruppe = F2.Altersgruppe
join Altersgruppen as A
on  F1.IdBundesland = A.IdBundesland
    and F1.Altersgruppe = A.Altersgruppe
join Bundesland as B
on F1.IdBundesland = B.IdBundesland
;

END

//

DROP PROCEDURE IF EXISTS Altersgruppe714LK //

CREATE PROCEDURE Altersgruppe714LK ( MD DATE)
BEGIN

drop table if exists a7;

create temporary table a7 (
    select 
          F.IdLandkreis as IdLandkreis    
        , F.Altersgruppe as Altersgruppe
        , sum(F.AnzahlFall) as Woche
    from Faelle as F 
    where 
        F.Meldedatum <= MD 
        and F.Meldedatum > adddate(MD, -7) 
        and F.Altersgruppe <> 'unbekan'
    group by 
        F.IdLandkreis, F.Altersgruppe
    );
    
drop table if exists a14;
create temporary table a14 (
    select
          F.IdLandkreis as IdLandkreis    
        , F.Altersgruppe as Altersgruppe
        , sum(F.AnzahlFall) as Woche
    from Faelle as F
    where 
        F.Meldedatum <= adddate(MD, - 7) 
        and F.Meldedatum > adddate(MD, -14) 
        and F.Altersgruppe <> 'unbekan'
    group by 
        F.IdLandkreis, F.Altersgruppe
    );
        
select 
      L.IdLandkreis as IdLandkreis
    , L.Landkreis as Landkreis  
    , B.Bundesland as Bundesland  
    , B.Abk as Abk  
    , F1.Altersgruppe as Altersgruppe
    , F1.Woche as Woche
    , F2.Woche as Vorwoche
    , round(power(F1.Woche/F2.Woche,4/7),2) as R
from a7 as F1 
join a14 as F2
on  F1.IdLandkreis = F2.IdLandkreis
    and F1.Altersgruppe = F2.Altersgruppe
join Bundesland as B
on F1.IdLandkreis div 1000 = B.IdBundesland
join Landkreis as L
on F1.IdLandkreis = L.IdLandkreis
;

END

//

DROP PROCEDURE IF EXISTS Altersgruppe714LKVergleich //

CREATE PROCEDURE Altersgruppe714LKVergleich ( MD DATE, AG1 CHAR(8), AG2 CHAR(8) )
BEGIN

drop table if exists a7;

create temporary table a7 (
    select 
          F.IdLandkreis as IdLandkreis    
        , F.Altersgruppe as Altersgruppe
        , sum(F.AnzahlFall) as Woche
    from Faelle as F 
    where 
        F.Meldedatum <= MD 
        and F.Meldedatum > adddate(MD, -7) 
        and F.Altersgruppe <> 'unbekan'
    group by 
        F.IdLandkreis, F.Altersgruppe
    );
    
drop table if exists a14;

create temporary table a14 (
    select
          F.IdLandkreis as IdLandkreis    
        , F.Altersgruppe as Altersgruppe
        , sum(F.AnzahlFall) as Woche
    from Faelle as F
    where 
        F.Meldedatum <= adddate(MD, - 7) 
        and F.Meldedatum > adddate(MD, -14) 
        and F.Altersgruppe <> 'unbekan'
    group by 
        F.IdLandkreis, F.Altersgruppe
    );
        
drop table if exists t;

create temporary table t (
select 
      F1.IdLandkreis as IdLandkreis
    , F1.Altersgruppe as Altersgruppe
    , round(power(F1.Woche/F2.Woche,4/7),2) as R
from a7 as F1
join a14 as F2
on  F1.IdLandkreis = F2.IdLandkreis
    and F1.Altersgruppe = F2.Altersgruppe

)
;

select 
      T1.IdLandkreis as IdLandkreis
    , B.Bundesland as Bundesland
    , B.Abk as Abk
    , L.Landkreis as Landkreis
    , AG1 as Altersgruppe1
    , AG2 as Altersgruppe2
    , T1.R as R1
    , T2.R as R2
from t as T1
join t as T2
on
    T1.IdLandkreis = T2.IdLandkreis
join Bundesland as B
on
    T1.IdLandkreis div 1000 = B.IdBundesland
join Landkreis as L
on
    T1.IdLandkreis = L.IdLandkreis
where
    T1.Altersgruppe = AG1
    and T2.Altersgruppe = AG2
;    
END

//

DROP PROCEDURE IF EXISTS AltersgruppeUp //

CREATE PROCEDURE AltersgruppeUp ( MD DATE)
BEGIN

drop table if exists a7;

create temporary table a7 (
    select
          F.IdLandkreis div 1000 as IdBundesland
        , F.Altersgruppe as Altersgruppe
        , sum(F.AnzahlFall) as Woche
    from Faelle as F 
    where 
        F.Meldedatum <= MD 
        and F.Meldedatum > adddate(MD, -7) 
        and F.Altersgruppe <> 'unbekan'
    group by 
        F.IdBundesland, F.Altersgruppe
    );
    
drop table if exists a14;
create temporary table a14 (
    select
        F.Altersgruppe as Altersgruppe
        , sum(F.AnzahlFall) as Woche
    from Faelle as F
    where 
        F.Meldedatum <= adddate(MD, - 7) 
        and F.Meldedatum > adddate(MD, -14) 
        and F.Altersgruppe <> 'unbekan'
    group by 
        F.Altersgruppe
    );
        
select 
    count(*) 
from a7 as F1 
join a14 as F2
on
    F1.Altersgruppe = F2.Altersgruppe
where
    F1.Woche > F2.Woche
union
select 
    count(*) 
from a7 as F1 
join a14 as F2
on
    F1.Altersgruppe = F2.Altersgruppe
where
    F1.Woche = F2.Woche
union
select 
    count(*) 
from a7 as F1 
join a14 as F2
on
    F1.Altersgruppe = F2.Altersgruppe
where
    F1.Woche < F2.Woche
;
END

//
delimiter ;
