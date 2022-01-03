USE RKI;

delimiter //
DROP PROCEDURE IF EXISTS BundeslandPw //

CREATE PROCEDURE BundeslandPw ()
BEGIN
 
    DROP TABLE IF EXISTS FaelleBundeslandPw;
    
    CREATE TABLE FaelleBundeslandPw
    ( IdBundesland INT 
    , Pw INT 
    , AnzahlFall BIGINT(20)
    , AnzahlTodesfall BIGINT(20)
    , PRIMARY KEY (IdBundesland,Pw) )
    (
    SELECT
        IdLandkreis div 1000 as IdBundesland
        , PandemieWoche(Meldedatum) as Pw
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle 
    group by 
        IdBundesland, Pw
    )
    ;    

END
//

DROP PROCEDURE IF EXISTS Bundesland714 //

CREATE PROCEDURE Bundesland714 ( MD DATE)
BEGIN

drop table if exists b7;

create temporary table b7 (
    select 
        F.IdLandkreis div 1000 as IdBundesland
        , sum(F.AnzahlFall) as Woche
    from Faelle as F 
    where 
        F.Meldedatum <= MD 
        and F.Meldedatum > adddate(MD, -7) 
    group by 
        F.IdLandkreis div 1000
    );
    
drop table if exists b14;
create temporary table b14 (
    select 
        F.IdLandkreis div 1000 as IdBundesland
        , sum(F.AnzahlFall) as Woche
    from Faelle as F
    where 
        F.Meldedatum <= adddate(MD, - 7) 
        and F.Meldedatum > adddate(MD, -14) 
    group by 
        F.IdLandkreis div 1000
    );
        
select 
    B.IdBundesland as IdBundesland
    , B.Bundesland as Bundesland
    , B.Abk as Abk
    , B.EW_insgesamt as EW_insgesamt
    , F1.Woche as Woche
    , F2.Woche as Vorwoche
    , power(F1.Woche/F2.Woche,4/7) as R
from b7 as F1 
join b14 as F2
on
    F1.IdBundesland = F2.IdBundesland
join 
    Bundesland as B
on
    F1.IdBundesland = B.IdBundesland
;

END

//

DROP PROCEDURE IF EXISTS BundeslandUp //

CREATE PROCEDURE BundeslandUp ( MD DATE)
BEGIN

drop table if exists b7;

create temporary table b7 (
    select 
        F.IdLandkreis div 1000 as IdBundesland
        , sum(F.AnzahlFall) as Woche
    from Faelle as F 
    where 
        F.Meldedatum <= MD 
        and F.Meldedatum > adddate(MD, -7) 
    group by 
        F.IdLandkreis div 1000
    );
    
drop table if exists b14;
create temporary table b14 (
    select 
        F.IdLandkreis div 1000 as IdBundesland
        , sum(F.AnzahlFall) as Woche
    from Faelle as F
    where 
        F.Meldedatum <= adddate(MD, - 7) 
        and F.Meldedatum > adddate(MD, -14) 
    group by 
        F.IdLandkreis div 1000
    );
        
select 
    count(*) 
from b7 as F1 
join b14 as F2
on
    F1.IdBundesland = F2.IdBundesland
where
    F1.Woche > F2.Woche
union
select 
    count(*) 
from b7 as F1 
join b14 as F2
on
    F1.IdBundesland = F2.IdBundesland
where
    F1.Woche = F2.Woche
union
select 
    count(*) 
from b7 as F1 
join b14 as F2
on
    F1.IdBundesland = F2.IdBundesland
where
    F1.Woche < F2.Woche
;
END

//
delimiter ;
