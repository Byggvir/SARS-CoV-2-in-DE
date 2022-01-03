use RKI;

create or replace view NichtMeldendeLK as
select 
    IdLandkreis div 1000 as IdBundesland
    , Bundesland
    , count(*) as Anzahl 
from ( 
    select 
        L.IdLandkreis
        , L.Landkreis
        , Anzahl
    from Landkreis as L 
    left outer join (
        select 
            IdLandkreis
            , sum(AnzahlFall) as Anzahl 
        from Faelle 
        where NeuerFall <> 0 
        group by 
            IdLandkreis
        ) as F 
    on 
        L.IdLandkreis = F.IdLandkreis 
) as B 
join Bundesland as BL 
on 
    BL.IdBundesland = B.IdLandkreis div 1000 
where 
    Anzahl is NULL 
group by IdBundesland;

delimiter //

drop procedure if exists LandkreisKw //

create procedure LandkreisKw ( Kreis CHAR(64), Kalenderwoche INT )
begin

set @i:=0;
create temporary table FaelleKw (
      IdLandkreis INT(11)
    , Kw INT(11)
    , AnzahlFall INT (11)
    , AnzahlTodesfall INT(11)
    , primary key ( IdLandkreis,Kw)
)    
select 
   IdLandkreis as IdLandkreis
 , ( case when Meldedatum > "2021-01-03" then week(Meldedatum,3)+53 else week(Meldedatum,3) end ) as Kw
 , sum(AnzahlFall) as AnzahlFall
 , sum(AnzahlTodesfall) as AnzahlTodesfall
 from Faelle
 group by
    IdLandkreis , Kw
    
;
select 
    A.IdLandkreis 
    , L.Landkreis
    , A.Kw
    , A.AnzahlFall
    , A.AnzahlFall / B.AnzahlFall as R7
   
from FaelleKw as A
join FaelleKw as B
on  A.IdLandkreis = B.IdLandkreis
    and A.Kw = B.Kw+1
join Landkreis as L
on A.IdLandkreis = L.IdLandkreis
where 
    A.Kw >= Kalenderwoche
    and L.Landkreis regexp Kreis
order by
    A.IdLandkreis 
    , A.Kw
;

end
//


drop procedure if exists LandkreisTag //

create procedure LandkreisTag ( Kreis CHAR(64), AbTag DATE )
begin

select 
    A.IdLandkreis 
    , L.Landkreis
    , A.Meldedatum
    , sum(A.AnzahlFall) as AnzahlZall
   
from Faelle as A
join Landkreis as L
on A.IdLandkreis = L.IdLandkreis
where
    A.Meldedatum >= AbTag
    and L.Landkreis regexp Kreis
group by
    A.IdLandkreis 
    , A.Meldedatum
order by
    A.IdLandkreis 
    , A.Meldedatum
    ;

end
//

drop procedure if exists LandkreisWoche //

create procedure LandkreisWoche ( IdLK INT )
begin


    select 
        ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) as Kw
        , sum(AnzahlFall) as Anzahl 
    from Faelle
    where 
        IdLandkreis = IdLK
    group by 
          Kw

;
end
//

drop procedure if exists LandkreisNeu //

create procedure LandkreisNeu ( Kreis CHAR(64) )
begin

select 
    A.IdLandkreis 
    , L.Landkreis
    , A.Meldedatum
    , sum(A.AnzahlFall) as AnzahlZall
   
from Faelle as A
join Landkreis as L
on A.IdLandkreis = L.IdLandkreis
where
    NeuerFall <> 0
    and Landkreis regexp Kreis
group by
    A.IdLandkreis 
    , A.Meldedatum
order by
    A.IdLandkreis 
    , A.Meldedatum
    ;

end
//
delimiter ; 
