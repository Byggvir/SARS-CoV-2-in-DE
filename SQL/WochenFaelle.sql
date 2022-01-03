use RKI;

drop table if exists tMDatum;
create temporary table tMDatum 
    (   Meldedatum DATE
        , primary key (Meldedatum)
    )
    (   select distinct
            Meldedatum as Meldedatum
        from Faelle
        where
            Meldedatum > '2021-06-30' 
    );
    
drop table if exists tBAM;
create temporary table tBAM   
    (     IdBundesland INT
        , Altersgruppe CHAR(8)
        , Meldedatum DATE
        , primary key (
              IdBundesland
            , Altersgruppe
            , Meldedatum
        )
    )
    (   select distinct
              L.IdLandkreis div 1000 as IdBundesland
            , A.Altersgruppe 
            , M.Meldedatum
        from 
              tMDatum as M
            , Landkreis as L
            , FallAltersgruppen as A
        where 
            M.Meldedatum > '2021-09-30'
    );
    
drop table if exists FWoche;

create table FWoche 
    (     IdBundesland INT
        , Altersgruppe CHAR(8)
        , Meldedatum DATE
        , Anzahl7Tage BIGINT(20)
        , primary key (
              IdBundesland
            , Altersgruppe
            , Meldedatum
        )
    )
    (
    select 
          F1.IdBundesland
        , F1.Altersgruppe
        , F1.Meldedatum
        , sum(F2.AnzahlFall) as Anzahl7Tage
    from tBAM as F1 
    join Faelle as F2 
    on
        F1.Meldedatum > adddate(F2.Meldedatum,7)
        and F1.Altersgruppe = F2.Altersgruppe
        and F1.IdBundesland = F2.IdLandkreis div 1000
    group by
          F1.Meldedatum
        , F1.Altersgruppe
        , F1.IdBundesland 
    )
;

create or replace view RWoche as
select 
    F1.IdBundesland
    , F1.Altersgruppe
    , F1.Meldedatum
    , power(F1.Anzahl7Tage/F2.Anzahl7Tage,4/7) as R 
from FWoche as F1 
join FWoche as F2
on 
    F1.IdBundesland = F2.IdBundesland
    and F1.Altersgruppe = F2.Altersgruppe
    and F1.Meldedatum = adddate(F2.Meldedatum,7)
    ;
    
    
create or replace view RVergleich as

select 
    r1.IdBundesland
    , B.Bundesland
    , B.Abk
    , r1.Meldedatum
    , weekday(r1.Meldedatum) as WTag 
    , r1.Altersgruppe as Altersgruppe1    
    , r2.Altersgruppe as Altersgruppe2
    , r1.R as R1
    , r2.R as R2 
from RWoche as r1 
join RWoche as r2
on  
    r1.IdBundesland = r2.IdBundesland 
    and r1.Meldedatum = r2.Meldedatum 
join Bundesland as B
on
    r1.IdBundesland = B.IdBundesland
;
