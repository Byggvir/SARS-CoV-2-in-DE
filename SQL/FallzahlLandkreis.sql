use RKI;

delimiter //

drop procedure if exists FaelleLandkreis //

create procedure FaelleLandkreis ()
begin

create table if not exists FaelleLandkreis (
      IdLandkreis INT(11) DEFAULT 0
    , Meldedatum DATE
    , Datenbestand DATE
    , AnzahlFall INT (11) DEFAULT 0
    , AnzahlTodesfall INT(11) DEFAULT 0
    , primary key ( IdLandkreis,Meldedatum, Datenbestand)
);

replace FaelleLandkreis
select 
      IdLandkreis
    , Meldedatum
    , Datenbestand
    , sum(AnzahlFall) as Anzahlfall
    , sum(AnzahlTodesfall) as AnzahlTodesfall
from Faelle as A  
group by 
      IdLandkreis
      , Meldedatum
;

end
//

drop procedure if exists WenigFaelle //

create procedure WenigFaelle ()

begin

select 
    A.IdLandkreis
    , L.Landkreis
    , A.Meldedatum
    , A.Datenbestand
    , A.AnzahlFall as FaelleTag
    , B.AnzahlFall as FaelleVortag
from
    FaelleLandkreis as A
join
    FaelleLandkreis as B
on
    A.IdLandkreis = B.IdLandkreis
    and A.Meldedatum = B.Meldedatum
    and A.Datenbestand = adddate(B.Datenbestand,-2)
join 
    Landkreis as L
on 
    A.IdLandkreis = L.IdLandkreis
where 
    A.Meldedatum = adddate(A.Datenbestand,-1)
    and A.AnzahlFall < B.AnzahlFall * 0.3
order by
    A.IdLandkreis
    , A.Datenbestand
;
    
end
//

delimiter ;
