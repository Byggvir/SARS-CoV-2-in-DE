use RKI;

delimiter //

drop procedure if exists TodesFaelleBundesland //

create procedure TodesFaelleBundesland ()
begin

set @i:=0;

select 
    @i:=@i+1 as Rang
    , Bundesland
    , AnzahlTodesfall
    , Bevoelkerung
from (
select 
      A.IdLandkreis div 1000 as IdBundesland
    , B.Bundesland as Bundesland
    , Altersgruppe
    , sum(A.AnzahlTodesfall) as AnzahlTodesfall
    , D.Anzahl as Bevoelkerung
    , sum(A.AnzahlTodesfall)/D.Anzahl as Letatlität
from Faelle as A 
join Bundesland as B 
on 
    A.IdLandkreis div 1000 = B.IdBundesland 
join
    ( select 
          IdBundesland
        , sum(Anzahl) as Anzahl
      from DESTATIS.DT124110013
      group by IdBundesland
    )
    as D 
on 
    A.IdLandkreis div 1000 = D.IdBundesland 
group by 
      A.IdLandkreis div 1000
order by AnzahlTodesfall desc
) as R
;

end
//

drop procedure if exists TodesFaelleBundeslandAlter //

create procedure TodesFaelleBundeslandAlter ( AG CHAR(20) )
begin

set @i:=0;

select 
    @i:=@i+1 as Rang
    , Bundesland
    , AnzahlTodesfall as Anzahl
    , Bevoelkerung
from (
select 
      A.IdLandkreis div 1000 as IdBundesland
    , B.Bundesland as Bundesland
    , Altersgruppe
    , sum(A.AnzahlTodesfall) as AnzahlTodesfall
    , D.Anzahl as Bevoelkerung
from Faelle as A 
join Bundesland as B 
on 
    A.IdLandkreis div 1000 = B.IdBundesland 
join
    ( select 
          IdBundesland
        , sum(Anzahl) as Anzahl
      from DESTATIS.DT124110013
      group by IdBundesland
    )
    as D 
on 
    A.IdLandkreis div 1000 = D.IdBundesland 
 
where Altersgruppe = AG
group by 
      A.IdLandkreis div 1000
    , A.Altersgruppe 
order by AnzahlTodesfall desc
) as R
;

end
//

drop procedure if exists LetalityBundesland //

create procedure LetalityBundesland ( AG CHAR(20) )
begin

set @i:=0;

set @TMP:= REGEXP_REPLACE(AG ,'A80\\+','A80-A99');

set @L:= REGEXP_REPLACE(@TMP ,'A([0-9]{2})[-].*','\\1');
set @U:= REGEXP_REPLACE(@TMP ,'A[0-9]{2}-A([0-9]{2})','\\1');

select 
    @i:=@i+1 as Rang
    , R.Bundesland
    , R.Anzahl
    , R.Bevoelkerung
    , R.Letality
from (
select 
      A.IdLandkreis div 1000 as IdBundesland
    , B.Bundesland
    , Altersgruppe
    , sum(AnzahlTodesfall) as Anzahl
    , C.Anzahl as Bevoelkerung
    , sum(AnzahlTodesfall)/C.Anzahl*100000 as Letality
from Faelle as A 
join Bundesland as B 
on 
    A.IdLandkreis div 1000 = B.IdBundesland
join
    ( select 
          IdBundesland
        , sum(Anzahl) as Anzahl
      from DESTATIS.DT124110013
      where 
        Altersgruppe >= @L 
        and Altersgruppe <= @U
      group by IdBundesland
    )
    as C 
on 
    A.IdLandkreis div 1000 = C.IdBundesland 
where 
    A.Altersgruppe = AG
group by A.IdLandkreis div 1000,Altersgruppe 
order by Letality desc
) as R
;

end
//

drop procedure if exists MortalityBundeslandStdBev //

create procedure MortalityBundeslandStdBev ()
begin

set @bev := (Select sum(Anzahl) from DESTATIS.StdBev6);

set @i:=0;

select 
    @i:=@i+1 as Rang
    , Z.Bundesland
    , round(Z.SAnzahl / @bev *100000,4) as Mortality
from (
select 
      R.IdBundesland
    , R.Bundesland
    , sum(R.SMortality) as SAnzahl
from (
select 
      A.IdLandkreis div 1000 as IdBundesland
    , L.Bundesland
    , A.Geschlecht
    , A.Altersgruppe
    , sum(AnzahlTodesfall) as Anzahl
    , S.Anzahl as Bevoelkerung
    , B.Anzahl as BevoelkerungBL
    , sum(AnzahlTodesfall)/B.Anzahl*S.Anzahl as SMortality
from Faelle as A 
join Bundesland as L
on 
    A.IdLandkreis div 1000 = L.IdBundesland

join DESTATIS.StdBev6 as S 
on 
    A.Altersgruppe = S.Altersgruppe 
    and A.Geschlecht = S.Geschlecht 

join DESTATIS.StdBev6BL as B
on 
    A.IdLandkreis div 1000 = B.IdBundesland
    and A.Geschlecht = B.Geschlecht 
    and A.Altersgruppe = B.Altersgruppe 

group by A.IdLandkreis div 1000,A.Geschlecht,A.Altersgruppe 
) as R
group by 
    R.IdBundesland
order by SAnzahl desc
) as Z
;

end
//

delimiter ; 
