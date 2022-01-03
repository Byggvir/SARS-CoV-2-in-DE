use RKI;

create or replace view FaelleBundesland as

select 
      A.IdLandkreis div 1000 as IdBundesland
    , B.Bundesland as Bundesland
    , B.Abk as Abk
    , sum(A.AnzahlFall) as Anzahlfall
    , D.Anzahl as Bevoelkerung
    , sum(A.AnzahlFall)/D.Anzahl as InfectionRatio
from Faelle as A 
join Bundesland as B 
on 
    A.IdLandkreis div 1000 = B.IdBundesland 
join
    ( select 
          IdBundesland
        , sum(Anzahl) as Anzahl
      from DESTATIS.DT124110013 as S
      where 
        Stichtag = "2020-12-31"
      group by IdBundesland
    )
    as D 
on 
    A.IdLandkreis div 1000 = D.IdBundesland 
group by 
      A.IdLandkreis div 1000
;

delimiter //

drop procedure if exists RangFaelleBundesland //

create procedure RangFaelleBundesland ()
begin

set @i:=0;

select 
    @i:=@i+1 as Rang
    , IdBundesland as IdBundesland
    , Bundesland as Bundesland
    , Abk as Abk
    , AnzahlFall as Anzahl
    , Bevoelkerung as Bevoelkerung
    , InfectionRatio as InfectionRatio
from ( select * from FaelleBundesland
order by AnzahlFall desc
) as R
order by
    InfectionRatio desc
;

end
//

drop procedure if exists FaelleBundeslandAlter //

create procedure FaelleBundeslandAlter ( AG CHAR(20) )
begin

set @i:=0;

select 
    @i:=@i+1 as Rang
    , Bundesland
    , AnzahlFall as Anzahl
    , Bevoelkerung
from (
select 
      A.IdLandkreis div 1000 as IdBundesland
    , B.Bundesland as Bundesland
    , sum(A.AnzahlFall) as AnzahlFall
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
      where 
        Stichtag = "2020-12-31"
      group by IdBundesland
    )
    as D 
on 
    A.IdLandkreis div 1000 = D.IdBundesland 
 
where Altersgruppe = AG
group by 
      A.IdLandkreis div 1000
    , A.Altersgruppe 
order by AnzahlFall desc
) as R
;

end
//

drop procedure if exists InfectionsBundesland //

create procedure InfectionsBundesland ( AG CHAR(20) )
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
    , R.InfectionRatio
from (
select 
      A.IdLandkreis div 1000
    , B.Bundesland
    , Altersgruppe
    , sum(AnzahlFall) as Anzahl
    , C.Anzahl as Bevoelkerung
    , sum(AnzahlFall)/C.Anzahl*100000 as InfectionRatio
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
        Stichtag = "2020-12-31"
        and Altersgruppe >= @L 
        and Altersgruppe <= @U
      group by IdBundesland
    )
    as C 
on 
    A.IdLandkreis div 1000 = C.IdBundesland 
where 
    A.Altersgruppe = AG
group by A.IdLandkreis div 1000,Altersgruppe 
order by InfectionRatio desc
) as R
;

end

//

drop procedure if exists InfectionsBundeslandStdBev //

create procedure InfectionsBundeslandStdBev ()
begin

set @bev := (Select sum(Anzahl) from DESTATIS.StdBev6 where Stichtag = "2020-12-31");

set @i:=0;

select 
    @i:=@i+1 as Rang
    , IdBundesland as IdBundesland
    , Z.Bundesland as Bundesland
    , Z.Abk as Abk
    , round(Z.SAnzahl / @bev *100000,4) as InfectionRatio
from (
select 
    0 as IdBundesland
    , 'Deutschland' as Bundesland
    , 'DE' as Abk
    , sum(AnzahlFall) as Anzahl
    , sum(AnzahlFall) as SAnzahl
from Faelle
union
select *
from StdFaelleBL
order by SAnzahl desc
) as Z
;

end
//

delimiter ; 

