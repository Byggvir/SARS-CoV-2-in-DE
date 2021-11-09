use RKI;

delimiter //

drop procedure if exists CFRBundeslandStdBev //

create procedure CFRBundeslandStdBev ()
begin

set @bev := (Select sum(Anzahl) from DESTATIS.StdBev6 where Stichtag = "2020-12-31");

set @i:=0;

select 
    @i:=@i+1 as Rang
    , Z.Bundesland
    , round(Z.Deaths / Z.Cases*100,2) as CFR
from (
select 
      0 as IdBundesland
    , 'Deutschland' as Bundesland
    , sum(AnzahlTodesfall) as Deaths
    , sum(AnzahlFall) as Cases
from Faelle

union
select 
      R.IdBundesland as IdBundesland
    , R.Bundesland as Bundesland
    , sum(R.SDeaths) as Deaths
    , sum(R.SCases) as Cases
from (
select 
      A.IdLandkreis div 1000 as IdBundesland
    , L.Bundesland
    , A.Geschlecht as Geschlecht
    , A.Altersgruppe
    , sum(A.AnzahlFall)/B.Anzahl*S.Anzahl as SCases
    , sum(A.AnzahlTodesfall)/B.Anzahl*S.Anzahl as SDeaths
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
    S.Stichtag = B.Stichtag
    and A.IdLandkreis div 1000 = B.IdBundesland
    and A.Geschlecht = B.Geschlecht
    and A.Altersgruppe = B.Altersgruppe 
where
    S.Stichtag = "2020-12-31"
group by A.IdLandkreis div 1000,A.Geschlecht,A.Altersgruppe 
) as R
group by 
    R.IdBundesland
    -- , R.Geschlecht
) as Z
order by CFR desc
;

end
//

drop procedure if exists CFRBundesland //

create procedure CFRBundesland ()
begin

set @i:=0;

select 
      @i:=@i+1 as Rang
    , Bundesland 
    , CFR 
from (
    select 
        'Deutschland' as Bundesland
        , round(sum(AnzahlTodesfall)/sum(AnzahlFall)*100,2) as CFR
    from  Faelle
    union
    select 
        B.Bundesland as Bundesland
        , round(sum(AnzahlTodesfall)/sum(AnzahlFall)*100,2) as CFR
    from Faelle as F 
    join Bundesland as B 
    on 
        F.IdLandkreis div 1000 = B.IdBundesland
    group by F.IdLandkreis div 1000
    ) as O 
order by 
    CFR desc;
end
//

drop procedure if exists CFRBundeslandAlter //

create procedure CFRBundeslandAlter ()
begin

set @i:=0;

select 
      @i:=@i+1 as Rang
    , Bundesland 
    , Altersgruppe
    , CFR 
from ( 
    select 
        B.Bundesland as Bundesland
        , F.Altersgruppe as Altersgruppe
        , round(sum(AnzahlTodesfall)/sum(AnzahlFall)*100,2) as CFR
    from Faelle as F 
    join Bundesland as B 
    on 
        F.IdLandkreis div 1000 = B.IdBundesland
    group by F.IdLandkreis div 1000, F.Altersgruppe
    ) as O 
order by 
    Altersgruppe, CFR desc;
end
//

delimiter ;
