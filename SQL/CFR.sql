use RKI;

delimiter //

drop function if exists SigmaRel ;

create function SigmaRel ( k BIGINT, n BIGINT )
returns DOUBLE
begin
  
  if n > 0 then
    return (sqrt(k*(n-k)/n)/n) ;
  else
    return (0);
  end if;
  
end
//

drop function if exists savediv ;

create function savediv ( a BIGINT, b BIGINT )
returns DOUBLE
begin
  
  if b > 0 then
    return (a/b) ;
  else
    return (0);
  end if;
  
end
//

drop procedure if exists CFRBundeslandStdBev //

create procedure CFRBundeslandStdBev ()
begin

set @bev := (Select sum(Anzahl) from DESTATIS.StdBev6 where Stichtag = "2020-12-31");

select 
      Z.Bundesland
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
group by A.IdLandkreis div 1000, A.Geschlecht, A.Altersgruppe 
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
    order by 
    CFR desc
    ) as O 
;
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

create or replace view RollendeCFR as

    select
        F1.Meldedatum
        , F1.IdBundesland
        , F1.Altersgruppe
        , (F1.AnzahlTodesfallKum - F2.AnzahlTodesfallKum) / (F1.AnzahlFallKum-F2.AnzahlFallKum) as CFR
        , round(sqrt((F1.AnzahlTodesfallKum -F2.AnzahlTodesfallKum)/(F1.AnzahlFallKum-F2.AnzahlFallKum) * (1-(F1.AnzahlTodesfallKum -F2.AnzahlTodesfallKum)/(F1.AnzahlFallKum-F2.AnzahlFallKum)) / (F1.AnzahlFallKum-F2.AnzahlFallKum)),4) as sigma

    from FaelleBL as F1
    join FaelleBL as F2
    on 
        F1.Meldedatum = adddate(F2.Meldedatum,41)
        and F1.IdBundesland = F2.IdBundesland
        and F1.Altersgruppe = F2.Altersgruppe
;

create or replace view CFR as

    select
        Meldedatum
        , IdBundesland
        , Altersgruppe
        , (AnzahlTodesfallKum/AnzahlFallKum) as CFR
        , sqrt(AnzahlTodesfallKum/AnzahlFallKum * (1-AnzahlTodesfallKum/AnzahlFallKum) / AnzahlFallKum) as Sigma
    from FaelleBL
;

create or replace view CFRWoche as 
    
    select 
          DATE_ADD(Meldedatum, INTERVAL(-WEEKDAY(Meldedatum)) DAY) as Datum  
        , year(Meldedatum) as Jahr
        , week(Meldedatum,3) as Kw
        , PandemieWoche(Meldedatum) as Pw
        , Altersgruppe
        , Geschlecht
        , FORMAT(sum(AnzahlTodesfall)/sum(AnzahlFall),6) as CFR
    from Faelle 
    where 
        Geschlecht <> 'u' and Altersgruppe <> 'unbekan'
    group by 
        Pw
        , Altersgruppe
        , Geschlecht
    union
    select 
          DATE_ADD(Meldedatum, INTERVAL(-WEEKDAY(Meldedatum)) DAY) as Datum  
        , year(Meldedatum) as Jahr
        , week(Meldedatum,3) as Kw
        , PandemieWoche(Meldedatum) as Pw
        , 'Alle' as Altersgruppe
        , Geschlecht
        , FORMAT(sum(AnzahlTodesfall)/sum(AnzahlFall),6) as CFR
    from Faelle 
    where 
        Geschlecht <> 'u' and Altersgruppe <> 'unbekan'
    group by 
        Pw
        , Geschlecht
    union
    select 
          DATE_ADD(Meldedatum, INTERVAL(-WEEKDAY(Meldedatum)) DAY) as Datum  
        , year(Meldedatum) as Jahr
        , week(Meldedatum,3) as Kw
        , PandemieWoche(Meldedatum) as Pw
        , Altersgruppe
        , 'B' as Geschlecht
        , FORMAT(sum(AnzahlTodesfall)/sum(AnzahlFall),6) as CFR
    from Faelle 
    where 
        Geschlecht <> 'u' and Altersgruppe <> 'unbekan'
    group by 
        Pw
        , Altersgruppe
    union
    select 
          DATE_ADD(Meldedatum, INTERVAL(-WEEKDAY(Meldedatum)) DAY) as Datum  
        , year(Meldedatum) as Jahr
        , week(Meldedatum,3) as Kw
        , PandemieWoche(Meldedatum) as Pw
        , 'Alle' as Altersgruppe
        , 'B' as Geschlecht
        , FORMAT(sum(AnzahlTodesfall)/sum(AnzahlFall),6) as CFR
    from Faelle 
    where 
        Geschlecht <> 'u' and Altersgruppe <> 'unbekan'
    group by 
        Pw ;
    
create or replace view CFRMonat as 

    select
        date(concat(year(Meldedatum),'-',month(Meldedatum),',',1)) as Datum
        , year(Meldedatum) as Jahr
        , month(Meldedatum) as Monat
        , F.Altersgruppe
        , F.Geschlecht
        , savediv(sum(AnzahlTodesfall),sum(AnzahlFall)) as CFR
        , SigmaRel(sum(AnzahlTodesfall),sum(AnzahlFall)) as SigmaRel
    from Faelle as F
    where 
        F.Geschlecht <> 'u' and F.Altersgruppe <> 'unbekan'
    group by 
        Jahr
        , Monat
        , F.Altersgruppe
        , F.Geschlecht
    union
    select 
        date(concat(year(Meldedatum),'-',month(Meldedatum),',',1)) as Datum
        , year(Meldedatum) as Jahr
        , month(Meldedatum) as Monat
        , 'Alle' as Altersgruppe
        , F.Geschlecht
        , savediv(sum(AnzahlTodesfall),sum(AnzahlFall)) as CFR
        , SigmaRel(sum(AnzahlTodesfall),sum(AnzahlFall)) as SigmaRel
    from Faelle as F
    where 
        F.Geschlecht <> 'u' and F.Altersgruppe <> 'unbekan'
    group by 
        Jahr
        , Monat
        , F.Geschlecht
    union
    select 
        date(concat(year(Meldedatum),'-',month(Meldedatum),',',1)) as Datum
        , year(Meldedatum) as Jahr
        , month(Meldedatum) as Monat
        , F.Altersgruppe
        , 'B' as Geschlecht
        , savediv(sum(AnzahlTodesfall),sum(AnzahlFall)) as CFR
        , SigmaRel(sum(AnzahlTodesfall),sum(AnzahlFall)) as SigmaRel
    from Faelle as F
    where 
        F.Geschlecht <> 'u' and F.Altersgruppe <> 'unbekan'
    group by 
        Jahr
        , Monat
        , F.Altersgruppe
    union
    select 
        date(concat(year(Meldedatum),'-',month(Meldedatum),',',1)) as Datum
        , year(Meldedatum) as Jahr
        , month(Meldedatum) as Monat
        , 'Alle' as Altersgruppe
        , 'B' as Geschlecht
        , savediv(sum(AnzahlTodesfall),sum(AnzahlFall)) as CFR
        , SigmaRel(sum(AnzahlTodesfall),sum(AnzahlFall)) as SigmaRel
    from Faelle as F
    where 
        F.Geschlecht <> 'u' and F.Altersgruppe <> 'unbekan'
    group by 
        Jahr
        , Monat
    ;


create or replace view CFRMonatBL as 

    select
        date(concat(year(Meldedatum),'-',month(Meldedatum),',',1)) as Datum
        , year(Meldedatum) as Jahr
        , month(Meldedatum) as Monat
        , IdLandkreis div 1000 as IdBundesland
        , Altersgruppe
        , Geschlecht
        , sum(AnzahlTodesfall)/sum(AnzahlFall) as CFR
    from Faelle 
    where 
        Geschlecht <> 'u' and Altersgruppe <> 'unbekan'
    group by 
        Jahr
        , Monat
        , IdBundesland
        , Altersgruppe
        , Geschlecht
    union
    select 
        date(concat(year(Meldedatum),'-',month(Meldedatum),',',1)) as Datum
        , year(Meldedatum) as Jahr
        , month(Meldedatum) as Monat
        , IdLandkreis div 1000 as IdBundesland
        , 'Alle' as Altersgruppe
        , Geschlecht
        , sum(AnzahlTodesfall)/sum(AnzahlFall) as CFR
    from Faelle 
    where 
        Geschlecht <> 'u' and Altersgruppe <> 'unbekan'
    group by 
        Jahr
        , Monat
        , IdBundesland
        , Geschlecht
    union
    select 
        date(concat(year(Meldedatum),'-',month(Meldedatum),',',1)) as Datum
        , year(Meldedatum) as Jahr
        , month(Meldedatum) as Monat
        , IdLandkreis div 1000 as IdBundesland
        , Altersgruppe
        , 'B' as Geschlecht
        , sum(AnzahlTodesfall)/sum(AnzahlFall) as CFR
    from Faelle
    where 
        Geschlecht <> 'u' and Altersgruppe <> 'unbekan'
    group by 
        Jahr
        , Monat
        , IdBundesland
        , Altersgruppe
        
    union
    select 
        date(concat(year(Meldedatum),'-',month(Meldedatum),',',1)) as Datum
        , year(Meldedatum) as Jahr
        , month(Meldedatum) as Monat
        , IdLandkreis div 1000 as IdBundesland
        , 'Alle' as Altersgruppe
        , 'B' as Geschlecht
        , sum(AnzahlTodesfall)/sum(AnzahlFall)  as CFR
    from Faelle 
    where 
        Geschlecht <> 'u' and Altersgruppe <> 'unbekan'
    group by 
        Jahr
        , Monat
        , IdBundesland
    ;

create or replace view StdCFRMonat as 

    select
        Datum 
        , Jahr
        , Monat
        , sum(AnzahlTodesfall)/sum(AnzahlFall) as CFR
        , sum(AnzahlTodesfall / AnzahlFall * B.Anteil) as StdCFR
    from (
    select
        date(concat(year(Meldedatum),'-',month(Meldedatum),',',1)) as Datum
        , year(Meldedatum) as Jahr
        , month(Meldedatum) as Monat
        , Altersgruppe as Altersgruppe
        , Geschlecht as Geschlecht
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle
    where 
        Geschlecht <> 'u' and Altersgruppe <> 'unbekan'
    group by 
        Jahr
        , Monat
        , Altersgruppe
        , Geschlecht
    ) as C
    join  DESTATIS.StdBev6 as B
    on
        C.Geschlecht = B.Geschlecht
        and C.Altersgruppe = B.Altersgruppe
    
    where 
        B.Stichtag = '2020-12-31'
        and C.Geschlecht <> 'u' and C.Altersgruppe <> 'unbekan'
    group by 
        Jahr
        , Monat
;
