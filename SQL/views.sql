use RKI;

delimiter //

-----
-- Bestimmen des Alters aus der Zeichenkette für die Altersgruppe
--
-- AlterLow('An-Am') = n 
-- AlterHigh('An-Am') = m
-- '+' wird in -A100 umgewandelt 
-----

create or replace 
function AlterLow ( AG CHAR(20)) returns INT
begin
    return REGEXP_REPLACE(REGEXP_REPLACE(AG ,'\\+','-A100') ,'A([0-9]*)[-].*','\\1');
end 
//

create or replace 
function AlterHigh ( AG CHAR(20)) returns INT
begin

    return REGEXP_REPLACE(REGEXP_REPLACE(AG ,'\\+','-A100') ,'A[0-9]*-A([0-9]*)','\\1');

end 
//

create or replace procedure topfaelle ( n INT )
begin
    set @r:=0; 
    select * 
    from ( 
        select 
            Meldedatum
            , Anzahlfall
            , @r:=@r+1 as Rank 
        from FaelleProTag 
        order by
            AnzahlFall desc limit n 
        ) as B 
    order by Meldedatum;
end 
//

delimiter ;

-----
--Berechnen der Personen in den Impf-Altersgruppen 12-17, 18-59 und 60+
-----

create or replace view ImpfBev as 
    select distinct 
        AlterVon
        , AlterBis
        , ( select 
                sum(Insgesamt) 
            from DEU as D 
            where 
                I.AlterVon <= Age 
                and I.AlterBis >= Age 
                and Stichtag="2020-12-31" 
            ) as Anzahl 
    from Impfungen as I 
    where AlterVon !=-1
    union
    select
        0 as AlterVon
        , 11 as AlterBis
        , ( select 
                sum(Insgesamt) 
            from DEU as D 
            where 
                Age >= 0 
                and Age <= 11 
                and Stichtag="2020-12-31" 
            ) as Anzahl 
    ;
    
;

-- Ende

-----
--Berechnen der Personen in den Fallzahl-Altersgruppen 0-4,5-14,15-34,35-59,60-79 und 80+
-----

create or replace view InfektBev as 
    select
        Altersgruppe
        , AlterVon
        , AlterBis
        , ( select 
                sum(Insgesamt) 
            from DEU as D 
            where 
                F.AlterVon <= Age 
                and F.AlterBis >= Age 
                and Stichtag="2020-12-31" 
            ) as Anzahl 
    from FallAltersgruppen as F
    where Altersgruppe !="unbekan"
;

-- Ende

-----
-- Zuammenfassen der Impfungen auf Landkreisebene 
-- vollständig geimpften pro Tag auf Bundesebene
-----

create or replace
view ImpfungenProTag as
    select 
        Impfdatum
        , AlterVon
        , AlterBis
        , sum(Anzahl) as Anzahl
    from Impfungen as I
    where
        Impfschutz = 2
    group by
        Impfdatum
        , AlterVon
    order by
        Impfdatum
        , AlterVon
;

-- Ende

------
-- Berechnen der kumulativen voll Geimpften auf Bundesebene
------

create or replace
view ImpfSummary as
    select 
        I1.Impfdatum
        , I1.AlterVon
        , I1.AlterBis
        , sum(I1.Anzahl)
        , ( select 
                sum(I2.Anzahl) 
            from ImpfungenProTag as I2 
            where I2.Impfdatum <= I1. Impfdatum
            and I2.AlterVon = I1.AlterVon
        ) as Kumulativ
    from ImpfungenProTag as I1
    group by
        I1.Impfdatum
        , I1.AlterVon
    order by
        I1.Impfdatum
        , I1.AlterVon
;

-- Ende ImpfSummary
    
------
-- Berechnen der Impfquote
------

create or replace
view ImpfQuote as
    select 
        I.ImpfDatum as ImpfDatum
        , I.AlterVon as AlterVon
        , I.AlterBis as AlterBis
        , Kumulativ/B.Anzahl as Quote
    from ImpfSummary as I 
    join ImpfBev as B 
    on 
        B.AlterVon = I.AlterVon
;
-- Ende

create or replace
view ImpfQuoteBL as
    select 
          I.IdLandkreis div 1000 as IdBundesland
        , B.Bundesland as Bundesland
        , B.Abk as Abk
        , sum(I.Anzahl) / S.Anzahl as Quote 
    from Impfungen as I 
    join Bundesland as B
    on 
        I.IdLandkreis div 1000 = B.IdBundesland 
    join DESTATIS.StdBevBL as S 
    on I.IdLandkreis div 1000 = S.IdBundesland 
    where I.ImpfSchutz = 2
        and S.Stichtag = "2020-12-31"
        and Impfdatum <= "2021-12-29"
    group by I.IdLandkreis div 1000
;

create or replace
view avgImpfQuote as
    select 
        I.Impfdatum as Impfdatum
        , I.AlterVon as AlterVon
        , I.AlterBis as AlterBis
        , (select avg(Kumulativ) from ImpfSummary as A where A.AlterVon = I.AlterVon and A.Impfdatum <= I.Impfdatum and adddate(A.Impfdatum,41) >= I.Impfdatum) / B.Anzahl as Quote
    from ImpfSummary as I 
    join ImpfBev as B 
    on 
        B.AlterVon = I.AlterVon
;
-- Ende

-----
-- RZahlUV
--
-- RZahlen der Ungeimpfen A00-A04 und der Geimpfen A60-A79
-----

create or replace 
view RZahlUV as 
    select 
        A.IdBundesland
        , A.Datum
        , A.Zeitraum
        , A.R as Ru 
        , B.R as Rv
        , A.Rlow as Rulow
        , A.Rhigh as Ruhigh
        , B.Rlow as Rvlow
        , B.Rhigh as Rvhigh 
    from RZahl as A 
    join RZahl as B 
    on 
        A.IdBundesland=B.IdBundesland
        and A.Datum=B.Datum 
        and A.Zeitraum=B.Zeitraum
    where 
        A.Altersgruppe = 'A05-A14' 
        and B.Altersgruppe='A60-A79' 
    ;

-- Ende RZahlUV

------
-- Berechnen der ImpfSchutzes
-- 
-- Annahme die Altersgruppe A00-A04 ist ungeimpft 
-- und in ihrem Verhalten hinsichtlich der NPI mit der 
-- geimpften Altersgruppe vergleichbar.
------

create or replace
view HelpSchutz as

    select 
        *
        , ( select Anzahl from InfektBev where AlterVon = 5 ) as N_u
        , ( select Anzahl from InfektBev where AlterVon = 80 ) as N_g
        , ( select AnzahlFallKum from FaelleBL where IdBundesland = R.IdBundesland and Altersgruppe= 'A05-A14' and Meldedatum = R.Datum) as I_u
        , ( select AnzahlFallKum from FaelleBL where IdBundesland = R.IdBundesland and Altersgruppe= 'A80+' and Meldedatum = R.Datum) as I_g
        , ( select max(Quote) from ImpfQuote where AlterVon=60 and ImpfDatum <= Datum) as Q_g
    from RZahlUV as R
    order by 
        Datum
        , IdBundesland
        , Zeitraum
;
-- Ende HelpSchutz

create or replace view Schutz as

    select 
        Datum
        , Zeitraum
        , IdBundesland
        , round((N_g - I_g - (N_u - I_u) * N_g / N_u * Rv/Ru)/Q_g/(N_g-I_g)*100,3) as PS_Schutz
        , round((N_g - I_g - (N_u - I_u) * N_g / N_u * ((Rvlow-Rv)/1.959964*1.650944+Rv)/((Ruhigh-Ru)/1.959964*1.650944+Ru))/Q_g/(N_g-I_g)*100,3) as PS_SchutzUG
        , round((N_g - I_g - (N_u - I_u) * N_g / N_u * ((Rvhigh-Rv)/1.959964*1.650944+Rv)/((Rulow-Ru)/1.959964*1.650944+Ru))/Q_g/(N_g - I_g)*100,3) as PS_SchutzOG
    from 
        HelpSchutz 
    ;

create or replace view PrognoseTote as
    select 
        F.Altersgruppe
        , F.AnzahlFall as AnzahlFall
        , F.AnzahlTodesfall as AnzahlTodesfall
        , F.AnzahlTodesfall / F.AnzahlFall * 100 as CFR 
        , round(F.AnzahlTodesfall/F.AnzahlFall * (I.Anzahl-F.AnzahlFall) + F.AnzahlTodesfall) as 'ProgTodesfall'
    from (
        select 
            Altersgruppe
            , sum(AnzahlTodesfall) as AnzahlTodesfall
            , sum(AnzahlFall) as AnzahlFall
        from Faelle
        where 
            Altersgruppe != 'unbekan' group by Altersgruppe 
        ) as F
    join InfektBev as I 
    on 
        F.Altersgruppe = I.Altersgruppe
;


create or replace view ImpfOverview as
select 
    AlterVon
    , AlterBis
    , sum(Teilgeimpft) as Teilweise
    , sum(Vollgeimpft) as Vollgeimpft
    , sum(Booster) as Booster 
from ( 
    select
        AlterVon 
        , AlterBis
        , ( case when ImpfSchutz = 1 then sum(Anzahl) else 0 end ) as Teilgeimpft
        , ( case when ImpfSchutz = 2 then sum(Anzahl) else 0 end ) as Vollgeimpft
        , ( case when ImpfSchutz = 3 then sum(Anzahl) else 0 end ) as Booster 
    from Impfungen 
    group by
        Impfschutz
        , AlterVon
    ) as A 
group by 
    AlterVon 
;

create or replace view FaelleProWoche as
    select     
          year(Meldedatum) as Jahr
        , week(Meldedatum,3) as Kw
        , PandemieWoche(Meldedatum) as PandemieWoche
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle  
    group by 
        PandemieWoche
    order by
        PandemieWoche
;

create or replace view FaelleProRefWoche as
    select     
        ( case when week(Refdatum,3) = 53 then 2020 else year(Refdatum) end ) as Jahr
        , week(Refdatum,3) as Kw
        , PandemieWoche(Refdatum) as PandemieWoche
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle  
    group by 
        Jahr
        , PandemieWoche  
;

create or replace view FaelleProTag as
    select     
        Meldedatum
        , year(Meldedatum) as Jahr
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle  
    group by 
        Meldedatum  
;

create or replace view FaelleProRefTag as
    select     
        Refdatum
        , year(Refdatum) as Jahr
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle  
    group by 
        Refdatum  
;
    
create or replace view FaelleProTagBL as
    select     
        IdLandkreis div 1000 as IdBundesland
        , Meldedatum
        , year(Meldedatum) as Jahr
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle  
    group by 
        IdBundesland
        , Meldedatum
;

create or replace view FaelleProTagAltersgruppe as
    select     
        Meldedatum
        , Altersgruppe
        , year(Meldedatum) as Jahr
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle  
    group by 
        Meldedatum
        , Altersgruppe;
;

create or replace view FaelleProWocheAltersgruppe as
   select
      PandemieWoche(Meldedatum) AS PandemieWoche
    , year(Meldedatum) as Jahr
    , week(Meldedatum,3) as Kw
    , Altersgruppe as Altersgruppe
    , AlterLow(Altersgruppe) as AlterVon
    , AlterHigh(Altersgruppe) as AlterBis
    , sum(AnzahlFall) as AnzahlFall
    , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle
    where Altersgruppe <> 'unbekan'
    group by 
        PandemieWoche
        , Altersgruppe;

create or replace view FaelleProMonatAltersgruppe as
    select 
        Jahr
        , Monat
        , F.Altersgruppe
        , AnzahlFall
        , AnzahlTodesfall
        , AnzahlFall / IB.Anzahl * 100000 as 'Fall [1/100k]'
        , AnzahlTodesfall / IB.Anzahl * 100000 as 'Todesfall [1/100k]'
        , AnzahlTodesfall / AnzahlFall * 100 as 'CFR [%]'
        , IB.Anzahl as AnzahlBev
    from (    
        select
            year(Meldedatum) as Jahr
            , month(Meldedatum) as Monat
            , Altersgruppe as Altersgruppe
            , sum(F.AnzahlFall) as AnzahlFall
            , sum(F.AnzahlTodesfall) as AnzahlTodesfall
        from Faelle as F
        where F.Altersgruppe <> 'unbekan'
        group by 
            Jahr
            , Monat
            , Altersgruppe
        ) as F
    join InfektBev as IB
    on ( F.Altersgruppe = IB.Altersgruppe )
    order by 
        Jahr
        , Monat
        , F.Altersgruppe
;

create or replace view FaelleProAltersgruppe as
   select 
      F.Altersgruppe as Altersgruppe
    , IB.AlterVon as AlterVon
    , IB.AlterBis as AlterBis
    , sum(AnzahlFall) as AnzahlFall
    , sum(AnzahlTodesfall) as AnzahlTodesfall
    , IB.Anzahl as AnzahlBev
    from Faelle as F
    join InfektBev as IB
    on ( F.Altersgruppe = IB.Altersgruppe )
    where F.Altersgruppe <> 'unbekan'
    group by 
       F.Altersgruppe;

create or replace view FaelleProAltersgruppeGeschlecht as
   select 
      Altersgruppe as Altersgruppe
    , Geschlecht as Geschlecht
    , sum(AnzahlFall) as AnzahlFall
    , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle as F
    group by 
       Altersgruppe
       , Geschlecht;

create or replace view FaelleProMonat as
    select     
        year(Meldedatum) as Jahr
        , month(Meldedatum) as Monat
        , date(concat(year(Meldedatum),"-",month(Meldedatum),"-",1)) as Datum
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle  
    group by 
        Jahr
        , Monat
;

create or replace view FaelleProJahr as
    select     
        year(Meldedatum) as Jahr
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle  
    group by 
        Jahr
;

create or replace view FaelleProRefMonat as
    select     
        year(Refdatum) as Jahr
        , month(Refdatum) as Monat
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle  
    group by 
        Jahr
        , Monat
;

create or replace view FaelleProMonatBL as
    select     
        IdLandkreis div 1000 as IdBundesland 
        , year(Meldedatum) as Jahr
        , month(Meldedatum) as Monat
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle  
    group by
        IdBundesland
        , Jahr
        , Monat
;

create or replace view InzidenzAltersgruppe as
   select
      PandemieWoche
    , Kw
    , F.Altersgruppe as Altersgruppe
    , AnzahlFall
    , AnzahlTodesfall
    , IB.Anzahl as AnzahlBev
    from FaelleProWocheAltersgruppe as F 
    join InfektBev as IB
    on ( F.Altersgruppe = IB.Altersgruppe )
    where 
        F.Altersgruppe <> 'unbekan'
    group by 
        PandemieWoche
        , Altersgruppe
    order by
        PandemieWoche
        , Altersgruppe
    ;
    
create or replace view InzidenzAltersgruppeBL as

    select 
        IdLandkreis div 1000 as IdBundesland
      , B.Bundesland as Bundesland
      , PandemieWoche(Meldedatum) as PandemieWoche
      , F.Altersgruppe as Altersgruppe
      , sum(AnzahlFall) as AnzahlFall
      , sum(AnzahlTodesfall) as AnzahlTodesfall
      , Anzahl as Bev
    from Faelle as F 
    join ( 
        select 
               IdBundesland
            , Altersgruppe 
            , sum(Anzahl) as Anzahl
            from DESTATIS.StdBev6BL 
            where 
                Stichtag = "2020-12-31" 
            group by 
                IdBundesland
                , Altersgruppe
        ) as S 
    on 
        F.IdLandkreis div 1000 = S.IdBundesland
        and F.Altersgruppe = S.Altersgruppe
    join Bundesland as B
    on
        F.IdLandkreis div 1000 = B.IdBundesland
    group by 
        F.IdLandkreis div 1000
        , PandemieWoche(Meldedatum)
        , F.Altersgruppe
;

create or replace view InzidenzBL as

    select 
        IdLandkreis div 1000 as IdBundesland
        , B.Bundesland as Bundesland
        , B.Abk as Abk
        , PandemieWoche(Meldedatum) as PandemieWoche
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
        , Anzahl as Bev
    from Faelle as F 
    join ( 
        select 
              IdBundesland
            , sum(Anzahl) as Anzahl
            from DESTATIS.StdBev6BL 
            where 
                Stichtag = "2020-12-31" 
            group by 
                IdBundesland
        ) as S 
    on 
        F.IdLandkreis div 1000 = S.IdBundesland
    join Bundesland as B
    on
        F.IdLandkreis div 1000 = B.IdBundesland
    group by 
        F.IdLandkreis div 1000
        , PandemieWoche(Meldedatum)
;

CREATE OR REPLACE VIEW Impfdurchbruch AS
SELECT
    Woche   
    , case when AlterBis <> 100 
        then concat(AlterVon,"-",AlterBis)
        else concat(AlterVon,"+")
      end as Altersgruppe
    , Outcome 
    , Gruppe
    , AnzahlKum
    , Anzahl4W
FROM ImpfD as I
JOIN ImpfDOutcome as O
ON 
    I.IdOutcome = O.IdOutcome
JOIN ImpfDGruppe as G
ON
    I.IdGruppe = G.IdGruppe
where
    I.IdGruppe <> "A"
;

CREATE OR REPLACE VIEW ImpfAnteile AS
SELECT 
    Woche
    , AlterVon
    , AlterBis
    , Outcome
    , max(Alle) as Alle
    , max(Geimpft) as Geimpft
    , max(Ungeimpft) as Ungeimpft
FROM (
      
    SELECT
        Woche
        , AlterVon
        , AlterBis
        , Outcome
        , case when IdGruppe = 'A' then Anzahl4W else 0 end as Alle
        , case when IdGruppe = 'G' then Anzahl4W else 0 end as Geimpft
        , case when IdGruppe = 'U' then Anzahl4W else 0 end as Ungeimpft
    FROM ImpfD as D
    JOIN ImpfDOutcome as O
    ON 
        O.IdOutcome = D.IdOutcome
) as I
GROUP BY
    Woche, AlterVon, AlterBis, Outcome
;

create or replace view StdFaelleBL as
select 
      R.IdBundesland as IdBundesland
    , R.Bundesland as Bundesland
    , R.Abk as Abk
    , sum(Anzahl) as Anzahl
    , sum(R.SInfections) as SAnzahl
from (
select 
      A.IdLandkreis div 1000 as IdBundesland
    , L.Bundesland as Bundesland
    , L.Abk as Abk
    , A.Geschlecht as Geschlecht
    , A.Altersgruppe as Altersgruppe
    , sum(AnzahlFall) as Anzahl
    , sum(AnzahlFall)/B.Anzahl*S.Anzahl as SInfections
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
group by 
    A.IdLandkreis div 1000
    , A.Geschlecht
    , A.Altersgruppe 
) as R
group by 
    R.IdBundesland
;   

create or replace view FaelleAG as
select 
    *
    , case when Altersgruppe = 'A00-A04' then AnzahlFall else 0 end as A00_A04
    , case when Altersgruppe = 'A05-A14' then AnzahlFall else 0 end as A05_A14
    , case when Altersgruppe = 'A15-A34' then AnzahlFall else 0 end as A15_A34
    , case when Altersgruppe = 'A35-A59' then AnzahlFall else 0 end as A35_A59
    , case when Altersgruppe = 'A60-A79' then AnzahlFall else 0 end as A60_A79
    , case when Altersgruppe = 'A80+' then AnzahlFall else 0 end as A80_A100
    , case when Altersgruppe = 'A00-A04' then AnzahlTodesfall else 0 end as T00_T04
    , case when Altersgruppe = 'A05-A14' then AnzahlTodesfall else 0 end as T05_T14
    , case when Altersgruppe = 'A15-A34' then AnzahlTodesfall else 0 end as T15_T34
    , case when Altersgruppe = 'A35-A59' then AnzahlTodesfall else 0 end as T35_T59
    , case when Altersgruppe = 'A60-A79' then AnzahlTodesfall else 0 end as T60_T79
    , case when Altersgruppe = 'A80+' then AnzahlTodesfall else 0 end as T80_T100
from 
    Faelle;

create or replace view FaelleAGProTag as
select 
    Meldedatum
    , sum(AnzahlFall) as AnzahlFall
    , sum(AnzahlTodesfall) as AnzahlTodesfall
    , sum(A00_A04) as A00_A04
    , sum(A05_A14) as A05_A14
    , sum(A15_A34) as A15_A34
    , sum(A35_A59) as A35_A59
    , sum(A60_A79) as A60_A79
    , sum(A80_A100) as A80_A100
    , sum(T00_T04) as T00_T04
    , sum(T05_T14) as T05_T14
    , sum(T15_T34) as T15_T34
    , sum(T35_T59) as T35_T59
    , sum(T60_T79) as T60_T79
    , sum(T80_T100) as T80_T100
from 
    FaelleAG
group by
    Meldedatum;

create or replace view FaelleBLSum as
select 
    B.IdBundesland as IdBundesland
    , B.Bundesland as Bundesland
    , F.Altersgruppe as Altersgruppe
    , F.Geschlecht as Geschlecht
    , sum(AnzahlFall) as AnzahlFall
    , sum(AnzahlTodesfall) as AnzahlTodesfall
    , E.Anzahl as Einwohner
from Faelle as F

join Bundesland as B
on B.IdBundesland = F.IdLandkreis div 1000

join DESTATIS.StdBev6BL as E
on 
    B.IdBundesland = E.IdBundesland
    and F.Geschlecht = E.Geschlecht
    and F.Altersgruppe = E.Altersgruppe
where 
  E.Stichtag = "2020-12-31"
group by
 B.IdBundesland
 , F.Geschlecht
 , F.Altersgruppe
;

create or replace view FaelleAGProMonat as
select 
    year(Meldedatum) as Jahr
    , month(Meldedatum) as Monat
    , sum(AnzahlFall) as AnzahlFall
    , sum(AnzahlTodesfall) as AnzahlTodesfall
    , sum(A00_A04) as A00_A04
    , sum(A05_A14) as A05_A14
    , sum(A15_A34) as A15_A34
    , sum(A35_A59) as A35_A59
    , sum(A60_A79) as A60_A79
    , sum(A80_A100) as A80_A100
    , sum(T00_T04) as T00_T04
    , sum(T05_T14) as T05_T14
    , sum(T15_T34) as T15_T34
    , sum(T35_T59) as T35_T59
    , sum(T60_T79) as T60_T79
    , sum(T80_T100) as T80_T100
from 
    FaelleAG
group by
    Jahr, Monat;

create or replace view FaelleNeu as

select 
      'Neue Fälle' as Art
    , sum(AnzahlFall) as Anzahl 
from Faelle 
where NeuerFall  <> 0
UNION
select 
      'Neue Todesfälle' as Art
    , sum(AnzahlTodesfall) as Anzahl 
from Faelle 
where NeuerTodesfall  <> 0

;
