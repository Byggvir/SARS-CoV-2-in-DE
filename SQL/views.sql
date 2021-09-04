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
                and Stichtag="2019-12-31" 
            ) as Anzahl 
    from Impfungen as I 
    where AlterVon !=-1
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
                and Stichtag="2019-12-31" 
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
                --sum(I2.Anzahl) 
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
view avgImpfQuote (AG) as
    select 
        I.ImpfDatum as ImpfDatum
        , I.AlterVon as AlterVon
        , I.AlterBis as AlterBis
        , (select avg(Kumulativ) from ImpfSummary as A where A.AlterVon = I.AlterVon and A.Impfdatum <= I.Impfdatum and adddate(A.Impfdatum,"AG") >= I.ImpfDatum) / B.Anzahl as Quote
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
-- Ende Schutz

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

create or replace view RollendeCFR as

    select
        F1.Meldedatum
        , F1.IdBundesland
        , F1.Altersgruppe
        , (F1.AnzahlTodesfallKum -F2.AnzahlTodesfallKum)/(F1.AnzahlFallKum-F2.AnzahlFallKum) as CFR
        , sqrt((F1.AnzahlTodesfallKum -F2.AnzahlTodesfallKum)/(F1.AnzahlFallKum-F2.AnzahlFallKum) * (1-(F1.AnzahlTodesfallKum -F2.AnzahlTodesfallKum)/(F1.AnzahlFallKum-F2.AnzahlFallKum)) / (F1.AnzahlFallKum-F2.AnzahlFallKum)) as Sigma

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
