USE RKI;

delimiter //

DROP PROCEDURE IF EXISTS LandkreisTable //

CREATE PROCEDURE LandkreisTable (IdLK INT)
BEGIN

select 
      Pw
    , max( case when Altersgruppe = 'A00-A04' then Anzahl else 0 end ) as 'A00-A04'
    , max( case when Altersgruppe = 'A05-A14' then Anzahl else 0 end ) as 'A05-A14'
    , max( case when Altersgruppe = 'A15-A34' then Anzahl else 0 end ) as 'A15-A34'
    , max( case when Altersgruppe = 'A35-A59' then Anzahl else 0 end ) as 'A35-A59'
    , max( case when Altersgruppe = 'A60-A79' then Anzahl else 0 end ) as 'A60-A79'
    , max( case when Altersgruppe = 'A80+'    then Anzahl else 0 end ) as 'A80+'
    , max( case when Altersgruppe = 'unbekan' then Anzahl else 0 end ) as 'unbekan'
    , sum(Anzahl) as Summe
from (
    select 
        PandemieWoche(Meldedatum) as Pw
        , Altersgruppe as Altersgruppe
        , sum(AnzahlFall) as Anzahl 
    from Faelle
    where 
        IdLandkreis = IdLK
    group by 
          Pw
        , Altersgruppe
    ) as A
group by
    A.Pw;

end
//

DROP PROCEDURE IF EXISTS LandkreisAltersgruppen //

CREATE PROCEDURE LandkreisAltersgruppen (IdLK INT)
BEGIN

    select 
        PandemieWoche(Meldedatum) as Pw
        , Altersgruppe as Altersgruppe
        , sum(AnzahlFall) as Anzahl 
    from Faelle
    where 
        IdLandkreis = IdLK
    group by 
          Pw
        , Altersgruppe

;
end
//

DROP PROCEDURE IF EXISTS CreateKalenderwochen //

CREATE PROCEDURE CreateKalenderwochen()
BEGIN
    
    drop table if exists Kalenderwochen;
    create table if not exists Kalenderwochen (
          Jahr INT
        , Kw INT
        , Pw INT
        , PRIMARY KEY (Jahr, Kw));
    
    set @f := 0;
    FOR j IN 2020..2021
    DO
    FOR k IN 1..53
    DO
        insert into Kalenderwochen values (j,k,@f:=@f+1);
    END FOR;
    END FOR;
    
END
//

DROP PROCEDURE IF EXISTS UpdateFallzahlenLandkreise //

CREATE PROCEDURE UpdateFallzahlenLandkreise ()
BEGIN
    
    create table if not exists FallzahlenLandkreis
    ( 
        IdLandkreis INT
        , Jahr INT
        , Kw INT
        , Pw INT
        , AnzahlFall BIGINT
        , AnzahlTodesfall BIGINT
        , primary key (IdLandkreis,Jahr,Kw))
    select 
        IdLandkreis
        , Jahr
        , Kw
        , Pw
        , 0 as AnzahlFall
        , 0 as AnzahlTodesfall
    from Landkreis, Kalenderwochen;
    
    replace into FallzahlenLandkreis
    select
        IdLandkreis as IdLandkreis
        , ( case when week(Meldedatum,3) = 53 then 2020 else year(Meldedatum) end ) as Jahr
        , week(Meldedatum,3) as Kw
        , PandemieWoche(Meldedatum) as Pw
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle as F
    join Kalenderwochen as K
    on 
        PandemieWoche(Meldedatum) = K.Pw
    group by 
          IdLandkreis
        , Pw
    ;


END
//

DROP PROCEDURE IF EXISTS Landkreise_Plus //

DROP PROCEDURE IF EXISTS Landkreise //

CREATE PROCEDURE Landkreise ( From_Kw INT, To_Kw INT )
BEGIN

    DROP TABLE IF EXISTS Tendenz;
    CREATE TEMPORARY TABLE Tendenz
    ( Jahr INT 
    , Kw INT 
    , Pw INT 
    , Anzahl INT
    , Tendenz CHAR(4) DEFAULT 'More')
 
    -- Ohne Fälle   
    ( select 
        K.Jahr as Jahr
        , K.Kw as Kw
        , K.Pw as Pw 
        , case when F.Lk is NULL then 0 else F.Lk end as Anzahl
        , 'Zero' as Tendenz

    from Kalenderwochen as K 
    left join (
        select 
            Jahr
            , Kw
            , Pw
            , count(IdLandkreis) as Lk
        from FallzahlenLandkreis
        where Anzahlfall=0
        group by 
            Pw
    ) as F
    on 
        K.Pw = F.Pw
    group by 
        Pw
    order by 
        Pw
        )
    
    union
    
    ( select 
        L1.Jahr as Jahr
        , L1.Kw as Kw
        , L1.Pw as Pw
        , count(L1.IdLandkreis) as Anzahl
        , 'LEQ' as Tendenz
    from FallzahlenLandkreis as L1
    join FallzahlenLandkreis as L2
    on L1.IdLandkreis = L2.IdLandkreis
    and L1.Pw = L2.Pw + 1
    where 
        L1.AnzahlFall <= L2.AnzahlFall
    group by
        L1.Pw
    order by L1.Jahr, L1.Kw
    )
        
    union
    
    (select 
        L1.Jahr as Jahr
        , L1.Kw as Kw
        , L1.Pw as Pw
        , count(L1.IdLandkreis) as Anzahl
        , 'GT' as Tendenz
    from FallzahlenLandkreis as L1
    join FallzahlenLandkreis as L2
    on L1.IdLandkreis = L2.IdLandkreis
    and L1.Pw = L2.Pw + 1
    where L1.AnzahlFall > L2.AnzahlFall
    
    group by
        L1.Pw
    order by L1.Jahr, L1.Kw
    );
    
    select 
        Jahr
        , Kw
        , sKw
        , sum(Zero) as Zero
        , sum(LEQ) as LEQ
        , sum(GT) as GT
    from ( 
        select 
            Jahr
            , Kw
            , Pw
            , case when Tendenz = 'Zero' then Anzahl else 0 end as Zero
            , case when Tendenz = 'LEQ' then Anzahl else 0 end as LEQ
            , case when Tendenz = 'GT' then Anzahl else 0 end as GT
        from Tendenz 
        group by 
            Jahr
            , Kw
            , Pw
            , Tendenz
        ) as T 
    where 
        Pw >= From_Kw
        and Pw <= To_Kw
    group by 
        Jahr
        , Kw
        , Pw;
   
end
//

DROP PROCEDURE IF EXISTS LandkreisePw //

CREATE PROCEDURE LandkreisePw ()
BEGIN
 
    DROP TABLE IF EXISTS FaelleLandkreisPw;
    
    CREATE TABLE FaelleLandkreisPw
    ( IdLandkreis INT 
    , Pw INT 
    , AnzahlFall BIGINT(20)
    , AnzahlTodesfall BIGINT(20)
    , PRIMARY KEY (IdLandkreis,Pw) )
    (
    SELECT
        IdLandkreis as IdLandkreis
        , PandemieWoche(Meldedatum) as Pw
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle 
    group by 
        IdLandkreis, Pw
    )
    ;    

END
//

DROP PROCEDURE IF EXISTS Landkreis714 //

CREATE PROCEDURE Landkreis714 ( MD DATE)
BEGIN

drop table if exists l7;

create temporary table l7 (
    select 
        F.IdLandkreis
        , sum(F.AnzahlFall) as Woche
    from Faelle as F 
    where 
        F.Meldedatum <= MD 
        and F.Meldedatum > adddate(MD, -7) 
    group by 
        F.IdLandkreis
    );
    
drop table if exists l14;
create temporary table l14 (
    select 
        F.IdLandkreis
        , sum(F.AnzahlFall) as Woche
    from Faelle as F
    where 
        F.Meldedatum <= adddate(MD, - 7) 
        and F.Meldedatum > adddate(MD, -14) 
    group by 
        F.IdLandkreis
    );
        
select 
    B.IdBundesland
    , B.Bundesland
    , F1.IdLandkreis
    , L.Landkreis
    , L.EW_insgesamt
    , F1.Woche as Woche
    , F2.Woche as Vorwoche 
from l7 as F1 
join l14 as F2
on
    F1.IdLandkreis = F2.IdLandkreis
join 
    Landkreis as L
on
    F1.IdLandkreis = L.IdLandkreis
join 
    Bundesland as B
on
    F1.IdLandkreis div 1000 = B.IdBundesland;

END

//

DROP PROCEDURE IF EXISTS LandkreisUp //

CREATE PROCEDURE LandkreisUp ( MD DATE)
BEGIN

drop table if exists l7;

create temporary table l7 (
    select 
        F.IdLandkreis
        , sum(F.AnzahlFall) as Woche
    from Faelle as F 
    where 
        F.Meldedatum <= MD 
        and F.Meldedatum > adddate(MD, -7) 
    group by 
        F.IdLandkreis
    );
    
drop table if exists l14;

create temporary table l14 (
    select 
        F.IdLandkreis
        , sum(F.AnzahlFall) as Woche
    from Faelle as F
    where 
        F.Meldedatum <= adddate(MD, - 7) 
        and F.Meldedatum > adddate(MD, -14) 
    group by 
        F.IdLandkreis
    );
        
select 
    count(*) 
from l7 as F1 
join l14 as F2
on
    F1.IdLandkreis = F2.IdLandkreis
where
    F1.Woche > F2.Woche
union
select 
    count(*) 
from l7 as F1 
join l14 as F2
on
    F1.IdLandkreis = F2.IdLandkreis
where
    F1.Woche = F2.Woche
union
select 
    count(*) 
from l7 as F1 
join l14 as F2
on
    F1.IdLandkreis = F2.IdLandkreis
where
    F1.Woche < F2.Woche
;
END

//

delimiter ;
