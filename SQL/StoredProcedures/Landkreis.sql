USE RKI;

delimiter //

DROP PROCEDURE IF EXISTS LandkreisTable //

CREATE PROCEDURE LandkreisTable (IdLK INT)
BEGIN

select 
      Kw
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
        ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) as Kw
        , Altersgruppe as Altersgruppe
        , sum(AnzahlFall) as Anzahl 
    from Faelle
    where 
        IdLandkreis = IdLK
    group by 
          Kw
        , Altersgruppe
    ) as A
group by
    A.Kw
having Kw >= 35
;
end
//

DROP PROCEDURE IF EXISTS LandkreisAltersgruppen //

CREATE PROCEDURE LandkreisAltersgruppen (IdLK INT)
BEGIN

    select 
        ( case when Meldedatum > "2021-01-03" then 53+week(Meldedatum,3) else week(Meldedatum,3) end ) as Kw
        , Altersgruppe as Altersgruppe
        , sum(AnzahlFall) as Anzahl 
    from Faelle
    where 
        IdLandkreis = IdLK
    group by 
          Kw
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
        , sKw INT
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
        , sKw INT
        , AnzahlFall BIGINT
        , AnzahlTodesfall BIGINT
        , primary key (IdLandkreis,Jahr,Kw))
    select 
        IdLandkreis
        , Jahr
        , Kw
        , sKw
        , 0 as AnzahlFall
        , 0 as AnzahlTodesfall
    from Landkreis, Kalenderwochen;
    
    replace into FallzahlenLandkreis
    select
        IdLandkreis as IdLandkreis
        , ( case when week(Meldedatum,3) = 53 then 2020 else year(Meldedatum) end ) as Jahr
        , week ( Meldedatum,3 ) as Kw
        , K.sKw as sKw
        , sum(AnzahlFall) as AnzahlFall
        , sum(AnzahlTodesfall) as AnzahlTodesfall
    from Faelle as F
    join Kalenderwochen as K
    on 
        ( case when week(Meldedatum,3) = 53 then 2020 else year(Meldedatum) end ) = K.Jahr
        and week(Meldedatum,3) = K.Kw
    group by 
          IdLandkreis
        , Kw
        , sKw
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
    , sKw INT 
    , Anzahl INT
    , Tendenz CHAR(4) DEFAULT 'More')
 
    -- Ohne Fälle   
    ( select 
        K.Jahr as Jahr
        , K.Kw as Kw
        , K.sKw as sKw 
        , case when F.Lk is NULL then 0 else F.Lk end as Anzahl
        , 'Zero' as Tendenz

    from Kalenderwochen as K 
    left join (
        select 
            Jahr
            , Kw
            , sKw
            , count(IdLandkreis) as Lk
        from FallzahlenLandkreis
        where Anzahlfall=0
        group by 
            sKw
    ) as F
    on 
        K.sKw = F.sKw
    group by 
        sKw
    order by 
        sKw
    )
    
    union
    
    ( select 
        L1.Jahr as Jahr
        , L1.Kw as Kw
        , L1.sKw as sKw
        , count(L1.IdLandkreis) as Anzahl
        , 'LEQ' as Tendenz
    from FallzahlenLandkreis as L1
    join FallzahlenLandkreis as L2
    on L1.IdLandkreis = L2.IdLandkreis
    and L1.sKw = L2.sKw + 1
    where 
        L1.AnzahlFall <= L2.AnzahlFall
    group by
        L1.sKw
    order by L1.Jahr, L1.Kw
    )
        
    union
    
    (select 
        L1.Jahr as Jahr
        , L1.Kw as Kw
        , L1.sKw as sKw
        , count(L1.IdLandkreis) as Anzahl
        , 'GT' as Tendenz
    from FallzahlenLandkreis as L1
    join FallzahlenLandkreis as L2
    on L1.IdLandkreis = L2.IdLandkreis
    and L1.sKw = L2.sKw + 1
    where L1.AnzahlFall > L2.AnzahlFall
    
    group by
        L1.sKw
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
            , sKw
            , case when Tendenz = 'Zero' then Anzahl else 0 end as Zero
            , case when Tendenz = 'LEQ' then Anzahl else 0 end as LEQ
            , case when Tendenz = 'GT' then Anzahl else 0 end as GT
        from Tendenz 
        group by 
            Jahr
            , Kw
            , sKw
            , Tendenz
        ) as T 
    where 
        sKw >= From_Kw
        and sKw <= To_Kw
    group by 
        Jahr
        , Kw
        , sKw;
   
end
//

delimiter ;
