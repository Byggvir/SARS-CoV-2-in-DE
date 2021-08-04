use COVID19;

delimiter //

drop procedure if exists CompareCasesPerDay //

create procedure CompareCasesPerDay (dFrom DATE, dUntil DATE)
begin
    
    drop table if exists rki_kum;
    drop table if exists rki_meldedatum;
    drop table if exists rki_refdatum;
    drop table if exists jhu_t;
    drop table if exists rki_dates;
    drop table if exists rki_meldedatum;
    drop table if exists rki_refdatum;
    
    create temporary table rki_kum 
        ( Date DATE primary key, Cases INT, Deaths INT)
    select 
            t1.date as Date
            , t1.cases - t2.cases as Cases
            , t1.deaths - t2.deaths as Deaths
        from rki as t1 
        inner join rki as t2 
        on t1.date=adddate(t2.date,1)
        where t1.cases > t2.cases
    ;
    
    create temporary table rki_dates 
        ( Datum DATE primary key)
    select distinct
            Meldedatum as Datum
    from RKIFaelle
    ;
   
    create temporary table rki_meldedatum 
        ( Date DATE  primary key, Cases INT )
    select 
            t1.Meldedatum as Date
            , sum(AnzahlFall) as Cases
        from RKIFaelle as t1
        where NeuerFall > -1
        group by t1.Meldedatum
    ;

    create temporary table rki_refdatum
        ( Date DATE primary key, Cases INT )
    select 
            t1.Refdatum as Date
            , sum(AnzahlFall) as Cases
        from RKIFaelle as t1 
        where NeuerFall > -1 and IstErkrankungsbeginn = 1
        group by t1.Refdatum
    ;
    
    create temporary table jhu_t 
        ( Date DATE primary key, Cases INT, Deaths INT )
    select 
            t1.reported as Date
            , t1.cases - t2.cases as Cases
            , t1.deaths - t2.deaths as Deaths
        from jhu as t1 
        inner join jhu as t2 
        on t1.reported = adddate(t2.reported,1) and t1.cid = t2.cid
        where t1.cases > t2.cases and t1.cid = 131
    ;
    select 
        a.Date
        , a.Cases as Kum_Cases
        , b.Cases as Melde_Cases
        , c.Cases as Ref_Cases
        , d.Cases as  JHU_Cases
--         , a.Deaths as Kum_Deaths
--         , b.Deaths as Melde_Deaths
--         , c.Deaths as Ref_Deaths
--         , d.Deaths as JHU_Deaths
    from rki_kum as a
    left join rki_meldedatum as b
    on a.Date = b.Date
    left join rki_refdatum as c
    on a.Date = c.Date
    left join jhu_t as d
    on a.Date = d.Date
    where a.Date >= dFrom and a.Date <= dUntil
    ;
    
end
//

delimiter ;
 
