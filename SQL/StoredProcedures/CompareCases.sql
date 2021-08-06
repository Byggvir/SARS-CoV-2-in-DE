use COVID19;

delimiter //

drop procedure if exists CompareCases //

create procedure CompareCases (dFrom DATE, dTo DATE)
begin

    set @f1 := 0 ;
    set @f2 := 0 ;

    select 
        R1.date as Datum
        , R1.cases as MeldungenKumuliert 
        , R2.cases2 as AnzahlBisMeldedatum 
        , R3.cases3 as AnzahlBisReferenzdatum
        , R4.cases4 as AnzahlJHUbisDatum
    from rki as R1
    join  
    ( select 
        Meldedatum as Date2
        , (@f1:=@f1+cases21) as cases2
        from (
            select 
                Meldedatum
                , sum(AnzahlFall) as cases21
            from Faelle 
            group by Meldedatum ) as R21 ) as R2
    on R1.date = R2.Date2
    join
    ( select 
        Refdatum as Date3
        , (@f2:=@f2+cases31) as cases3
        from (
            select 
                Refdatum
                , sum(AnzahlFall) as cases31
            from Faelle 
            group by Refdatum ) as R31 ) as R3
    on R1.date = R3.Date3
    join
    ( select 
        reported as Date4
        , cases as cases4
    from jhu 
    where cid = 131 ) as R4
    on R1.Date = R4.Date4
    having R1.date >= dFrom and R1.date <= dTo

    ;
end
//
delimiter ;
 
