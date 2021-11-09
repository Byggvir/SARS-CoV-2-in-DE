use RKI;

delimiter //

drop procedure if exists CreateFaelleBL //

create procedure CreateFaelleBL ()
begin

drop table if exists FaelleBL;

create table FaelleBL 
    ( Meldedatum DATE
    , IdBundesland INT  DEFAULT 0
    , Altersgruppe CHAR(8)
    , AnzahlFall INT  DEFAULT 0
    , AnzahlFallKum INT  DEFAULT 0
    , AnzahlTodesfall INT  DEFAULT 0
    , AnzahlTodesfallKum INT DEFAULT 0
    , primary key(
        Meldedatum
        , IdBundesland
        , Altersgruppe
        )
    ) 
    select 
            Meldedatum as Meldedatum
            , IdLandkreis div 1000 as IdBundesland
            , Altersgruppe as Altersgruppe
            , sum(AnzahlFall) as AnzahlFall
            , 0 as AnzahlFallKum
            , sum(AnzahlTodesfall) as AnzahlTodesfall
            , 0 as AnzahlTodesfallKum
            from Faelle as F1 
            group by 
                Meldedatum
                , IdLandkreis div 1000
                , Altersgruppe 
        union 
        select 
            Meldedatum as Meldedatum
            , 0 as IdBundesland
            , Altersgruppe as Altersgruppe
            , sum(AnzahlFall) as AnzahlFall
            , 0 as AnzahlFallKum
            , sum(AnzahlTodesfall) as AnzahlTodesfall
            , 0 as AnzahlTodesfallKum
            from Faelle as F1 
            group by 
                Meldedatum
                , Altersgruppe
    ;
    
    -- lock tables FaelleBL write wait 30;
    
    update FaelleBL as A 
        set AnzahlFallKum = ( 
            select 
                sum(B.AnzahlFall) 
            from FaelleBL as B 
            where 
                B.Meldedatum <= A.Meldedatum 
                and A.IdBundesland = B.IdBundesland 
                and A.Altersgruppe = B.Altersgruppe
            )  
    ;
        
    update FaelleBL as A 
        set  
            AnzahlTodesfallKum = ( 
            select 
                sum(B.AnzahlTodesfall) 
            from FaelleBL as B 
            where 
                B.Meldedatum <= A.Meldedatum 
                and A.IdBundesland = B.IdBundesland 
                and A.Altersgruppe = B.Altersgruppe
            )
            
    ; 
    
--    unlock tables;
end
//

delimiter ;
