use COVID19;

delimiter //

drop procedure if exists CompareCountryDEU //

create procedure CompareCountryDEU (TCC INT)
begin

set @TBEV := (select
        sum(Anzahl) as Anzahl
    from 
        WPP.WPP20191
    where
        CountryCode = TCC
        and Jahr = 2019
        ) 
;


select
      WD.CountryCode 
    , C.Region
    , WD.Altersgruppe div 10 * 10 as Altersgruppe
    , round(sum(WD.Anzahl),0) as O_Anzahl
    , round(sum(WD.Anzahl / PSCC.Anzahl * PTCC.Anzahl),0) as S_Anzahl
    , round(sum(WDT.Anzahl),0) as T_Anzahl
from 
    WorldDeaths as WD
join 
    WorldDeaths as WDT
on
    WD.Altersgruppe = WDT.Altersgruppe
    and WD.Sex = WDT.Sex
join
    WPP.WPP20191 as PSCC
on
    WD.CountryCode = PSCC.CountryCode
    and WD.Altersgruppe = PSCC.Altersgruppe
join 
    WPP.WPP20191C as C
on 
    WD.CountryCode = C.CountryCode
join 
    WPP.WPP20191 as PTCC
on
    WD.Altersgruppe = PTCC.Altersgruppe
where
    
    PTCC.CountryCode = TCC
    and PTCC.Jahr = 2019
    and PSCC.Jahr = 2019
    and WDT.CountryCode = TCC
    and WD.Sex = 'B'
group by
    WD.CountryCode
    , Altersgruppe
    ;

end //

delimiter ;

call CompareCountryDEU (276);
