use COVID19;

delimiter //

drop procedure if exists CompareCountries //

create procedure CompareCountries (TCC INT)
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
      WD.CountryCode as CountryCode
    , C.Region as Region
    , (case when WD.Altersgruppe = 100 then 90 else WD.Altersgruppe div 10 * 10 end ) as Altersgruppe
    , round(sum(WD.Anzahl),0) as OrigTodesfallzahl
    , round(sum(WD.Anzahl / PSCC.Anzahl * PTCC.Anzahl) / @TBEV * 1e5,2) as VergleichsTodesfallzahl
    , round(sum(WDT.Anzahl),0) as Todesfallzahl
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

drop procedure if exists Compare2Countries //

create procedure Compare2Countries (TCC INT, SCC INT)
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
      WD.CountryCode as CountryCode
    , C.Region as Region
    , (case when WD.Altersgruppe = 100 then 90 else WD.Altersgruppe div 10 * 10 end ) as Altersgruppe
    , round(sum(WD.Anzahl),0) as OrigTodesfallzahl
    , round(sum(WD.Anzahl / PSCC.Anzahl * PTCC.Anzahl),0) as VergleichsTodesfallzahl
    , round(sum(WDT.Anzahl),0) as Todesfallzahl
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
    and ( WD.CountryCode = SCC or WD.CountryCode = TCC)
group by
    WD.CountryCode
   , Altersgruppe
    ;

end //

drop procedure if exists CompareCountriesSum //

create procedure CompareCountriesSum (TCC INT)
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
--      WD.CountryCode as CountryCode
      C.Region as Region
--    , round(sum(WD.Anzahl),0) as OrigTodesfallzahl
    , round(sum(WD.Anzahl / PSCC.Anzahl * PTCC.Anzahl) / @TBEV * 100000,1) as VergleichsTodesfallzahl
--    , round(sum(WDT.Anzahl),0) as Todesfallzahl
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
    ;

end //


drop procedure if exists GetRegion //

create procedure GetRegion (CC INT)
begin

select
    CountryCode as CountryCode
    , Region as Region
from 
  WPP.WPP20191C
where
    CountryCode = CC
    ;

end //

delimiter ;

call CompareCountries (276);
call Compare2Countries (276,752);
call CompareCountriesSum(276);
call GetRegion(840);
