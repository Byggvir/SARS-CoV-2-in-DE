-- SQL
use COVID19;

delimiter //

drop procedure if exists ExportDeaths //

create procedure ExportDeaths ()
begin
set @y := (select max(Jahr) from RKI_DeathsByAgeKw);
set @k := (select max(Kw) from RKI_DeathsByAgeKw where Jahr = @y);

select 'CountryCode','Sex' as Sex,'Altersgruppe' as AgeGroup,'Bis','Todesfall'
union
( select 276,case when Sex=1 then 'F' else 'M' end as Sex,AgeGroup,Agegroup+9,Count 
from RKI_DeathsByAgeKw 
where 
    Jahr = @y 
    and Kw = @k
union
select 276,'B' as Sex,AgeGroup,Agegroup+9,sum(Count) 
from RKI_DeathsByAgeKw 
where 
    Jahr = @y 
    and Kw = @k
group by
    AgeGroup
order by
    Sex, AgeGroup
)    
into OUTFILE '/tmp/276.csv' FIELDS TERMINATED by ',';


end
//

delimiter ;

call ExportDeaths();
