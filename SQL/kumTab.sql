use RKI;

set @f:=0;
set @t:=0;

select 
    Meldedatum
    , @f:=AnzahlFall + @f as kumFall
    , @t:=AnzahlTodesfall + @t as kumTodesfall
    , FORMAT(@t / @f,6) as CFR
from (
select 
    Meldedatum
    , sum(AnzahlFall) as AnzahlFall
    , sum(AnzahlTodesfall) as AnzahlTodesfall
from
    Faelle
group by
    Meldedatum
order by 
    Meldedatum
) as F
;
