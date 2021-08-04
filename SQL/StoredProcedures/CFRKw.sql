use COVID19;

delimiter //

drop procedure if exists CFRKw //

create procedure CFRKw (_Jahr INT, _Kw INT)
begin

select 
    A.Jahr,
    A.Kw,
    A.AgeGroup div 10 *10 as AgeGroup,
    sum(A.CumulatedCount) as Cases,
    C.Deaths as Deaths,
    round(C.Deaths
    / sum(A.CumulatedCount)*100,3) as CFR
    
from RKI_CasesByAge as A 
join ( 
    select 
        B.Jahr,
        B.Kw,
        B.AgeGroup,
        sum(Count) as Deaths
    from RKI_DeathsByAgeKw as B
    group by Jahr,Kw, AgeGroup 
    )
    as C 
on 
    A.Jahr = C.Jahr
    and A.Kw = C.Kw
    and A.AgeGroup = C.AgeGroup
where
    A.Jahr = _Jahr
    and A.Kw = _Kw
group by 
    A.Jahr, A.Kw,AgeGroup div 10 * 10
;
end
//

delimiter ;
