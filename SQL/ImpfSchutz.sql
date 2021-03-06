use RKI;

create or replace view ImpfDSchutz as 
select 
    week(Woche,3) as Woche
    , case when AlterBis < 100 
        then concat ("A",AlterVon,"-A",AlterBis) 
        else concat ("A",AlterVon,"+" ) 
        end 
      as Altersgruppe
    , B.IdOutcome
    , O.Outcome
    , G_Quote as G_Quote
    , U_Quote as U_Quote
    , G_Anzahl as G_Anzahl
    , U_Anzahl as U_Anzahl
    , 1 -(U_Quote/G_Quote)*(G_Anzahl/U_Anzahl) as Schutz
from (
    select 
    Woche
    , AlterVon
    , AlterBis
    , IdOutcome
    , sum(G_Quote) as G_Quote
    , sum(U_Quote) as U_Quote
    , sum(G_Anzahl) as G_Anzahl
    , sum(U_Anzahl) as U_Anzahl
from (
    select 
        I1.Woche as Woche
        , I1.AlterVon as AlterVon
        , I1.AlterBis as AlterBis
        , I1.IdOutcome as IdOutcome
        , case when I1.IdGruppe = "G" then I2.Anzahl4W else 0 end as G_Quote
        , case when I1.IdGruppe = "U" then I2.Anzahl4W else 0 end as U_Quote
        , case when I1.IdGruppe = "G" then I1.Anzahl4W else 0 end as G_Anzahl
        , case when I1.IdGruppe = "U" then I1.Anzahl4W else 0 end as U_Anzahl
    from ImpfD as I1 
    join ImpfD as I2 
    on 
        I1.Woche=I2.Woche
        and I1.AlterVon = I2.AlterVon 
        and I1.AlterBis = I2.AlterBis
        and I1.IdGruppe = I2.IdGruppe
    where 
        I2.IdOutcome = "Q" 
        and I1.IdGruppe <> "A"
        and I1.IdOutcome <> "Q"
) as A
group by
    Woche
    , AlterVon
    , AlterBis
    , IdOutcome
) as B
join ImpfDOutcome as O
on B.IdOutcome = O.IdOutcome
order by Woche, Altersgruppe
;

delimiter //

drop procedure if exists UpdateImpfQuote //

create procedure UpdateImpfQuote ( W DATE, Q1 DOUBLE, Q2 DOUBLE, Q3 DOUBLE)
begin
    insert into ImpfD values
        (W, 12, 17, 'Q', 'A', 100, 100)
        , (W, 12, 17, 'Q', 'G', Q1, Q1)
        , (W, 12, 17, 'Q', 'U', 100 - Q1, 100 - Q1)
        , (W, 18, 59, 'Q', 'A', 100, 100)
        , (W, 18, 59, 'Q', 'G', Q2, Q2)
        , (W, 18, 59, 'Q', 'U', 100 - Q2, 100 - Q2)
        , (W, 60, 100, 'Q', 'A', 100, 100)
        , (W, 60, 100, 'Q', 'G', Q3, Q3)
        , (W, 60, 100, 'Q', 'U', 100 - Q3, 100 - Q3)
    ;
end 
//

delimiter ;
