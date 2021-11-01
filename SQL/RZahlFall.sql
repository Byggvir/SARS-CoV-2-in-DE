use RKI;

set @DATUM := "2021-10-26";

select 
    'Deutschland' as Bezeichnung
    , 'A0' as Altersgruppe
    , round(R,3) as R
    , round(Rlow,3) as Rlow
    , round(Rhigh,3) as Rhigh
    , T.AnzahlFall as AnzahlFall
from RZahl as R 
join FaelleProTag as T 
on 
    R.Datum = T.Meldedatum 
where 
    Datum=@DATUM 
    and Zeitraum = 20 
    and R.Altersgruppe="A0+" 
    and R.IdBundesland = 0
union
select 
    Bundesland as Bezeichnung
    , 'A0' as Altersgruppe
    , round(R,3) as R
    , round(Rlow,3) as Rlow
    , round(Rhigh,3) as Rhigh
    , T.AnzahlFall as AnzahlFall
from RZahl as R 
join Bundesland as B 
on 
    R.IdBundesland = B.IdBundesland
join FaelleProTagBL as T 
on 
    T.IdBundesland = R.IdBundesland 
    and R.Datum = T.Meldedatum
where
    Datum=@DATUM
    and Zeitraum = 20
    and Altersgruppe = 'A0+'
union
select 
    Bundesland as Bezeichnung
    , R.Altersgruppe as Altersgruppe
    , round(R,3) as R
    , round(Rlow,3) as Rlow
    , round(Rhigh,3) as Rhigh
    , T.AnzahlFall as AnzahlFall
from RZahl as R 
join Bundesland as B 
on 
    R.IdBundesland = B.IdBundesland
join FaelleBL as T 
on 
    T.IdBundesland = R.IdBundesland
    and T.Altersgruppe = R.Altersgruppe
    and R.Datum = T.Meldedatum
where
    Datum=@DATUM
    and Zeitraum = 20
;
