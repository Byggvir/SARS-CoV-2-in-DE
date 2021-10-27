use RKI;

delimiter //

-- Berechnen der Pandemiewaoch. 
-- Die erste Pandemiewoche ist die 1. Woche 2020, 
-- auch wenn die ersten Fälle in DEU in Woche 9 und 10 auftraten.

create or replace 
function PandemieWoche ( Datum DATE ) returns INT
begin
      return datediff(Datum,"2019-12-30") div 7 + 1;
end
//
