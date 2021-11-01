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

create or replace
function sigma_rel ( n INT, k INT) returns DOUBLE

begin
  
  if ( n != 0  and k <= n ) then
  
  	return sqrt(k * (n-k) / n) / n ;

  else
  	return 0;
  end if;

end
//
create or replace
function sigma ( n INT, k INT) returns DOUBLE

begin
  
  if ( n != 0  and k <= n ) then
  
  	return sqrt(k * (n-k) / n) ;

  else
  	return 0;
  end if;

end
//

delimiter ;
