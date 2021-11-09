# sed: Aufbereiten der mit Okular exportierten Tabelle
s#\.##g;
/Impfquote/ s#%##g;
/Impfquote/ s#,#.#g;

s#/[0-9]*%##g;
1,2d;
7,10d;
/%/d;
s#[^;]*;##; 
s# ##g;

