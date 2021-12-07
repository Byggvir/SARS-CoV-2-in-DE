# sed: Aufbereiten der mit Okular exportierten Tabelle
s#\.##g;
1,2d;
/Anteil Impfdurchbrüche/d
s#%##g;
s#[^;]*;##; 
s# ##g;
s#-#0#g;

