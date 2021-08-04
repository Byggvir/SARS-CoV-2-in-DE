# Format CSV-table for inpit into awk

1d;
s#,,#,0,#;                              # insert 0 in empty fields
s#^\([0-9]*\),\([0-9]*\),#A;\1;\2\n#;   # start new line
s#,#\n#g;                               # replace every ',' with new line
