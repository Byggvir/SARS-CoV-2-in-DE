s#,\([0-9]\{2,2\}\)-\([0-9]\{2,2\}\),#,\1,\2,# ;
s#,60+,#,60,100,# ;
s#60+#60,100# ;
s#^\([^,]*\),u,#\1,0,# ;
s#,u,#,-1,-1,# ;

