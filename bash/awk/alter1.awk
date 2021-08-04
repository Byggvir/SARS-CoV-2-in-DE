BEGIN {
    jahr=2020; 
    i=10; 
    k=10; 
    s=0; 
    UID==0 
    
}
{ 
    if ($1 == UID) 
    { 
        s=s+$2;
        print ( "INSERT INTO RKI_CasesByAge VALUES (" jahr "," i "," $1 "," $2 "," s ");")
        ; 
        i=i+1; 
        if ( i > 53 ) 
        { 
            i=1; 
            jahr=jahr+1;
            
        } 
        
    } else { 
        UID=$1;
        i=10;
        jahr=2020;
        s=0;
        
    } 
    
}
