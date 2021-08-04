BEGIN {
    i=0;
}
{ 
    if ( i != 0 )
    { 
      print (i "," $1)  
    }
    i=i+1;
}
