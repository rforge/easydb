library( "easydb" )


## Matrix or data.frame case
m <- matrix( data = NA_integer_, nrow = 21, ncol = 2 ) 
easydb:::.splitBySize( x = m, size = 5 ) 


## Vector case
v <- rep( NA_integer_, times = 21 ) 
easydb:::.splitBySize( x = v, size = 5 ) 

