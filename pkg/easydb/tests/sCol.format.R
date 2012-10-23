library( "easydb" ) 



tmpFun <- function( 

 sCol, 
 
 refCol=NULL, 
### Vector of character strings. If not \code{NULL}, column names 
### expected in the database table. Will be used to check the 
### column names provided.

 errMsg="Some names in 'termz' do not match with names in 'refCol' (%s)", 
### Single character string. Error message passed to \code{sprintf}. 
### See \code{\link{.checkColNames}}.

 colQ = c("`","`")  #  c("[","]")
### A vector of length 2, with the character string that must preceed 
### and follow the column names. For example, \code{c("`","`")} 
### or \code{c("[","]")}. 

){  
    sCol2 <- substitute( sCol ) 
    
    sCol2 <- easydb:::.sCol.format( sCol2, refCol, errMsg = errMsg, 
        colQ = colQ ) 
    
    return( sCol2 ) 
}   



# Just convert R expressions into SQL expressions
tmpFun( sCol = c( mx = mean(x,y), my = sum("x"+"y"), length('z'), 
    xy = `x` + y, z, zmx = 'z' + mean(`x`), mean( z * sum(x) ) ) ) 

# Also check column names:
tmpFun( sCol = c( mx = mean(x,y), my = sum("x"+"y"), length('z'), 
    xy = `x` + y, z, zmx = 'z' + mean(`x`), mean( z * sum(x) ) ), 
    refCol = c("x","y","z") ) 

## error, z missing:
try( tmpFun( sCol = c( mx = mean(x,y), my = sum("x"+"y"), length('z'), 
    xy = `x` + y, z, zmx = 'z' + mean(`x`), mean( z * sum(x) ) ), 
    refCol = c("x","y") ) ) 

