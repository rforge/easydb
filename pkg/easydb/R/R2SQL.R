


.funR2sql <- function(# Converts R or SQL functions name into SQL function names
### Converts R or SQL functions name into SQL function names.

 x, 
### Vector of character strings. Either name of SQL functions, or 
### names of R function that can be translated into SQL functions.
 
 equiv = c( "mean" = "AVG", "AVG" = "AVG", "sum" = "SUM", 
    "length" = "COUNT", "COUNT" = "COUNT", "nchar" = "LEN", 
    "LEN" = "LEN", "max" = "MAX", "min" = "MIN", "tolower" = "LCASE", 
    "LCASE" = "LCASE", "touper" = "UCASE", "UCASE" = "UCASE", 
    "FIRST" = "FIRST", "LAST" = "LAST", "unique" = "DISTINCT", 
    "DISTINCT" = "DISTINCT" ) 
### Vector of labelled character strings, following the pattern 
### \code{"originalName" = "SQLName"}.

){  
    ## All uppercase 
    u.x <- toupper(x) 
    
    ## Uppercase names
    names( equiv ) <- toupper( names( equiv ) )
    n.equiv <- names( equiv )  
    
    ## Test if all input functions can be found in 'equiv'
    text.x <- !(u.x %in% n.equiv)
    
    if( any( text.x ) ){ 
        stop( sprintf( 
            "Some function names can not be converted to SQL: %s", 
            paste( x[ text.x ], collapse = ", " ) 
        ) ) 
    }   
    
    ## Converts & return
    return( as.character( equiv[ u.x ] ) ) 
### Returns a vector of SQL functions names.
}   

# ## Example 
# .funR2sql( c("mean","AVG","length","COUNT") ) 

# ## Does not work:
# try( .funR2sql( c("mean","sd") ) )






.noQuote <- function(# Remove all quotes from a character string
### Remove all quotes from a character string

 x
### Vector of character strings from which quotes will be removed.
 
){  
    x <- gsub(pattern = "'", replacement = "", x = x ) 
    x <- gsub(pattern = '"', replacement = "", x = x ) 
    x <- gsub(pattern = '\\[', replacement = "", x = x ) 
    x <- gsub(pattern = '\\]', replacement = "", x = x ) 
    x <- gsub(pattern = '`', replacement = "", x = x ) 
    
    return( x ) 
### Return a vector of unquted character strings
}   

# x <- c('"hello"',"'hello'","[hello]","`hello`","hello")
# .noQuote( x )  






.checkColNames <- function(# Check if the column names provided exists in a list of known column names
### Check if the column names provided exists in a list of known column names

 termz, 
### Vector of character strings. Column names provided by the user.

 refCol=NULL, 
### Vector of character strings. If not \code{NULL}, column names 
### expected in the database table. Will be used to check the 
### column names provided.

 errMsg="Some names in 'termz' do not match with names in 'refCol' (%s)" 
### Single character string. Error message passed to \code{sprintf}.

){  
    if( !is.null( refCol ) ){ 
        # termz <- .noQuote( termz )
        
        testCol <- termz %in% refCol 
        
        if( !all( testCol ) ){ 
            stop( sprintf( 
                errMsg, paste( termz[ !testCol ], collapse = ", " )
            ) ) 
        }   
    }   
}   




.call2sql <- function(# Converts an R call to an SQL functions
### Converts an R call to an SQL functions.

 expr, 
### A single, un-evaluated, R call.
 
 name = "", 
### Vector of character strings. Name of the output column (will 
### be \code{AS `name`} in SQL).

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
    if( length( expr ) == 1 ){ 
        termz <- .noQuote( as.character( expr ) )   
        
        .checkColNames( termz = termz, refCol = refCol, 
            errMsg = errMsg ) 
        
        e <- paste0( colQ[1], termz, colQ[2] ) 
    }else{ 
        isCall <- class( expr[[2]] ) == "call" 
        
        if( isCall ){ 
            expr[[2]] <- .call2sql( expr = expr[[2]], refCol = refCol, 
                errMsg = errMsg, colQ = colQ ) 
            
            colQ <- c("","") 
        }   
        
        ## Extract the function's name from the call
        first <- as.character( expr[[1]] ) 
        
        ## Case, some arithmetic expressions are provided
        if( first %in% c("+","-","/","*") ){ 
            ## Trim quotes: 
            expr2 <- unlist( as.list( expr[-1] ) ) 
            
            expr2 <- lapply( 
                X   = expr2, 
                FUN = function(X){ 
                    if( class( X ) == "call" ){ 
                        X <- .call2sql( expr = X, refCol = refCol, 
                            errMsg = errMsg, colQ = colQ )
                    }else{ 
                        X <- as.character( X ) 
                        
                        if( !isCall ){ 
                            X <- .noQuote( X ) 
                            
                            .checkColNames( termz = X, refCol = refCol, 
                                errMsg = errMsg ) 
                        }   
                        
                        X <- paste0( colQ[1], X, colQ[2] ) 
                    }   
                    
                    return( X ) 
                }   
            )   
            
            # termz <- as.character( expr2 ) 
            # if( !isCall ){ 
            #     termz <- .noQuote( termz ) 
            # }   
            
            # .checkColNames( termz = termz, refCol = refCol, 
            #     errMsg = errMsg ) 
            
            # e <- paste0( colQ[1], termz, colQ[2] ) 
            
            e <- paste( expr2, collapse = first ) # e
            
        ## Case, 'regular' SQL functions:
        }else{ 
            ## R function -> SQL
            first <- .funR2sql( first ) 
            
            # if( first == "SUM" ){ browser() } 
            
            ## Trim quotes:
            expr2 <- unlist( as.list( expr[-1] ) ) 
            
            termz <- as.character( expr2 ) 
            if( !isCall ){ 
                termz <- .noQuote( termz ) 
                
                .checkColNames( termz = termz, refCol = refCol, 
                    errMsg = errMsg ) 
            }   
            
            # .checkColNames( termz = termz, refCol = refCol, 
            #     errMsg = errMsg ) 
            
            # Bind function and arguments
            e <- paste0( colQ[1], termz, colQ[2] ) 
            e <- paste( e, collapse = ", " ) 
            e <- paste0( first, "(", e, ")" ) 
        }   
    }   
    
    if( name != "" ){ 
        e <- paste0( e, " AS `", name, "`" ) 
    }   

    return( e )    
}     

# .call2sql(  r[[2]][[2]] ) 
# .call2sql(  r[[1]] ) 
# .call2sql(  r[[3]] ) 






.expression2sql <- function(#
### 

 expr, 
### A vector of un-evaluated, R call.

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
    nExpr <- names(expr) 

    if( is.null( nExpr ) ){ 
        nExpr <- rep( "", length(expr) )
    }   

    e <- lapply(
        X   = 1:length(expr), 
        FUN = function(X,expr){ 
            if( length( expr[[X]] ) == 1 ){ 
                clExprX <- class( expr[[X]] ) 
            }else{ 
                clExprX <- class( expr[[X]][[1]] ) 
            }   
            
            if( clExprX == "name" ){ 
                e <- .call2sql( expr[[X]], name = nExpr[X], 
                    refCol = refCol, errMsg = errMsg, colQ = colQ ) 
            }else{ 
                e <- .expression2sql( expr[[X]], refCol = refCol, 
                    errMsg = errMsg, colQ = colQ ) 
            }      

            return( e )
        },  
        expr = expr  
    )   
    
    e <- unlist ( e ) 
    e <- paste( e, collapse = ", " )

    return( e ) 
### 
}   

# r2 <- .expression2sql( r )
# r2 





.sCol.format <- function( 
 
 sCol, 
### Result of \code{substitute( sCol )}.

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
    sCol <- eval( call( 
        "substitute", 
        sCol, 
        list( 
            "list" = as.name("expression"), 
            "c"    = as.name("expression") 
        ) 
    ) )   
    ## Call to an expression -> expression
    sCol <- eval( sCol ) 
    
    ## R -> SQL
    sCol <- .expression2sql( sCol, refCol, errMsg = errMsg, 
        colQ = colQ ) 
    
    return( sCol ) 
}   


