# source( "/media/JMOEYS_8G2/_R_PACKAGES/easydb/pkg/easydb/R/easydb.R" ) 
# source( "C:/_R_PACKAGES/easydb/pkg/easydb/R/easydb.R" )
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# See the ../DESCRIPTION file for information on the package & 
# terms of use.

# Author: Julien MOEYS, after a VB code by Fredrik Stenemo

# Language & Software environment: R. 
# See ..\DESCRIPTION for more details

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Loading required packages:

# Remove this once the package is "stable" 
# require( "soilmacroutils" ) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #






.onLoad <- function(# Print a welcome message
### Print a welcome message.

 libname, 
### See help(".onLoad")

 pkgname
### See help(".onLoad")

){  #
    packageStartupMessage( "'easydb' package loaded. Type ?edb or help(package='easydb') for examples or help." ) 
### Does not return anything.
}   #






edb <- function(# Create a database description (class 'edb'), to be used by other functions.
### Create a database description (class 'edb'), to be used by 
### other functions. Notice that this is _not_ a database connection, 
### so the function won't give a warning if the database does not 
### exists (yet) or if some parameters are wrong.

##seealso<< \code{link{edbRead}}, \code{link{edbWrite}}, 
## \code{link{edbNames}}, \code{link{edbColnames}}.

 dbType, 
### Single character string. Type of the database to describe, 
### formatted as \code{RPackageName_DatabaseType}. Possible values 
### are \code{RSQLite_SQLite}, \code{RODBC_Access} or 
### \code{RODBC_MySQL}. 

 dbName, 
### Single character string. Name of the database to describe 
### (should include the path to the database if it is not in the 
### working directory. MySQL database have no path of course). 
### Not to be confused with the data source name below.

 dbSourceName="edbDefault", 
### Single character string. Name of the data source (only ODBC 
### databases) for this database.

 dbHost="", 
### Single character string. The IP address of the remote database 
### server (only for RODBC_MySQL).

 dbLogin="", 
### Single character string. Database login (or user name). Only 
### for certain dbType (only for RODBC_MySQL). 

 dbPwd="", 
### Single character string. Database password (eventually user 
### specific). Only for certain dbType (only for RODBC_MySQL). 

 dbPort=integer(0), 
### Single integer. The connexion port to the database (only for 
### RODBC_MySQL). 

 ... 
### Additional named arguments, to be passed to the function used 
### to connect to the database (internally).

){  #
    res <- list( 
        "dbName"       = dbName, 
        "dbLogin"      = dbLogin, 
        "dbPwd"        = dbPwd, 
        "dbSourceName" = dbSourceName, 
        "dbHost"       = dbHost, 
        "dbPort"       = dbPort  
    )   #
    #
    dotDot <- list(...) 
    #
    if( length( dotDot ) != 0 ) 
    {   #
        res <- c( res, dotDot ) 
    }   #
    #
    class( res ) <- c( "edb", dbType ) 
    #
    return( res ) 
}   #






.formatCol <- function(# Convert the columns of a data.frame 
### Convert the columns of a data.frame.

##seealso<<\link{edbRead} 

 x, 
### A data.frame

 formatCol=NULL
### If not NULL, a named list of functions to be applied to certain columns 
### after the data has been extracted from the database. The name of each list 
### item gives the column to process, and the value of each item gives the 
### function that must be applied. For instance 
### formatCol = list("DATE"=as.Date) will apply the function 
### \link{as.Date} to the column "DATE".

){  #
    if( !is.null(formatCol) )
    {   #
        if( !is.list( formatCol ) ) # Test that it is a list 
        {   #
            stop( "'formatCol' must be a list." ) 
        }   #
        #
        namez <- names( formatCol ) 
        #
        if( is.null( namez ) ) # Test that each item has a name
        {   #
            stop( "'formatCol' must be a named list. names(formatCol) can not be NULL." ) 
        }   #
        #
        colz <- colnames(x) 
        #
        # Test that each name in formatCol is a column name:
        testNamez <- namez %in% colz 
        #
        if( any( !testNamez ) )
        {   #
            stop( 
                paste( 
                    sep = "", 
                    "Some names in 'formatCol' are not column names:", 
                    paste( collapse = ", ", namez[ !testNamez ] ) 
                )   #
            )   #
        }   #
        #
        # Test that all item in formatCol is a function:
        testItems <- unlist( lapply( X = formatCol, FUN = class ) ) 
        testItems <- testItems == "function" 
        #
        if( any( !testItems ) )
        {   #
            stop( "Values in 'formatCol' must be functions." ) 
        }   #
        #
        res <- lapply( 
            X   = 1:length(formatCol), 
            FUN = function(X){ 
                selCol <- namez[ X ] 
                #
                res <- formatCol[[ X ]]( x[, selCol ] ) 
                #
                return( res ) 
            }   #
        )   #
        #
        res <- do.call( 
            what = "data.frame", 
            args = c(res,"stringsAsFactors" = FALSE) 
        )   #
        #
        colnames( res ) <- namez 
        #
        x <- data.frame( 
            x[, !(colz %in% namez), drop = FALSE ], 
            res, 
            stringsAsFactors = FALSE 
        )   #
        #
        x <- x[, colz ] 
        #
        names( x ) <- colz 
    }   #
    #
    return( x ) 
}   #






edbRead <- function(# Read all or part of a table in a database (referenced by 'edb').
### Read all or part of a table in a database (referenced by 'edb'). 
### Generic function that call class-specific method corresponding 
### to the class of the \code{edb} object provided.

##seealso<< \code{link{edb}}, \code{link{edbWrite}}, 
## \code{link{edbNames}}, \code{link{edbColnames}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 sRow=NULL, 
### A list of named items. List of contrains/criterion to be applied 
### on \code{tableName} to retrieve a subset of rows. Each item in 
### \code{rowC} must be named after the columns on which the constrain 
### apply. The (vector of) value(s) of each items are the possible values 
### that can be retrieved. Values can be character or numeric. If NULL 
### (the default), all values are returned.

 sCol=NULL, 
### A vector of character strings. Names of the columns to retrieve.

 sRowOp=c("AND","OR")[1], 
### A single character string. Operator to be used to combine multiple 
### constrains in sRow. Possible values are "OR" or "AND". Default value 
### is "AND".

 formatCol=NULL, 
### If not NULL, a named list of functions to be applied to certain columns 
### after the data has been extracted from the database. The name of each list 
### item gives the column to process, and the value of each item gives the 
### function that must be applied. For instance 
### formatCol = list("DATE"=as.Date) will apply the function 
### \link{as.Date} to the column "DATE".

 distinct=FALSE, 
### Single logical. If TRUE, unique values in the result table will 
### be returned, using the \code{SELECT DISTINCT} SQL statement. 
### This is equivalent to applying \code{\link{unique}} to the 
### data.frame returned by the function, except that the action is 
### performed inside the database (not in R).

 orderBy=NULL, 
### Vector of character strings, or NULL (the default). If non NULL, 
### vector of column names that must be used to sort the result table. 
### Column names may be followed by a space and 'DESC' if the column 
### must be sorted in a descending order ('ASC', ascending, is the 
### default). For example, \code{oderBy = "MYCOLUMN DESC"}. This 
### operation is performed in the database with SQL ORDER BY 
### statement and is equivalent to ordering the data in R with 
### \code{\link{order}}.

 ...
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbRead")}

){  #
    UseMethod( generic = "edbRead", object = edb ) 
}   #






.edbOperation <- function(# Connect to a database (referenced by 'edb'), do some operation and close the database.
### Connect to a database (referenced by 'edb'), do some 
### operation and close the database.
### Generic function that call class-specific method corresponding 
### to the class of the \code{edb} object provided.

##seealso<<\code{\link{.edbOperation.RSQLite_SQLite}}, \code{\link{edb}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 expr, 
### Single expression, eventually between \code{expression({})}. Expression 
### that will be passed to tryCatch after a connection to the database has 
### been established. Additional parameters can be passed 
### to ..., and the database connexion object must be named 'dbCon'. 
### If an output is returned, it must be saved in an object called 
### 'exprOut' that is then returned by \code{.edbOperation}. After the 
### operation is done, the database connection is closed (even if an 
### error was detected).

 errorClasses=c("simpleError","error","condition"),  
### Vector of character strings. Error data classes to be found in 
### tryCatch result.

 stopOnError=TRUE, 
### Single logical. If TRUE and an error is detected, the function stops 
### AFTER closing the database and driver. If FALSE it just returns 
### the error as an object.

 errorMessage="An error was detected by tryCatch", 
### Error message to be send if an error is detected. Either as 
### stop message if \code{stopOnError = TRUE} or as a warning 

 ...
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbRead")}

){  #
    UseMethod( generic = ".edbOperation", object = edb ) 
}   #






.edbOperation.RSQLite_SQLite <- function(# Connect to a SQLite database (referenced by 'edb'), do some operation and close the database.
### Connect to a database (referenced by 'edb'), do some 
### operation and close the database. Generic function 
### that call class-specific method corresponding to the 
### class of the \code{edb} object provided. In more details, 
### the function open a connection to the database, then 

##seealso<<\code{\link{.edbOperation}}, \code{\link{edb}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 expr, 
### Single expression, eventually between \code{expression({})}. Expression 
### that will be passed to tryCatch after a connection to the database has 
### been established. Additional parameters can be passed 
### to ..., and the database connexion object must be named 'dbCon'. 
### If an output is returned, it must be saved in an object called 
### 'exprOut' that is then returned by \code{.edbOperation}. After the 
### operation is done, the database connection is closed (even if an 
### error was detected).

 errorClasses=c("simpleError","error","condition"),  
### Vector of character strings. Error data classes to be found in 
### tryCatch result.

 stopOnError=TRUE, 
### Single logical. If TRUE and an error is detected, the function stops 
### AFTER closing the database and driver. If FALSE it just returns 
### the error as an object.

 errorMessage="An error was detected by tryCatch", 
### Error message to be send if an error is detected. Either as 
### stop message if \code{stopOnError = TRUE} or as a warning 

#  drvName="SQLite",
# ### Single character string. See ?dbDriver 

 maxCon=1,  
### Single integer. See ?dbDriver 

 ...
### Additional parameters to be passed to some function in \code{expr}.

){  # Empty output:
    exprOut <- NULL 
    #
    # require( "DBI" ) 
    require( "RSQLite" ) 
    #
    sqliteCon <- dbDriver( 
        drvName = "SQLite", 
        max.con = maxCon  
    )   #
    #
    dbCon <- dbConnect( 
        drv    = sqliteCon, 
        dbname = edb[[ "dbName" ]]  
    )   #
    #
    # Initiate the error catching object:
    catchRes <- NULL 
    #
    catchRes <- tryCatch( 
        expr = eval( expr ),  #
        # What to do with an eventual error message catched (theError)?
        error = function(theError){ 
            theError # just return it.
        },  #
        ... 
    )   #
    #
    exRes <- dbGetException( dbCon ) 
    #
    dbDisconnect( conn = dbCon ) 
    #
    dbUnloadDriver( drv = sqliteCon ) 
    #
    if( any( class(catchRes) %in% errorClasses ) )
    {   #
        warning( catchRes ) 
        #
        if( stopOnError )
        {   #
            stop( errorMessage ) 
        }else{ 
            warning( errorMessage ) 
        }   #
    }   #
    #
    if( exRes[["errorNum"]] != 0 )
    {   #
        warning( exRes[["errorMsg"]] ) 
        #
        if( stopOnError )
        {   #
            stop( errorMessage ) 
        }else{ 
            warning( errorMessage ) 
        }   #
    }   #
    #
    return( exprOut ) 
### The function returns the object 'exprOut' eventually outputed 
### by expr, and NULL otherwise.
}   #







.edbFileExists <- function(# Internal function. Checks if some files exist and stop otherwise.
### Internal function. Checks if some files exist and stop otherwise.

 fileNames
### Names of the file to check.

){  #
    absent <- !file.exists( fileNames ) 
    #
    if( any( absent ) ) 
    {   #
        msg <- sprintf( 
            fmt = "Some file(s) (%s) could not be found.\n", 
            paste( fileNames[ absent ], collapse = ", " ) 
        )   #
        #
        stop( msg )
    }   #
}   #






edbColnames <- function(# Retrieve column names of a table in a database (referenced by 'edb').
### Retrieve column names of a table in a database (referenced by 
### 'edb'). Generic function that call class-specific method 
### corresponding to the class of the \code{edb} object provided.
### Notice that the methods do NOT retrieve the full table to 
### get its column names (so it should work even if the table is big).

##seealso<< \code{link{edb}}, \code{link{edbRead}}, 
## \code{link{edbWrite}}, 
## \code{link{edbNames}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 ...
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbColnames")}

){  #
    UseMethod( generic = "edbColnames", object = edb ) 
}   #






edbColnames.RSQLite_SQLite <- function(# Retrieve column names of a table in a SQLite database (referenced by 'edb').
### Retrieve column names of a table in a SQLIte database 
### (referenced by 'edb'). Wrapper around DBI::dbListFields().
### Notice that the method does NOT retrieve the full table to 
### get its column names (so it should work even if the table is big).

##seealso<< \code{link{edb}}, \code{link{edbRead.RSQLite_SQLite}}, 
## \code{link{edbwrite.RSQLite_SQLite}}, 
## \code{link{edbNames.RSQLite_SQLite}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 testFiles=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 ...
### Additional parameters to be passed to dbListFields(). See 
### \code{?dbListFields}.

){  #
    if( testFiles ) 
    {   # Check if the database files is present:
        .edbFileExists( edb[[ "dbName" ]] ) 
    }   #
    # 
    msg <- sprintf( 
        fmt = "Error detected in dbListFields() in edbColnames.RSQLite_SQLite() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    tbl <- .edbOperation.RSQLite_SQLite(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- dbListFields(
                conn      = dbCon, 
                ... 
            )   #
        }), #
        maxCon       = 1,  
        errorClasses = c("simpleError","error","condition"),  
        stopOnError  = TRUE, 
        errorMessage = msg, 
        # ... options for expr:
        name         = tableName, 
        ... 
    )   #
    #
    return( tbl ) 
### The function returns a vector of character strings with the 
### columns / fields of the original sqlite table.
}   #






edbRead.RSQLite_SQLite <- function(# Read all or part of a table in a SQLIte database (referenced by 'edb').
### Read all or part of a table in a SQLIte database (referenced by 'edb'). 

##seealso<< \code{link{edb}}, \code{link{edbWrite.RSQLite_SQLite}}, 
## \code{link{edbNames.RSQLite_SQLite}}, 
## \code{link{edbColnames.RSQLite_SQLite}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 sRow=NULL, 
### A list of named items. List of contrains/criterion to be applied 
### on \code{tableName} to retrieve a subset of rows. Each item in 
### \code{rowC} must be named after the columns on which the constrain 
### apply. The (vector of) value(s) of each items are the possible values 
### that can be retrieved. Values can be character or numeric. If NULL 
### (the default), all values are returned.

 sCol=NULL, 
### Either (1) a vector of character strings with the name of the 
### columns to retrieve or (2) a vector of logical of the same 
### length as the number of columns or (3) a vector of indexes / 
### integers giving the indexes of the column to retrieve. If 
### negative, then it indicates the indexes of the column to leave 
### out.

 sRowOp=c("AND","OR")[1], 
### A single character string. Operator to be used to combine multiple 
### constrains in sRow. Possible values are "OR" or "AND". Default value 
### is "AND".

 formatCol=NULL, 
### If not NULL, a named list of functions to be applied to certain columns 
### after the data has been extracted from the database. The name of each list 
### item gives the column to process, and the value of each item gives the 
### function that must be applied. For instance 
### formatCol = list("DATE"=as.Date) will apply the function 
### \link{as.Date} to the column "DATE".

 distinct=FALSE, 
### Single logical. If TRUE, unique values in the result table will 
### be returned, using the \code{SELECT DISTINCT} SQL statement. 
### This is equivalent to applying \code{\link{unique}} to the 
### data.frame returned by the function, except that the action is 
### performed inside the database (not in R).

 orderBy=NULL, 
### Vector of character strings, or NULL (the default). If non NULL, 
### vector of column names that must be used to sort the result table. 
### Column names may be followed by a space and 'DESC' if the column 
### must be sorted in a descending order ('ASC', ascending, is the 
### default). This operation is performed in the database with 
### SQL ORDER BY statement and is equivalent to ordering the 
### data in R with \code{\link{order}}. You may write the 
### column names between square brackets [] if they contain spaces.

 testFiles=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 verbose=FALSE, 
### Single logical. If TRUE, information on what is done are output 
### on screen.

 ...
### Additional parameters to be passed to \code{dbGetQuery}.

){  #
    if( testFiles ) 
    {   # Check if the database files is present:
        .edbFileExists( edb[[ "dbName" ]] ) 
    }   #
    # 
    # Prepare the list of columns to choose in the table:
    if( length(sCol) != 0 )
    {   #
        if( is.numeric( sCol ) ) # Index selection
        {   #
            sCol <- as.integer( sCol ) 
            #
            # Test that the sign ofcolums is homogeneous:
            # (inspired by the package dfdb-rodbc)
            signSCol <- sign( sCol )
            testSign <- !(sum( signSCol ) %in% (c(1,-1)*length(sCol))) 
            #
            if( testSign )
            {   #
                stop( "When 'sCol' is integers/index, it must be either all positive or all negative" ) 
            }   #
            #
            # Get column names:
            colsList <- edbColnames( edb, tableName = tableName ) 
            #
            # Test that the range of values does not exceed the number of columns
            testRange <- abs( sCol ) %in% 1:length( colsList ) 
            #
            if( !all(testRange) ) # Positive index
            {   #
                stop( "When 'sCol' is integers/index, it can't be 0 or bigger than the number of columns." ) 
            }   #
            #
            # Transform indexes into column names
            if( all(as.logical(signSCol)) == 1 ) # Positive index
            {   #
                sCol <- colsList[ sCol ] 
            }else{ # Negative index
                sCol <- colsList[ !(colsList %in% colsList[ sCol ]) ] 
            }   #
            #
            # Wrap and concatenate column names for SQL
            selectWhat <- paste( sep="", "[", sCol, "]" ) 
            #
            selectWhat <- paste( sCol, collapse = ", " ) 
        }else{ 
            if( is.logical( sCol ) )
            {   #
                # Get column names:
                colsList <- edbColnames( edb, tableName = tableName ) 
                #
                if( length(sCol) != length(colsList) )
                {   #
                    stop( "When 'sCol' is logical, it must be the same length as the number of columns in the table." ) 
                }   #
                #
                sCol <- colsList[ sCol ]
                #
                # Wrap and concatenate column names for SQL
                selectWhat <- paste( sep="", "[", sCol, "]" ) 
                #
                selectWhat <- paste( sCol, collapse = ", " ) 
            }else{ 
                if( is.character( sCol ) )
                {   #
                    # Wrap and concatenate column names for SQL
                    selectWhat <- paste( sep="", "[", sCol, "]" ) 
                    #
                    selectWhat <- paste( sCol, collapse = ", " ) 
                }else{ 
                    stop( "class(sCol) must be numerical/integer, logical or character." )
                }   #
            }   #
        }   #
    }else{ 
        selectWhat <- "*"
    }   #
    # 
    # Prepare the 1st series of constrains:
    if( length(sRow) != 0 ) 
    {   #
        if( class(sRow) != "list" ){ 
            stop("'sRow' must be a list.")
        }   #
        #
        sRowLength <- length( sRow ) 
        sRowNames  <- names( sRow ) 
        #
        if( length( sRowNames ) != sRowLength )
        {   #
            stop( "(column) names are missing in 'sRow'." ) 
        }   #
        #
        selSQL  <- names(sRow) == "SQL"
        sRowSQL <- sRow[ selSQL ]  
        sRow    <- sRow[ !selSQL ] 
        #
        if( length(sRow) > 0 ){
            sRow <- lapply( 
                X   = 1:length(sRow), 
                FUN = function(X){ 
                    const <- sRow[[ X ]] 
                    #
                    if( class(const) == "character" ) 
                    {   #
                        const <- paste( "\"", const, "\"", sep = "" ) 
                    }   #
                    #
                    const <- paste( 
                        sep = "", 
                        "([", sRowNames[ X ], "] = ", 
                        const, ")" 
                    )   #
                    #
                    const <- paste( const, collapse = " OR " )
                    #
                    const <- paste( "(", const, ")", sep = "" )  
                    #
                    return( const ) 
                 }  #
            )   #
        }else{ 
            sRow <- NULL 
        }   #
        #
        if( length(sRowSQL) ){
            sRowSQL <- paste( "(", sRowSQL, ")", sep = "" )
        }   # 
        #
        sRowOp <- paste( " ", sRowOp, " ", sep = "" ) 
        #
        sRow <- c( unlist(sRow), sRowSQL ) 
        #
        sRow <- paste( unlist( sRow ), collapse = sRowOp ) 
        #
        sRow <- paste( "WHERE", sRow, sep = " " ) 
    }else{ 
        sRow <- NULL 
    }   #
    #
    # DISTINCT statement:
    distinct <- ifelse(distinct,"DISTINCT ","")
    #
    # ORDER BY statement:
    if( !is.null(orderBy) ){ 
        orderBy <- paste( 
            sep = "", 
            "\nORDER BY ", 
            paste( collapse = ", ", orderBy ), 
            "\n" 
        )   #
    }else{ 
        orderBy <- "" 
    }   #
    #
    # Create the full querry statement:
    statement <- paste( 
            sep = "", 
            "SELECT ", distinct, selectWhat, "\n", 
            "FROM [", tableName, "]\n", 
            sRow, 
            orderBy, 
            ";\n" 
        )   #
    #
    if( verbose ){ 
        cat( "SQL statement:\n" ) 
        cat( statement, sep = "\n" )
    }   #
    #
    # require( "DBI" ) # in .edbOperation.RSQLite_SQLite
    # require( "RSQLite" ) 
    #
    msg <- sprintf( 
        fmt = "Error detected in dbGetQuery() in edbRead.RSQLite_SQLite() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    tbl <- .edbOperation.RSQLite_SQLite(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- dbGetQuery( 
                conn = dbCon, 
                ...  
            )   #
        }),  #
        maxCon       = 1,  
        errorClasses = c("simpleError","error","condition"),  
        stopOnError  = TRUE, 
        errorMessage = msg, 
        # ... options for expr:
        statement    = statement, 
        ... 
    )   #
    #
    if( dim(tbl)[2] == 0 ) 
    {   #
        fieldsRes <- edbColnames.RSQLite_SQLite( 
            edb       = edb,
            tableName = tableName, 
            testFiles = FALSE  # Already done
        )   #
        #
        tbl <- as.data.frame( 
            matrix( 
                data = vector(mode = "numeric", length = 0), 
                nrow = 0, 
                ncol = length( fieldsRes ) 
            )   #
        )   #
        #
        colnames(tbl) <- fieldsRes 
        #
        if( length(sCol) != 0 ) 
        {   #
            tbl <- tbl[, sCol, drop = FALSE ] 
        }   #
    }   #
    #
    tbl <- .formatCol( 
        x         = tbl, 
        formatCol = formatCol 
    )   #
    #
    return( tbl ) 
### The function returns the requested table. 
}   #






edbNames <- function(# Retrieve table names in a database (referenced by 'edb').
### Retrieve table names in a database (referenced by 'edb'). 
### Generic function that call class-specific method corresponding 
### to the class of the \code{edb} object provided.

##seealso<< \code{link{edb}}, \code{link{edbRead}}, 
## \code{link{edbWrite}}, 
## \code{link{edbColnames}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 ...
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbColnames")}

){  #
    UseMethod( generic = "edbNames", object = edb ) 
}   #






edbNames.RSQLite_SQLite <- function(# Retrieve table names in a SQLIte database (referenced by 'edb').
### Retrieve table names in a SQLite database (referenced by 'edb'). 

##seealso<< \code{link{edb}}, \code{link{edbRead.RSQLite_SQLite}}, 
## \code{link{edbWrite.RSQLite_SQLite}}, 
## \code{link{edbColnames.RSQLite_SQLite}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 testFiles=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 ...
### Additional parameters to be passed to \code{dbListTables}.

){  #
    if( testFiles ) 
    {   # Check if the database files is present:
        .edbFileExists( edb[[ "dbName" ]] ) 
    }   #
    #
    # require( "DBI" ) # in .edbOperation.RSQLite_SQLite
    # require( "RSQLite" ) 
    #
    msg <- sprintf( 
        fmt = "Error detected in dbListTables() in edbNames.RSQLite_SQLite() (database: %s). Database connection closed.\n", 
        edb[["dbName"]] 
    )   #
    #
    tbl <- .edbOperation.RSQLite_SQLite(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- dbListTables(
                conn    = dbCon,  
                ...  
            )   #
        }), #
        maxCon       = 1,  
        errorClasses = c("simpleError","error","condition"),  
        stopOnError  = TRUE, 
        errorMessage = msg, 
        # ... options for expr:
        ... 
    )   #
    #
    return( tbl ) 
### The function returns the list of tables found in the database. 
}   #






# "[.edb" <- function(# "[" method for reading all or part of a table in a database (referenced by 'edb').
# ### "[" method for reading all or part of a table in a database (referenced 
# ### by 'edb'). Wrapper for \code{\link{edbRead}}. Generic function that 
# ### call class-specific method corresponding to the class of the 
# ### \code{edb} object provided.

# ##seealso<< \code{link{edb}}, \code{link{edbRead}}, 
# ## \code{link{edbNames}}, \code{link{edbColnames}}.

#  edb,
# ### An object of class 'edb', such as returned by \code{\link{edb}}.

#  tableName, 
# ### Single character string. Name of the table to read in 'edb'.

#  sRow=NULL, 
# ### A list of named items. List of contrains/criterion to be applied 
# ### on \code{tableName} to retrieve a subset of rows. Each item in 
# ### \code{rowC} must be named after the columns on which the constrain 
# ### apply. The (vector of) value(s) of each items are the possible values 
# ### that can be retrieved. Values can be character or numeric. If NULL 
# ### (the default), all values are returned.

#  sCol=NULL, 
# ### A vector of character strings. Names of the columns to retrieve.

#  sRowOp=c("AND","OR")[1], 
# ### A single character string. Operator to be used to combine multiple 
# ### constrains in sRow. Possible values are "OR" or "AND". Default value 
# ### is "AND".

#  testFiles=TRUE,  
# ### Single logical. Should the function test for the presence 
# ### (file.exist()) of the needed files in the folder before trying 
# ### to fetch information from the database? 

#  verbose=FALSE, 
# ### Single logical. If TRUE, information on what is done are output 
# ### on screen.

#  ...
# ### Additional parameters to be passed to \code{dbGetQuery}.

# ){  #
#     UseMethod( generic = "[.edb", object = edb ) 
# }   #






"[.RSQLite_SQLite" <- function(# "[" method for reading all or part of a table in a SQLite database (referenced by 'edb').
### "[" method for reading all or part of a table in a SQLite 
### database (referenced by 'edb'). Wrapper for 
### \code{\link{edbRead.RSQLite_SQLite}}. 

##seealso<< \code{link{edb}}, \code{link{edbRead.RSQLite_SQLite}}, 
## \code{link{edbNames.RSQLite_SQLite}}, 
## \code{link{edbColnames.RSQLite_SQLite}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 sRow=NULL, 
### A list of named items. List of contrains/criterion to be applied 
### on \code{tableName} to retrieve a subset of rows. Each item in 
### \code{rowC} must be named after the columns on which the constrain 
### apply. The (vector of) value(s) of each items are the possible values 
### that can be retrieved. Values can be character or numeric. If NULL 
### (the default), all values are returned.

 sCol=NULL, 
### Either (1) a vector of character strings with the name of the 
### columns to retrieve or (2) a vector of logical of the same 
### length as the number of columns or (3) a vector of indexes / 
### integers giving the indexes of the column to retrieve. If 
### negative, then it indicates the indexes of the column to leave 
### out.

 sRowOp=c("AND","OR")[1], 
### A single character string. Operator to be used to combine multiple 
### constrains in sRow. Possible values are "OR" or "AND". Default value 
### is "AND".

 formatCol=NULL, 
### If not NULL, a named list of functions to be applied to certain columns 
### after the data has been extracted from the database. The name of each list 
### item gives the column to process, and the value of each item gives the 
### function that must be applied. For instance 
### formatCol = list("DATE"=as.Date) will apply the function 
### \link{as.Date} to the column "DATE".

 distinct=FALSE, 
### Single logical. If TRUE, unique values in the result table will 
### be returned, using the \code{SELECT DISTINCT} SQL statement. 
### This is equivalent to applying \code{\link{unique}} to the 
### data.frame returned by the function, except that the action is 
### performed inside the database (not in R).

 orderBy=NULL, 
### Vector of character strings, or NULL (the default). If non NULL, 
### vector of column names that must be used to sort the result table. 
### Column names may be followed by a space and 'DESC' if the column 
### must be sorted in a descending order ('ASC', ascending, is the 
### default). This operation is performed in the database with 
### SQL ORDER BY statement and is equivalent to ordering the 
### data in R with \code{\link{order}}. You may write the 
### column names between square brackets [] if they contain spaces.

 testFiles=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 verbose=FALSE, 
### Single logical. If TRUE, information on what is done are output 
### on screen.

 ...
### Additional parameters to be passed to \code{dbGetQuery}.

){  #
    tbl <- edbRead.RSQLite_SQLite(       
        edb       = edb, 
        tableName = tableName, 
        sRow      = sRow,
        sCol      = sCol,
        sRowOp    = sRowOp,
        testFiles = testFiles,  
        verbose   = verbose, 
        formatCol = formatCol, 
        distinct  = distinct, 
        orderBy   = orderBy, 
        ...
    )   #
    #
    return( tbl ) 
}   #






edbWrite <- function(# Write data in a table in a database (referenced by 'edb').
### Write data in a table in a database (referenced by 'edb'). 
### Generic function that call class-specific method corresponding 
### to the class of the \code{edb} object provided.

##seealso<< \code{link{edb}}, \code{link{edbRead}}, 
## \code{link{edbNames}}, \code{link{edbColnames}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 data, 
### data.frame. Data to be writen in \code{tableName}. If the table 
### has a PRIMARY KEY, and if it is AUTOINCREMENT, then the column 
### can be omitted, and the attributed ID's will be retrieved if 
### \code{!is.null(getKey)} (not the default).

 mode=c("a","u","o")[1], 
### Single character string. If \code{"a"} (default), the data are 
### appened to the table (added after the last row), and \code{sRow} 
### is ignored. If \code{"u"}, the data are updated according to some 
### critearia in \code{sRow} (that can't be NULL). If \code{"o"}, 
### the table is overwritten and \code{sRow} is ignored. 

 pKey=NULL, # NEW NEW
### Single character string (if mode == "u") or NULL. Column name that 
### is PRIMARY KEY in the table.

#  sRow=NULL, 
# ### A list of named items. List of contrains/criterion to be applied 
# ### on \code{tableName} to _update_ a subset of rows. Each item in 
# ### \code{rowC} must be named after the columns on which the constrain 
# ### apply. The (vector of) value(s) of each items are the values 
# ### that must be updated, and it must have the same length as 
# ### \code{nrow(data)} (the 1st value being the constrain for row 1 in 
# ### \code{data}, etc.). Values can be character or numeric.

 getKey=NULL, 
### Single character string or NULL. If non NULL, name of the PRIMARY 
### KEY whose latest attributed values should be retrieved.

 formatCol=NULL, 
### If not NULL, a named list of functions to be applied to certain columns 
### before the data are written to the database. The name of each list 
### item gives the column to process, and the value of each item gives the 
### function that must be applied. For instance 
### formatCol = list("DATE"=as.Date) will apply the function 
### \link{as.Date} to the column "DATE".

 posixFormat="", 
### Single character string. 'format' argument of the functions 
### format.POSIXlt() or format.POSIXct() used to convert POSIX 
### date-time into character strings when writing into the database.
### Only used if getKey is not NULL or when mode == "u" in SQLite or 
### MySQL.

 dateFormat="", 
### Single character string. 'format' argument of the functions 
### format.Date() used to convert "Date" 
### dates into character strings when writing into the database.
### Only used if getKey is not NULL or when mode == "u" in SQLite or 
### MySQL.

 logOp=FALSE, 
### Single logical. If TRUE, then a log of the operation is written 
### into the database, using the function \code{\link{edbLog}}. 
### See the arguments below and \code{\link{edbLog}} for more details.

 logRandId=rnorm(1), 
### Single numerical. See \code{\link{edbLog}}.

 logMsg=as.character(NA), 
### Single character string. See \code{\link{edbLog}}.

 logTableName="edbLog", 
### Single character string. See \code{\link{edbLog}}.

 logCreateTableIfNotExist=TRUE, 
### Single logical. See \code{\link{edbLog}}.

 parano=TRUE, 
### Single logical. If set to TRUE (the default), the function is 
### run on "paranoia mode", that is additional tests are performed 
### before the data are written into the database. This slows down 
### a bit (more) the function, but it may avoid some mistakes.

 ...
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbWrite")}

){  #
    UseMethod( generic = "edbWrite", object = edb ) 
}   #






.edbSendGetQuery.DBI_RSQLite <- function(# Internal. Mixes dbSendQuery() and dbGetQuery()
### Internal. Mixes dbSendQuery() and dbGetQuery() 

 conn,
### Connexion to a DBI database.

 statement, 
### vector of 2 character strings. Two statements to be passed to 
### dbSendQuery() and then dbGetQuery(), respectively.
 
 ...){  #
    exprOut <- dbSendQuery( 
        conn        = conn, 
        statement   = statement[1], 
        ... 
    )   #
    #
    exprOut <- dbGetQuery( 
        conn        = conn, 
        statement   = statement[2], 
        ... 
    )   #
    #
    return( exprOut ) 
### Returns the output of dbGetQuery().
}   #






.formatTable4Query <- function(# Format data in a data.frame so it is ready to be send in a query (format conversion).
### Format data in a data.frame so it is ready to be send in a query (format conversion).

 data, 
### A data.frame 

 del="\"", 
### A single character. Type of delimiter to be used for character strings: 
### Either "\"" or "'" or "`" 

 posixFormat="", 
### Single character string. 'format' argument of the functions 
### format.POSIXlt() or format.POSIXct() used to convert POSIX 
### date-time into character strings when writing into the database.

 dateFormat=""  
### Single character string. 'format' argument of the functions 
### format.Date() used to convert "Date" 
### dates into character strings when writing into the database.

){  #
    dataCol <- colnames( data ) 
    #
    # Identify character or factor columns:
    testDataCol <- unlist( 
        lapply( 
            X   = 1:ncol(data), 
            FUN = function(X){ 
                is.character( data[,X] ) | is.factor( data[,X] ) 
            }   #
        )   #
    )   #
    #
    tNA  <- paste( del, "NA", del, sep = "" ) 
    tNA2 <- paste( del, del, sep = "" ) 
    #
    #
    # Wrap the character data into "" for the SQL statement
    if( any(testDataCol) )
    {   #
        data[,testDataCol] <- do.call( 
            what = "cbind", 
            args = lapply( 
                X   = (1:ncol(data))[ testDataCol ], 
                FUN = function(X){ 
                    tmp <- paste( del, data[,X], del, sep = "" ) 
                    #
                    tmp[ tmp == tNA ] <- tNA2 
                    #
                    tmp 
                }   #
            )   #
        )   #
    }   #
    #
    # NEW NEW: Identify character or factor columns:
    testDateCol <- unlist( 
        lapply( 
            X   = 1:ncol(data), 
            FUN = function(X){ 
                any( class( data[,X] ) %in% c("POSIXlt","POSIXct") )
            }   #
        )   #
    )   #
    #
    # Wrap the character data into "" for the SQL statement
    if( any(testDateCol) )
    {   #
        data[,testDateCol] <- do.call( 
            what = "cbind", 
            args = lapply( 
                X   = (1:ncol(data))[ testDateCol ], 
                FUN = function(X){ 
                    tmp <- paste( 
                        del, 
                        format( data[,X], format = posixFormat ), 
                        del, 
                        sep = ""
                    )   # 
                    #
                    tmp[ tmp == tNA ] <- tNA2 
                    #
                    tmp 
                }   #
            )   #
        )   #
    }   #
    #
    testDateCol2 <- unlist( 
        lapply( 
            X   = 1:ncol(data), 
            FUN = function(X){ 
                class( data[,X] ) == "Date" 
            }   #
        )   #
    )   #
    #
    # Wrap the character data into "" for the SQL statement
    if( any(testDateCol2) )
    {   #
        data[,testDateCol2] <- do.call( 
            what = "cbind", 
            args = lapply( 
                X   = (1:ncol(data))[ testDateCol2 ], 
                FUN = function(X){ 
                    tmp <- paste( 
                        del, 
                        format( data[,X], format = dateFormat ), 
                        del, 
                        sep = ""
                    )   # 
                    #
                    tmp[ tmp == tNA ] <- tNA2 
                    #
                    tmp 
                }   #
            )   #
        )   #
    }   #
    #
    selCol <- !(testDataCol | testDateCol | testDateCol2) 
    #
    # Same for non character columns:
    if( any( selCol ) )
    {   #
        data[,selCol] <- do.call( 
            what = "cbind", 
            args = lapply( 
                X   = (1:ncol(data))[ selCol ], 
                FUN = function(X){ 
                    tmp <- data[,X]
                    #
                    tmp[ is.na(tmp) ] <- tNA2 
                    #
                    tmp 
                }   #
            )   #
        )   #
    }   #
    #
    data <- data[, dataCol, drop = FALSE ] 
    #
    return( data ) 
}   #






edbWrite.RSQLite_SQLite <- function(# Write data in a SQLite table in a database (referenced by 'edb').
### Write data in a table in a SQLite database (referenced by 'edb'). 

##seealso<< \code{link{edb}}, \code{link{edbRead.RSQLite_SQLite}}, 
## \code{link{edbNames.RSQLite_SQLite}}, 
## \code{link{edbColnames.RSQLite_SQLite}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 data, 
### data.frame. Data to be writen in \code{tableName}. If the table 
### has a PRIMARY KEY, and if it is AUTOINCREMENT, then the column 
### can be omitted, and the attributed ID's will be retrieved if 
### \code{!is.null(getKey)} (not the default). If \code{sRow} is not 
### NULL, then data must contain the column names given in \code{sRow}.

 mode=c("a","u","o")[1], 
### Single character string. If \code{"a"} (default), the data are 
### appened to the table (added after the last row), and \code{sRow} 
### is ignored. If \code{"u"}, the data are updated according to some 
### critearia in \code{pKey} (that can't be NULL). If \code{"o"}, 
### the table is overwritten and \code{pKey} is ignored. 

#  sRow=NULL, 
# ### A vector of character strings. Name of the columns in \code{data}  
# ### that contains the constrains defining which values must be updated 
# ### (for instance the name of the PRIMARY KEY column). Ignored if 
# ### \code{mode} is not \code{"u"}.

 pKey=NULL, # NEW NEW
### Single character string (if mode == "u") or NULL. Column name that 
### is PRIMARY KEY in the table.

 getKey=NULL, 
### Single character string or NULL. If non NULL, name of the PRIMARY 
### KEY whose latest attributed values should be retrieved.

#  sRowOp=c("AND","OR")[1], 
# ### A single character string. Operator to be used to combine multiple 
# ### constrains in sRow. Possible values are "OR" or "AND". Default value 
# ### is "AND".

 formatCol=NULL, 
### If not NULL, a named list of functions to be applied to certain columns 
### before the data are written to the database. The name of each list 
### item gives the column to process, and the value of each item gives the 
### function that must be applied. For instance 
### formatCol = list("DATE"=as.Date) will apply the function 
### \link{as.Date} to the column "DATE".

 posixFormat="", 
### Single character string. 'format' argument of the functions 
### format.POSIXlt() or format.POSIXct() used to convert POSIX 
### date-time into character strings when writing into the database.
### Only used if getKey is not NULL or when mode == "u" in SQLite.

 dateFormat="", 
### Single character string. 'format' argument of the functions 
### format.Date() used to convert "Date" 
### dates into character strings when writing into the database.
### Only used if getKey is not NULL or when mode == "u" in SQLite.

 logOp=FALSE, 
### Single logical. If TRUE, then a log of the operation is written 
### into the database, using the function \code{\link{edbLog}}. 
### See the arguments below and \code{\link{edbLog}} for more details.

 logRandId=rnorm(1), 
### Single numerical. See \code{\link{edbLog}}.

 logMsg=as.character(NA), 
### Single character string. See \code{\link{edbLog}}.

 logTableName="edbLog", 
### Single character string. See \code{\link{edbLog}}.

 logCreateTableIfNotExist=TRUE, 
### Single logical. See \code{\link{edbLog}}.

 parano=TRUE, 
### Single logical. If set to TRUE (the default), the function is 
### run on "paranoia mode", that is additional tests are performed 
### before the data are written into the database. This slows down 
### a bit (more) the function, but it may avoid some mistakes.

 testFiles=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 verbose=FALSE, 
### Single logical. If TRUE, information on what is done are output 
### on screen.

 ...
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbWrite")}

){  #
    if( testFiles ) 
    {   # Check if the database files is present:
        .edbFileExists( edb[[ "dbName" ]] ) 
    }   #
    #
    # require( "DBI" ) # in .edbOperation.RSQLite_SQLite
    # require( "RSQLite" ) 
    #
    # Save the existing state of 'last.warning' to be able to detect 
    # if warnings were emitted during the transaction (see after dbUnloadDriver)
    last.warning.exist <- exists( "last.warning", envir = baseenv() ) 
    #
    if( last.warning.exist ) 
    {   #
        old.warn <- get( "last.warning", envir = baseenv() ) 
        #
        assign( 
            x     = "last.warning", 
            value = list(), 
            envir = baseenv() 
        )   
        #
        # remove( list = "last.warning", envir = baseenv() ) 
    }else{ 
        old.warn <- list()  
    }   #
    #
    # Convert the format of some columns:
    data <- .formatCol( 
        x         = data, 
        formatCol = formatCol 
    )   #
    #
    if( mode != "u" ) 
    {   ### Case 1: mode != "u", append or overwrite mode.
        #
        if( mode == "a" )
        {   #
            append <- TRUE 
        }else{ 
            if( mode == "o" ) 
            {   #
                append <- FALSE
            }else{ 
                stop( "'mode' must be either 'u' or 'a' or 'o'." )
            }   #
        }   #
        #
        if( is.null( getKey ) )
        {   # 
            if( parano & (mode != "o") ) 
            {   #
                colNamez <- edbColnames( edb = edb, tableName = tableName ) 
                #
                # 1 - Check that all columns in data are present in tableName:
                testCol1 <- colnames( data ) %in% colNamez 
                #
                if( !all( testCol1 ) ) 
                {   #
                    stop( paste(
                        sep = "", 
                        "Some columns in input 'data' could not be found in the table '", 
                        tableName,"' (", 
                        paste( colnames( data )[!testCol1], collapse = ", " ), 
                        ")." 
                    ) ) #
                }   #
                #
                # 2 - Check that all columns in tableName are present in data:
                testCol2 <- colNamez %in% colnames( data ) 
                #
                if( !all( testCol2 ) ) 
                {   #
                    stop( paste(
                        sep = "", 
                        "Some columns in the table '", 
                        tableName, "' could not be found in input 'data' (", 
                        paste( colNamez[!testCol2], collapse = ", " ), 
                        ")." 
                    ) ) #
                }   #
                #
                # 3 - Put the columns in the right order:
                data <- data[, colNamez ] 
            }   #
            # 
            msg <- sprintf( 
                fmt = "Error detected in dbWriteTable() in edbWrite.RSQLite_SQLite() (database: %s; table: %s). Database connection closed.\n", 
                edb[["dbName"]], tableName 
            )   #
            #
            oldOptions <- options( "warn" )[[ 1 ]] 
            options( "warn" = 1 )  
            #
            res <- .edbOperation.RSQLite_SQLite(
                edb          = edb, 
                expr         = expression({ 
                    exprOut <- dbWriteTable( 
                        conn        = dbCon, 
                        ... 
                    )   #
                }),  #
                maxCon       = 1,  
                errorClasses = c("simpleError","error","condition"),  
                stopOnError  = TRUE, 
                errorMessage = msg, 
                # ... options for expr:
                name         = tableName, 
                value        = data, 
                row.names    = FALSE, 
                overwrite    = !append, 
                append       = append,   
                ... 
            )   #
            #
            options( "warn" = oldOptions ) 
        }else{ 
            #
            data <- .formatTable4Query( 
                data        = data, 
                del         = "\"", 
                posixFormat = posixFormat, 
                dateFormat  = dateFormat  
            )   #
            #
            dataCol <- colnames( data ) 
            #
            oldOptions <- options( "warn" )[[ 1 ]]  
            options( "warn" = 1 )  
            #
            newId <- lapply( 
                X   = 1:nrow( data ), 
                FUN = function(X){ 
                    # Create the SQL update statement
                    sqlUpdate <- paste( 
                        sep = "", 
                        "INSERT INTO [", tableName, "]\n", 
                        "(", paste( "[", dataCol, "]", collapse = ",", sep = "" ), ")\n", 
                         "VALUES(", paste( as.character( data[X,] ), collapse = "," ), ");\n"    
                    )   #
                    #
                    if( verbose ){ 
                         cat( sqlUpdate ) 
                    }    # 
                    #
                    sqlUpdate2 <- paste( 
                        sep = "", 
                        "SELECT ", 
                        getKey, 
                        " FROM [", 
                        tableName, 
                        "] WHERE ", 
                        getKey, 
                        " = last_insert_rowid();\n" 
                    )   #
                    #
                    if( verbose ){ 
                         cat( sqlUpdate2 ) 
                    }    # 
                    #
                    msg <- sprintf( 
                        fmt = "Error detected in dbGetQuery() in edbWrite.RSQLite_SQLite() (database: %s; table: %s; row: %s). Database connection closed.\n", 
                        edb[["dbName"]], tableName, as.character(X) 
                    )   #
                    #
                    newId <- .edbOperation.RSQLite_SQLite(
                        edb          = edb, 
                        expr         = expression({ 
                            exprOut <- .edbSendGetQuery.DBI_RSQLite( conn = dbCon, ... )
                        }), #
                        maxCon       = 1,  
                        errorClasses = c("simpleError","error","condition"),  
                        stopOnError  = TRUE, 
                        errorMessage = msg, 
                        # ... options for expr:
                        statement    = c(sqlUpdate,sqlUpdate2), 
                        ... 
                    )   #
                    #
                    msg <- sprintf( 
                        fmt = "Error detected in dbGetQuery() in edbWrite.RSQLite_SQLite() (database: %s; table: %s; row: %s). Database connection closed.\n", 
                        edb[["dbName"]], tableName, as.character(X) 
                    )   #
                    #
                    return( newId )
                }   #
            )   #
            #
            newId <- as.numeric( unlist( newId ) ) 
        }   #    
    }else{ ### mode == "u", update mode.
        #
        data <- .formatTable4Query( 
            data        = data, 
            del         = "\"", 
            posixFormat = posixFormat, 
            dateFormat  = dateFormat  
        )   #
        #
        dataCol <- colnames( data ) 
        #
        if( is.null(pKey) ){ # NEW NEW
            stop( "When mode = 'u', pKey must be a non-null character string." )
        }   #
        #
#         if( !is.null( sRow ) )
#         {   #
#             if( class(sRow) != "character" ){ 
#                 stop("'sRow' must be a vector of character string(s).")
#             }   #
#             #
#             testSRow <- sRow %in% dataCol 
#             #
#             if( any( !testSRow ) ){ 
#                 msg <- sprintf( 
#                     fmt = "Some 'sRow' values where not found in 'data' column names (%s).\n", 
#                     paste( sRow[ !testSRow ], collapse = ", " ) 
#                 )   #
#                 #
#                 stop( msg ) 
#             }   #
#             #
#             data[, sRow ] <- do.call( 
#                 what = "cbind", 
#                 args = lapply( 
#                     X   = sRow, 
#                     FUN = function(X){ 
#                         res <- paste( "[", X, "] = ", data[, X ], sep = "" ) 
#                         return( res ) 
#                     }   # 
#                 )   #
#             )   #
#             #
#             const <- do.call( 
#                 what = "paste", 
#                 args = c( lapply( 
#                     X   = sRow,
#                     FUN = function(X){ data[,X] } 
#                     ), sep = " AND "  
#                 )   # 
#             )   #
#             #
#             const <- paste( "WHERE ", const, sep = " " )  
#             #
#             dataCol <- dataCol[ !testSRow ]
#         }else{ # NEW NEW
        #
        if( pKey %in% colnames(data) ){ 
            const <- paste( "WHERE ([", pKey, "] = ", data[, pKey ], ")", sep = "" )  
        }else{ 
            stop( "When 'pKey' is not NULL, 'pKey' must be provided in 'data' (column names)." )
        }   #
        #
#         }   #
        #
        data <- data[, dataCol, drop = FALSE ] 
        #
        oldOptions <- options( "warn" )[[ 1 ]]  
        options( "warn" = 1 )  
        #
        res <- lapply( 
            X   = 1:nrow( data ), 
            FUN = function(X){ 
                # Create the SQL update statement
                sqlUpdate <- paste( 
                    sep = "", 
                    "UPDATE [", tableName, "]\n", 
                    "SET ", paste( 
                        "[", dataCol, "]", 
                        " = ", 
                        data[X,], 
                        collapse = ", ", # Not AND 
                        sep      = ""
                    ),  "\n", 
                    const[ X ], ";\n\n"  
                )   #
                #
                if( verbose ){ 
                     cat( sqlUpdate ) 
                }    # 
                #
                msg <- sprintf( 
                    fmt = "Error detected in dbGetQuery() in edbWrite.RSQLite_SQLite() (database: %s; table: %s; row: %s). Database connection closed.\n", 
                    edb[["dbName"]], tableName, as.character(X) 
                )   #
                #
                res <- .edbOperation.RSQLite_SQLite(
                    edb          = edb, 
                    expr         = expression({ 
                        exprOut <- dbGetQuery( 
                            conn        = dbCon, 
                            ... 
                        )   #
                    }),  #
                    maxCon       = 1,  
                    errorClasses = c("simpleError","error","condition"),  
                    stopOnError  = TRUE, 
                    errorMessage = msg, 
                    # ... options for expr:
                    statement       = sqlUpdate,    
                    ... 
                )   #
                #
                return( res ) 
            }   #
        )   #
        #
        res <- unlist( res ) 
        #
        options( "warn" = oldOptions ) 
    }   #
    #
    last.warning.exist2 <- exists( "last.warning", envir = baseenv() ) 
    #
    if( last.warning.exist2 )
    {   #
        last.warning2 <- get( "last.warning", envir = baseenv() )
        #
        if( length( last.warning2 ) != 0 ) 
        {   #
            assign( 
                x     = "last.warning", 
                value = c( 
                    old.warn, 
                    last.warning2 
                ),  #
                envir = baseenv() 
            )   #
            #
            stop( "Warning(s) detected in SQLite transaction. type warnings() to see it/them." ) 
        }   #
    }else{ 
        if( last.warning.exist ) 
        {   #
            assign( 
                x     = "last.warning", 
                value = old.warn, 
                envir = baseenv() 
            )   #
        }   #
    }   #
    #
    if( logOp )
    {   #
        tmp <- edbLog(
            edb             = edb,
            tableName       = tableName, 
            fun             = "edbWrite.RSQLite_SQLite", 
            date            = date(), 
            R.version       = R.version.string, 
            nodename        = Sys.info()[["nodename"]], 
            edbVersion      = NULL, 
            mode            = mode, 
            getKey          = getKey, 
            logRandId       = logRandId, 
            logMsg          = logMsg, 
            logTableName    = logTableName, 
            logCreateTableIfNotExist=TRUE  
        )   #
    }   #
    #
    if( exists( "newId" ) ){ 
        return( newId )
    }else{ 
        return( res ) 
    }   #
### If id.col.nm is not NA, the function returns a list containing
### a vector of ID values, and named after 'id.col.nm'. 
### If an error message is detected the function stops.
}   #






# "[<-.edb" <- function(# "[<-" generic for 'edb' objects. Write data in a table in a database (referenced by 'edb').
# ### "[<-" generic for 'edb' objects. Write data in a table in a 
# ### database (referenced by 'edb'). 

# ##seealso<< \code{\link{edb}}, \code{\link{edbWrite}}, 
# ## \code{\link{edbRead}}.


#  edb,
# ### An object of class 'edb', such as returned by \code{\link{edb}}.

#  tableName, 
# ### Single character string. Name of the table to read in 'edb'.

#  mode="a", 
# ### Single character string. If \code{"a"} (default), the data are 
# ### appened to the table (added after the last row), and \code{sRow} 
# ### is ignored. If \code{"u"}, the data are updated according to some 
# ### critearia in \code{sRow} (that can't be NULL). If \code{"o"}, 
# ### the table is overwritten and \code{sRow} is ignored. 

#  sRow=NULL, 
# ### A vector of character strings. Name of the columns in \code{data}  
# ### that contains the constrains defining which values must be updated 
# ### (for instance the name of the PRIMARY KEY column). Ignored if 
# ### \code{mode} is not \code{"u"}.

#  getKey=NULL, 
# ### Single logical. If TRUE, the latest attributed primary keys will be 
# ### retrieved.

# #  sRowOp=c("AND","OR")[1], 
# # ### A single character string. Operator to be used to combine multiple 
# # ### constrains in sRow. Possible values are "OR" or "AND". Default value 
# # ### is "AND".

#  testFiles=TRUE,  
# ### Single logical. Should the function test for the presence 
# ### (file.exist()) of the needed files in the folder before trying 
# ### to fetch information from the database? 

#  verbose=FALSE, 
# ### Single logical. If TRUE, information on what is done are output 
# ### on screen.

#  ..., 
# ### Additional parameters to be passed to class-specific method. See 
# ### \code{methods("edbWrite")}

#  value 
# ### data.frame. Data to be writen in \code{tableName}. If the table 
# ### has a PRIMARY KEY, and if it is AUTOINCREMENT, then the column 
# ### can be omitted, and the attributed ID's will be retrieved if 
# ### \code{getKey = TRUE} (not the default). If \code{sRow} is not 
# ### NULL, then data must contain the column names given in \code{sRow}.

# ){  #
#     if( !is.null(getKey) ){ 
#         stop( "'getKey' must be NULL to use '[<-' methods." ) 
#     }   #
#     #
#     UseMethod( generic = "[<-.edb", object = edb ) 
# }   #






"[<-.RSQLite_SQLite" <- function(# "[<-" method for SQLite databases. Write data in a SQLite table in a database (referenced by 'edb').
### "[<-" method for SQLite databases. Write data in a table in a 
### SQLite database (referenced by 'edb'). 

##seealso<< \code{\link{edb}}, \code{\link{edbWrite.RSQLite_SQLite}}, 
## \code{\link{edbRead.RSQLite_SQLite}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 mode="a", 
### Single character string. If \code{"a"} (default), the data are 
### appened to the table (added after the last row), and \code{sRow} 
### is ignored. If \code{"u"}, the data are updated according to some 
### critearia in \code{sRow} (that can't be NULL). If \code{"o"}, 
### the table is overwritten and \code{sRow} is ignored. 

 pKey=NULL, # NEW NEW
### Single character string (if mode == "u") or NULL. Column name that 
### is PRIMARY KEY in the table.

#  sRow=NULL, 
# ### A vector of character strings. Name of the columns in \code{data}  
# ### that contains the constrains defining which values must be updated 
# ### (for instance the name of the PRIMARY KEY column). Ignored if 
# ### \code{mode} is not \code{"u"}.

 getKey=NULL, 
### Single character string or NULL. If non NULL, name of the PRIMARY 
### KEY whose latest attributed values should be retrieved.

#  sRowOp=c("AND","OR")[1], 
# ### A single character string. Operator to be used to combine multiple 
# ### constrains in sRow. Possible values are "OR" or "AND". Default value 
# ### is "AND".

 formatCol=NULL, 
### If not NULL, a named list of functions to be applied to certain columns 
### before the data are written to the database. The name of each list 
### item gives the column to process, and the value of each item gives the 
### function that must be applied. For instance 
### formatCol = list("DATE"=as.Date) will apply the function 
### \link{as.Date} to the column "DATE".

 posixFormat="", 
### Single character string. 'format' argument of the functions 
### format.POSIXlt() or format.POSIXct() used to convert POSIX 
### date-time into character strings when writing into the database.
### Only used if getKey is not NULL or when mode == "u" in SQLite.

 dateFormat="", 
### Single character string. 'format' argument of the functions 
### format.Date() used to convert "Date" 
### dates into character strings when writing into the database.
### Only used if getKey is not NULL or when mode == "u" in SQLite.

#  logOp=FALSE, 
# ### Single logical. If TRUE, then a log of the operation is written 
# ### into the database, using the function \code{\link{edbLog}}. 
# ### See the arguments below and \code{\link{edbLog}} for more details.

#  logRandId=rnorm(1), 
# ### Single numerical. See \code{\link{edbLog}}.

#  logMsg=as.character(NA), 
# ### Single character string. See \code{\link{edbLog}}.

#  logTableName="edbLog", 
# ### Single character string. See \code{\link{edbLog}}.

#  logCreateTableIfNotExist=TRUE, 
# ### Single logical. See \code{\link{edbLog}}.

 parano=TRUE, 
### Single logical. If set to TRUE (the default), the function is 
### run on "paranoia mode", that is additional tests are performed 
### before the data are written into the database. This slows down 
### a bit (more) the function, but it may avoid some mistakes.

 testFiles=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 verbose=FALSE, 
### Single logical. If TRUE, information on what is done are output 
### on screen.

 ..., 
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbWrite")}

 value 
### data.frame. Data to be writen in \code{tableName}. If the table 
### has a PRIMARY KEY, and if it is AUTOINCREMENT, then the column 
### can be omitted, and the attributed ID's will be retrieved if 
### \code{getKey = TRUE} (not the default). If \code{sRow} is not 
### NULL, then data must contain the column names given in \code{sRow}.

){  #
    if( !is.null(getKey) ){ 
         stop( "'getKey' must be NULL to use '[<-' methods." )
    }   #
    #
    res <- edbWrite.RSQLite_SQLite( 
        edb         = edb,
        tableName   = tableName, 
        data        = value, 
        mode        = mode, 
        pKey        = pKey, 
        getKey      = getKey, 
        testFiles   = testFiles,  
        verbose     = verbose, 
        formatCol   = formatCol, 
        posixFormat = posixFormat, 
        dateFormat  = dateFormat, 
#         logOp       = logOp, 
#         logRandId   = logRandId, 
#         logMsg      = logMsg, 
#         logTableName= logTableName, 
#         logCreateTableIfNotExist=logCreateTableIfNotExist, 
        parano      = parano, 
        ...
    )   #
    #
    return( edb ) 
}   #






edbDelete <- function(# Delete all or some rows in a table in a database (referenced by 'edb').
### Delete all or some rows in a table in a database (referenced by 'edb'). 
### Generic function that call class-specific method corresponding 
### to the class of the \code{edb} object provided.

##seealso<< \code{link{edb}}, \code{link{edbWrite}}, 
## \code{link{edbNames}}, \code{link{edbColnames}}, 
## \code{link{edbRead}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 sRow=NULL, 
### A list of named items. List of contrains/criterion to be applied 
### on \code{tableName} to retrieve a subset of rows. Each item in 
### \code{rowC} must be named after the columns on which the constrain 
### apply. The (vector of) value(s) of each items are the possible values 
### that can be retrieved. Values can be character or numeric. If NULL 
### (the default), all values are returned.

 sRowOp=c("AND","OR")[1], 
### A single character string. Operator to be used to combine multiple 
### constrains in sRow. Possible values are "OR" or "AND". Default value 
### is "AND".

 logOp=FALSE, 
### Single logical. If TRUE, then a log of the operation is written 
### into the database, using the function \code{\link{edbLog}}. 
### See the arguments below and \code{\link{edbLog}} for more details.

 logRandId=rnorm(1), 
### Single numerical. See \code{\link{edbLog}}.

 logMsg=as.character(NA), 
### Single character string. See \code{\link{edbLog}}.

 logTableName="edbLog", 
### Single character string. See \code{\link{edbLog}}.

 logCreateTableIfNotExist=TRUE, 
### Single logical. See \code{\link{edbLog}}.

 ...
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbDelete")}

){  #
    UseMethod( generic = "edbDelete", object = edb ) 
}   #






edbDelete.RSQLite_SQLite <- function(# Delete all or some rows in a table in a SQLIte database (referenced by 'edb').
### Delete all or some rows in a table in a SQLIte database (referenced by 'edb'). 

##seealso<< \code{link{edb}}, \code{link{edbWrite.RSQLite_SQLite}}, 
## \code{link{edbNames.RSQLite_SQLite}}, 
## \code{link{edbColnames.RSQLite_SQLite}}, \code{link{edbRead.RSQLite_SQLite}}

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 sRow=NULL, 
### A list of named items. List of contrains/criterion to be applied 
### on \code{tableName} to retrieve a subset of rows. Each item in 
### \code{rowC} must be named after the columns on which the constrain 
### apply. The (vector of) value(s) of each items are the possible values 
### that can be retrieved. Values can be character or numeric. If NULL 
### (the default), all values are returned.

 sRowOp=c("AND","OR")[1], 
### A single character string. Operator to be used to combine multiple 
### constrains in sRow. Possible values are "OR" or "AND". Default value 
### is "AND".

 logOp=FALSE, 
### Single logical. If TRUE, then a log of the operation is written 
### into the database, using the function \code{\link{edbLog}}. 
### See the arguments below and \code{\link{edbLog}} for more details.

 logRandId=rnorm(1), 
### Single numerical. See \code{\link{edbLog}}.

 logMsg=as.character(NA), 
### Single character string. See \code{\link{edbLog}}.

 logTableName="edbLog", 
### Single character string. See \code{\link{edbLog}}.

 logCreateTableIfNotExist=TRUE, 
### Single logical. See \code{\link{edbLog}}.

 testFiles=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 verbose=FALSE, 
### Single logical. If TRUE, information on what is done are output 
### on screen.

 ...
### Additional parameters to be passed to \code{dbGetQuery}.

){  #
    if( testFiles ) 
    {   # Check if the database files is present:
        .edbFileExists( edb[[ "dbName" ]] ) 
    }   #
    # 
    # Prepare the 1st series of constrains:
    if( length(sRow) != 0 ) 
    {   #
        if( class(sRow) != "list" ){ 
            stop("'sRow' must be a list.")
        }   #
        #
        sRowLength <- length( sRow ) 
        sRowNames  <- names( sRow ) 
        #
        if( length( sRowNames ) != sRowLength )
        {   #
            stop( "(column) names are missing in 'sRow'." ) 
        }   #
        #
        selSQL  <- names(sRow) == "SQL"
        sRowSQL <- sRow[ selSQL ]  
        sRow    <- sRow[ !selSQL ] 
        #
        if( length(sRow) > 0 ){
            sRow <- lapply( 
                X   = 1:length(sRow), 
                FUN = function(X){ 
                    const <- sRow[[ X ]] 
                    #
                    if( class(const) == "character" ) 
                    {   #
                        const <- paste( "\"", const, "\"", sep = "" ) 
                    }   #
                    #
                    const <- paste( 
                        sep = "", 
                        "([", sRowNames[ X ], "] = ", 
                        const, ")" 
                    )   #
                    #
                    const <- paste( const, collapse = " OR " )
                    #
                    const <- paste( "(", const, ")", sep = "" )  
                    #
                    return( const ) 
                 }  #
            )   #
        }else{ 
            sRow <- NULL 
        }   #
        #
        if( length(sRowSQL) > 0 ){
            sRowSQL <- paste( "(", sRowSQL, ")", sep = "" )
        }   # 
        #
        sRowOp <- paste( " ", sRowOp, " ", sep = "" ) 
        #
        sRow <- c( unlist(sRow), sRowSQL ) 
        #
        sRow <- paste( unlist( sRow ), collapse = sRowOp ) 
        #
        sRow <- paste( "WHERE", sRow, sep = " " ) 
    }else{ 
        sRow <- NULL 
    }   #
    #
    # Create the full querry statement:
    statement <- paste( 
            sep = "", 
            "DELETE FROM [", tableName, "]\n", 
            sRow, "\n", 
            ";\n" 
        )   #
    #
    if( verbose ){ 
        cat( "SQL statement:\n" ) 
        cat( statement, sep = "\n" )
    }   #
    #
    # require( "DBI" ) # in .edbOperation.RSQLite_SQLite
    # require( "RSQLite" ) 
    #
    msg <- sprintf( 
        fmt = "Error detected in dbGetQuery() in edbRead.RSQLite_SQLite() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    out <- .edbOperation.RSQLite_SQLite(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- dbGetQuery( 
                conn = dbCon, 
                ...  
            )   #
        }),  #
        maxCon       = 1,  
        errorClasses = c("simpleError","error","condition"),  
        stopOnError  = TRUE, 
        errorMessage = msg, 
        # ... options for expr:
        statement    = statement, 
        ... 
    )   #
    #
    if( logOp )
    {   #
        tmp <- edbLog(
            edb             = edb,
            tableName       = tableName, 
            fun             = "edbDelete.RSQLite_SQLite", 
            date            = date(), 
            R.version       = R.version.string, 
            nodename        = Sys.info()[["nodename"]], 
            edbVersion      = NULL, 
            mode            = as.character(NA), 
            getKey          = as.character(NA), 
            logRandId       = logRandId, 
            logMsg          = logMsg, 
            logTableName    = logTableName, 
            logCreateTableIfNotExist=TRUE 
        )   #
    }   #
    #
    return( out ) 
### The function returns the requested table. 
}   #






edbDrop <- function(# Drop a table in a database (referenced by 'edb').
### Drop a table in a database (referenced by 'edb'). 
### Generic function that call class-specific method corresponding 
### to the class of the \code{edb} object provided.

##seealso<< \code{link{edb}}, \code{link{edbWrite}}, 
## \code{link{edbNames}}, \code{link{edbColnames}}, 
## \code{link{edbRead}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 logOp=FALSE, 
### Single logical. If TRUE, then a log of the operation is written 
### into the database, using the function \code{\link{edbLog}}. 
### See the arguments below and \code{\link{edbLog}} for more details.

 logRandId=rnorm(1), 
### Single numerical. See \code{\link{edbLog}}.

 logMsg=as.character(NA), 
### Single character string. See \code{\link{edbLog}}.

 logTableName="edbLog", 
### Single character string. See \code{\link{edbLog}}.

 logCreateTableIfNotExist=TRUE, 
### Single logical. See \code{\link{edbLog}}.

 ...
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbDelete")}

){  #
    UseMethod( generic = "edbDrop", object = edb ) 
}   #






edbDrop.RSQLite_SQLite <- function(# Delete all or some rows in a table in a SQLIte database (referenced by 'edb').
### Delete all or some rows in a table in a SQLIte database (referenced by 'edb'). 

##seealso<< \code{link{edb}}, \code{link{edbWrite.RSQLite_SQLite}}, 
## \code{link{edbNames.RSQLite_SQLite}}, 
## \code{link{edbColnames.RSQLite_SQLite}}, \code{link{edbRead.RSQLite_SQLite}}

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 logOp=FALSE, 
### Single logical. If TRUE, then a log of the operation is written 
### into the database, using the function \code{\link{edbLog}}. 
### See the arguments below and \code{\link{edbLog}} for more details.

 logRandId=rnorm(1), 
### Single numerical. See \code{\link{edbLog}}.

 logMsg=as.character(NA), 
### Single character string. See \code{\link{edbLog}}.

 logTableName="edbLog", 
### Single character string. See \code{\link{edbLog}}.


 logCreateTableIfNotExist=TRUE, 
### Single logical. See \code{\link{edbLog}}.

 testFiles=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 verbose=FALSE, 
### Single logical. If TRUE, information on what is done are output 
### on screen.

 ...
### Additional parameters to be passed to \code{dbGetQuery}.

){  #
    if( testFiles ) 
    {   # Check if the database files is present:
        .edbFileExists( edb[[ "dbName" ]] ) 
    }   #
    #
    # Create the full querry statement:
    statement <- paste( 
            sep = "", 
            "DROP TABLE [", tableName, "]\n", 
            ";\n" 
        )   #
    #
    if( verbose ){ 
        cat( "SQL statement:\n" ) 
        cat( statement, sep = "\n" )
    }   #
    #
    # require( "DBI" ) # in .edbOperation.RSQLite_SQLite
    # require( "RSQLite" ) 
    #
    msg <- sprintf( 
        fmt = "Error detected in dbGetQuery() in edbRead.RSQLite_SQLite() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    out <- .edbOperation.RSQLite_SQLite(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- dbGetQuery( 
                conn = dbCon, 
                ...  
            )   #
        }),  #
        maxCon       = 1,  
        errorClasses = c("simpleError","error","condition"),  
        stopOnError  = TRUE, 
        errorMessage = msg, 
        # ... options for expr:
        statement    = statement, 
        ... 
    )   #
    #
    if( logOp )
    {   #
        tmp <- edbLog(
            edb             = edb,
            tableName       = tableName, 
            fun             = "edbDrop.RSQLite_SQLite", 
            date            = date(), 
            R.version       = R.version.string, 
            nodename        = Sys.info()[["nodename"]], 
            edbVersion      = NULL, 
            mode            = as.character(NA), 
            getKey          = as.character(NA), 
            logRandId       = logRandId, 
            logMsg          = logMsg, 
            logTableName    = logTableName, 
            logCreateTableIfNotExist=TRUE  
        )   #
    }   #
    #
    return( out ) 
### The function returns the requested table. 
}   #






edbDim <- function(# Retrieve the dimension of a table in a database (referenced by 'edb').
### Retrieve the dimension of a table in a database (referenced by 
### 'edb'). Notice that the methods do NOT retrieve the full table to 
### get its column names (so it should work even if the table is big).

##seealso<< \code{link{edb}}, \code{link{edbColnames}}, 
## \code{link{edbRead}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 ...
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbColnames")} and \code{methods("edbRead")}.

){  # Retrieve the column names:
    cn <- edbColnames( 
        edb       = edb, 
        tableName = tableName, 
        ...
    )   #
    #
    # Number of columns
    nbcol <- length( cn ) 
    #
    # Retrieve the first column
    nbrow <- edbRead( 
        edb       = edb, 
        tableName = tableName, 
        sCol      = cn[1], 
        ... 
    )[,]#
    #
    # Number of rows
    nbrow <- length( nbrow )
    #
    return( c(nbrow,nbcol) ) 
}   #






edbNRow <- function(# Retrieve the number of rows of a table in a database (referenced by 'edb').
### Retrieve the number of rows of a table in a database (referenced by 
### 'edb'). Notice that the methods do NOT retrieve the full table to 
### get its column names (so it should work even if the table is big).

##seealso<< \code{link{edb}}, \code{link{edbColnames}}, 
## \code{link{edbRead}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 ...
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbColnames")} and \code{methods("edbRead")}.

){  # Retrieve the dimention of the table:
    tblDim <- edbDim( 
        edb       = edb, 
        tableName = tableName, 
        ...
    )   #
    #
    return( tblDim[ 1 ] ) 
}   #






edbNCol <- function(# Retrieve the number of columns of a table in a database (referenced by 'edb').
### Retrieve the number of columns of a table in a database (referenced by 
### 'edb'). Notice that the methods do NOT retrieve the full table to 
### get its column names (so it should work even if the table is big).

##seealso<< \code{link{edb}}, \code{link{edbColnames}}, 
## \code{link{edbRead}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 ...
### Additional parameters to be passed to class-specific method. See 
### \code{methods("edbColnames")} and \code{methods("edbRead")}.

){  # Retrieve the dimention of the table:
    tblDim <- edbDim( 
        edb       = edb, 
        tableName = tableName, 
        ...
    )   #
    #
    return( tblDim[ 2 ] ) 
}   #






edbLog <- function(# Write an operation "log" (used when modifying the database)
### Write an operation "log" (to be used when modifying the 
### database). An operation log consists in information about (a) the function 
### used, (b) the table modified, (c) the operation date, (d) R version, (e) 
### easydb version, (f) the option useds ('mode' and 'getKey' if used with 
### edbWrite()). This is not version control, and this is not comprehensive 
### operation log.

##seealso<<\code{link{edb}}, \code{link{edbWrite}}, \code{link{edbDelete}}, 
## \code{link{edbDrop}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.
### Database in which the log has to be written.

 tableName=as.character(NA), 
### Single character string. The name of the table that was modified.
### Not to be confused with 'logTableName'.

 fun=as.character(NA), 
### Single character string. The name of the function that did the modification. 

 date=date(), 
### Single character string. The date and time of the operation.

 R.version=R.version.string, 
### Single character string. R version

 nodename=Sys.info()[["nodename"]], 
### Single character string. Name of the computer (node). 

 edbVersion=NULL, 
### Single character string. Version of easydb. If NULL, the version is 
### fetched using 'installed.packages()'.

 mode=as.character(NA), 
### Single character string. Value of the argument 'mode', if fun is 
### edbWrite().
  
 getKey=as.character(NA), 
### Single character string. Value of the argument 'getKey', if fun is 
### edbWrite().

 logRandId=rnorm(1), 
### Single numerical. Some number, hopefully unique, that can be used 
### to identify the log record, as a complement to the log date.

 logMsg=as.character(NA), 
### Single character string. Aditional log message to be written in the 
### log table (same record as the rest).

 logTableName="edbLog", 
### Single character string. Name of the log table. If that table does not 
### exist, it will be created.

 logCreateTableIfNotExist=TRUE  
### Single logical. If TRUE (the default), then the table is created if it 
### does not exist yet. Please notice that this option is not multi-thread 
### safe.

){  #
    if( is.null( edbVersion ) )
    {   #
        edbVersion <- installed.packages()[,c("Package","Version")] 
        #
        selRow <- edbVersion[, "Package" ] == "easydb" 
        #
        edbVersion <- edbVersion[ selRow, "Version" ] 
    }   #
    #
    if( is.null(getKey) ){ getKey <- "" }
    #
    tbl <- data.frame( 
        "TABLE_NAME"   = tableName, 
        "FUN"          = fun, 
        "DATE_LOG"     = date, 
        "R_VERSION"    = R.version, 
        "NODE_NAME"    = nodename, 
        "EDB_VERSION"  = edbVersion, 
        "MODE"         = mode, 
        "GETKEY"       = getKey, 
        "RAND_ID"      = logRandId, 
        "LOG_MESSAGE"  = logMsg, 
        stringsAsFactors = FALSE 
    )   #
    #
    if( logCreateTableIfNotExist )
    {   #
        tableList <- edbNames( edb ) 
        #
        if( !(logTableName %in% tableList) )
        {   #
            mode2 <- "o" 
        }else{ 
            mode2 <- "a" 
        }   #
    }else{ 
        mode2 <- "a" 
    }   #
    #
    res <- edbWrite(
        edb         = edb,
        tableName   = logTableName, 
        data        = tbl, 
        mode        = mode2, 
        #pKey        = NULL, # NEW NEW
        #getKey      = NULL, 
        #formatCol   = NULL, 
        #posixFormat = "", 
        #dateFormat  = "", 
        logOp       = FALSE  # DO NOT SET TO TRUE!!
        #logRandId   =rnorm(1), 
        #logMsg      =as.character(NA), 
        #logTableName="edbLog", 
        #logCreateTableIfNotExist=TRUE, 
    )   #
    #
    return( res ) 
}   #


