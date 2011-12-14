# source( "/media/JMOEYS_8G2/_R_PACKAGES/easydb/pkg/easydb/R/edb_ms_access.R" ) 
# source( "C:/_R_PACKAGES/easydb/pkg/easydb/R/edb_ms_access.R" )
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






.edbOperation.RODBC_Access <- function(# Connect to a MS Access database (referenced by 'edb'), do some operation and close the database.
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

 ...
### Additional parameters to be passed to some function in \code{expr}.

){  # Empty output:
    exprOut <- NULL 
    #
    require( "RODBC" ) 
    #
    if( "dbLogin" %in% names(edb) ){ 
        uid <- edb[[ "dbLogin" ]] 
    }else{ 
        uid <- "" 
    }   #
    #
    if( "dbPwd" %in% names(edb) ){ 
        pwd <- edb[[ "dbPwd" ]] 
    }else{ 
        pwd <- "" 
    }   #
    #
    if( "accessVersion" %in% names(edb) ){ 
        if( edb[[ "accessVersion" ]] == 2007 ){ 
            dbCon <- odbcConnectAccess2007( 
                access.file = edb[[ "dbName" ]], 
                uid         = uid, 
                pwd         = pwd 
            )   #
        }else{ 
            dbCon <- odbcConnectAccess( 
                access.file = edb[[ "dbName" ]], 
                uid         = uid, 
                pwd         = pwd 
            )   #
        }   #
    }else{ 
        dbCon <- odbcConnectAccess( 
            access.file = edb[[ "dbName" ]], 
            uid         = uid, 
            pwd         = pwd 
        )   #
    }   #
    #
    if( any( dbCon == -1 ) ){ 
        stop( sprintf( "Connexion to MS Access database %s failed.", edb[[ "dbName" ]] ) ) 
    }   #
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
    # dbDisconnect( conn = dbCon ) 
    odbcClose( channel = dbCon ) 
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
    return( exprOut ) 
### The function returns the object 'exprOut' eventually outputed 
### by expr, and NULL otherwise.
}   #






edbColnames.RODBC_Access <- function(# Retrieve column names of a table in a MS Access database (referenced by 'edb').
### Retrieve column names of a table in a MS Access database 
### (referenced by 'edb'). Wrapper around RODBC::sqlColumns().
### Notice that the method does NOT retrieve the full table to 
### get its column names (so it should work even if the table is big).

##seealso<< \code{link{edb}}, \code{link{edbRead.RODBC_Access}}, 
## \code{link{edbwrite.RODBC_Access}}, 
## \code{link{edbNames.RODBC_Access}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 onlyNames=TRUE, 
### Single logical. If TRUE only returns the column names (vector), 
### and if FALSE returns a detailed table as in 
### \code{\link[RODBC]{sqlColumns}}.

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
        fmt = "Error detected in sqlColumns() in edbColnames.RODBC_Access() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    tbl <- .edbOperation.RODBC_Access(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- sqlColumns(
                channel = dbCon, 
                ... 
            )   #
        }), #
        errorClasses = c("simpleError","error","condition"),  
        stopOnError  = TRUE, 
        errorMessage = msg, 
        # ... options for expr:
        sqtable      = tableName, 
        ... 
    )   #
    #
    if( onlyNames ){ 
        tbl <- tbl[,"COLUMN_NAME"] 
    }   #
    #
    return( tbl ) 
### The function returns a vector of character strings with the 
### columns / fields of the original MS Access table, or a table with 
### full details on the columns (see \code{onlyNames}).
}   #






edbRead.RODBC_Access <- function(# Read all or part of a table in a MS Access database (referenced by 'edb').
### Read all or part of a table in a MS Access database (referenced by 'edb'). 

##seealso<< \code{link{edb}}, \code{link{edbWrite.RODBC_Access}}, 
## \code{link{edbNames.RODBC_Access}}, 
## \code{link{edbColnames.RODBC_Access}}.

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
                        const <- paste( "'", const, "'", sep = "" ) 
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
    # Create the full querry statement:
    statement <- paste( 
            sep = "", 
            "SELECT ", selectWhat, "\n", 
            "FROM [", tableName, "]\n", 
            sRow, "\n", 
            ";\n" 
        )   #
    #
    if( verbose ){ 
        cat( "SQL statement:\n" ) 
        cat( statement, sep = "\n" )
    }   #
    #
    msg <- sprintf( 
        fmt = "Error detected in sqlQuery() in edbRead.RODBC_Access() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    tbl <- .edbOperation.RODBC_Access(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- sqlQuery( 
                channel = dbCon, 
                ...  
            )   #
        }),  #
        errorClasses = c("simpleError","error","condition"),  
        stopOnError  = TRUE, 
        errorMessage = msg, 
        # ... options for expr:
        query        = statement, 
        #case        = "nochange", 
        ... 
    )   #
    #
    if( dim(tbl)[2] == 0 ) 
    {   #
        fieldsRes <- edbColnames.RODBC_Access( 
            edb       = edb,
            tableName = tableName 
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






edbNames.RODBC_Access <- function(# Retrieve table names in a MS Access database (referenced by 'edb').
### Retrieve table names in a MS Access database (referenced by 'edb'). 

##seealso<< \code{link{edb}}, \code{link{edbRead.RODBC_Access}}, 
## \code{link{edbWrite.RODBC_Access}}, 
## \code{link{edbColnames.RRODBC_Access}}.

 edb,
### An object of class 'edb', such as returned by \code{\link{edb}}.

 onlyNames=TRUE, 
### Single logical. If TRUE only returns the table names (vector), 
### and if FALSE returns a detailed table as in 
### \code{\link[RODBC]{sqlTables}}.

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
    msg <- sprintf( 
        fmt = "Error detected in sqlTables() in edbNames.RODBC_Access() (database: %s). Database connection closed.\n", 
        edb[["dbName"]] 
    )   #
    #
    tbl <- .edbOperation.RODBC_Access(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- sqlTables(
                channel = dbCon,  
                ...  
            )   #
        }), #
        errorClasses = c("simpleError","error","condition"),  
        stopOnError  = TRUE, 
        errorMessage = msg, 
        # ... options for expr:
        ... 
    )   #
    #
    if( onlyNames ){ 
        tbl <- tbl[,"TABLE_NAME"] 
    }   #
    #
    return( tbl ) 
### The function returns the list of tables found in the database. 
}   #






"[.RODBC_Access" <- function(# "[" method for reading all or part of a table in a MS Access database (referenced by 'edb').
### "[" method for reading all or part of a table in a MS Access 
### database (referenced by 'edb'). Wrapper for 
### \code{\link{edbRead.RODBC_Access}}. 

##seealso<< \code{link{edb}}, \code{link{edbRead.RODBC_Access}}, 
## \code{link{edbNames.RODBC_Access}}, 
## \code{link{edbColnames.RODBC_Access}}.

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

 verbose=FALSE, 
### Single logical. If TRUE, information on what is done are output 
### on screen.

 ...
### Additional parameters to be passed to \code{dbGetQuery}.

){  #
    tbl <- edbRead.RODBC_Access(       
        edb       = edb, 
        tableName = tableName, 
        sRow      = sRow,
        sCol      = sCol,
        sRowOp    = sRowOp,
        verbose   = verbose, 
        formatCol = formatCol, 
        ...
    )   #
    #
    return( tbl ) 
}   #






.edbSendGetQuery.RODBC_Access <- function(# Internal. Mixes dbSendQuery() and dbGetQuery()
### Internal. Mixes dbSendQuery() and dbGetQuery() 

 channel,
### Connexion to a RODBC database.

 query, 
### vector of 2 character strings. Two statements to be passed to 
### dbSendQuery() and then dbGetQuery(), respectively.
 
 ...
### Additional parameters passed to \code{odbcQuery}, \code{sqlQuery} 
### and \code{sqlGetResults}.

){  #
    exprOut <- sqlQuery( 
        channel = channel, 
        query   = query[1], 
        ... 
    )   #
    #
    exprOut <- odbcQuery( 
        channel = channel, 
        query   = query[2], 
        ... 
    )   #
    #
    exprOut <- sqlGetResults( 
        channel = channel, 
        ... 
    )   #
    #
    return( exprOut ) 
### Returns the output of sqlGetResults().
}   #






edbWrite.RODBC_Access <- function(# Write data in a MS Access table in a database (referenced by 'edb').
### Write data in a table in a MS Access database (referenced by 'edb'). 

##seealso<< \code{link{edb}}, \code{link{edbRead.RODBC_Access}}, 
## \code{link{edbNames.RODBC_Access}}, 
## \code{link{edbColnames.RODBC_Access}}.

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
### Only used if getKey is not NULL.

 dateFormat="", 
### Single character string. 'format' argument of the functions 
### format.Date() used to convert "Date" 
### dates into character strings when writing into the database.
### Only used if getKey is not NULL.

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
                fmt = "Error detected in dbWriteTable() in edbWrite.RODBC_Access() (database: %s; table: %s). Database connection closed.\n", 
                edb[["dbName"]], tableName 
            )   #
            #
            oldOptions <- options( "warn" )[[ 1 ]] 
            options( "warn" = 1 )  
            #
            res <- .edbOperation.RODBC_Access(
                edb          = edb, 
                expr         = expression({ 
                    exprOut <- sqlSave( 
                        channel = dbCon, 
                        ... 
                    )   #
                }),  #
                errorClasses = c("simpleError","error","condition"),  
                stopOnError  = TRUE, 
                errorMessage = msg, 
                # ... options for expr:
                tablename    = tableName, 
                dat          = data, 
                rownames     = FALSE, 
                append       = append,   
                ... 
            )   #
            #
            options( "warn" = oldOptions ) 
        }else{ 
            #
            data <- .formatTable4Query( 
                data        = data, 
                del         = "'", 
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
                        "SELECT @@IDENTITY\n" # FROM [", 
                        #tableName, 
                        #"];\n" 
                    )   #
                    #
                    if( verbose ){ 
                         cat( sqlUpdate2 ) 
                    }    # 
                    #
                    msg <- sprintf( 
                        fmt = "Error detected in .edbSendGetQuery.RODBC_Access() in edbWrite.RODBC_Access() (database: %s; table: %s; row: %s). Database connection closed.\n", 
                        edb[["dbName"]], tableName, as.character(X) 
                    )   #
                    #
                    newId <- .edbOperation.RODBC_Access(
                        edb          = edb, 
                        expr         = expression({ 
                            exprOut <- .edbSendGetQuery.RODBC_Access( channel = dbCon, ... )
                        }), #
                        errorClasses = c("simpleError","error","condition"),  
                        stopOnError  = TRUE, 
                        errorMessage = msg, 
                        # ... options for expr:
                        query        = c(sqlUpdate,sqlUpdate2), 
                        ... 
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
        # dataCol <- colnames( data ) 
        # #
        # # Identify character or factor columns:
        # testDataCol <- unlist( 
        #     lapply( 
        #         X   = 1:ncol(data), 
        #         FUN = function(X){ 
        #             is.character( data[,X] ) | is.factor( data[,X] ) 
        #         }   #
        #     )   #
        # )   #
        # #
        # # Wrap the character data into "" for the SQL statement
        # if( any(testDataCol) ) 
        # {   #
        #     data[,testDataCol] <- do.call( 
        #         what = "cbind", 
        #         args = lapply( 
        #             X   = (1:ncol(data))[ testDataCol ], 
        #             FUN = function(X){ 
        #                 tmp <- paste( "\"", data[,X], "\"", sep = "" ) 
        #                 #
        #                 tmp[ tmp == "NA" ] <- "" 
        #                 #
        #                 tmp 
        #             }   #
        #         )   #
        #     )   #
        # }   #
        # #
        # # Same for non character columns:
        # if( any(!testDataCol) )
        # {   #
        #     data[,!testDataCol] <- do.call( 
        #         what = "cbind", 
        #         args = lapply( 
        #             X   = (1:ncol(data))[ !testDataCol ], 
        #             FUN = function(X){ 
        #                 tmp <- data[,X]
        #                 #
        #                 tmp[ is.na(tmp) ] <- "\"\"" 
        #                 #
        #                 tmp 
        #             }   #
        #         )   #
        #     )   #
        # }   #
        #
        if( is.null(pKey) ){ # NEW NEW
            stop( "When mode = 'u', pKey must be a non-null character string." )
        }   #
        #
        # if( !is.null( sRow ) )
        # {   #
        #     if( class(sRow) != "character" ){ 
        #         stop("'sRow' must be a vector of character string(s).")
        #     }   #
        #     #
        #     testSRow <- sRow %in% dataCol 
        #     #
        #     if( any( !testSRow ) ){ 
        #         msg <- sprintf( 
        #             fmt = "Some 'sRow' values where not found in 'data' column names (%s).\n", 
        #             paste( sRow[ !testSRow ], collapse = ", " ) 
        #         )   #
        #         #
        #         stop( msg ) 
        #     }   #
        #     #
        #     data[, sRow ] <- do.call( 
        #         what = "cbind", 
        #         args = lapply( 
        #             X   = sRow, 
        #             FUN = function(X){ 
        #                 res <- paste( "`", X, "` = ", data[, X ], sep = "" ) 
        #                 return( res ) 
        #             }   # 
        #         )   #
        #     )   #
        #     #
        #     const <- do.call( 
        #         what = "paste", 
        #         args = c( lapply( 
        #             X   = sRow,
        #             FUN = function(X){ data[,X] } 
        #             ), sep = " AND "  
        #         )   # 
        #     )   #
        #     #
        #     const <- paste( "WHERE ", const, sep = " " )  
        #     #
        #     dataCol <- dataCol[ !testSRow ]
        # }else{ 
        #     stop( "If 'mode' is 'u', then 'sRow' can't be NULL" )
        # }   #
        #
        if( !(pKey %in% colnames(data)) ){ 
            stop( "When 'pKey' is not NULL, 'pKey' must be provided in 'data' (column names)." ) 
            # const <- paste( "WHERE (([", tableName, "].[", pKey, "]) = ", data[, pKey ], ")", sep = "" )  
        }   #
        #
        # dataCol <- dataCol[ dataCol != pKey ] 
        # #
        # data <- data[, dataCol, drop = FALSE ] 
        #
        oldOptions <- options( "warn" )[[ 1 ]]  
        options( "warn" = 1 )  
        #
        msg <- sprintf( 
            fmt = "Error detected in .edbOperation.RODBC_Access() in edbWrite.RODBC_Access() (database: %s; table: %s), update mode. Database connection closed.\n", 
            edb[["dbName"]], tableName 
        )   #
        #
        res <- .edbOperation.RODBC_Access(
            edb          = edb, 
            expr         = expression({ 
                exprOut <- sqlUpdate( 
                    channel = dbCon, 
                    ... 
                )   #
            }), #
            errorClasses = c("simpleError","error","condition"),  
            stopOnError  = TRUE, 
            errorMessage = msg, 
            # ... options for expr:
            dat          = data, 
            tablename    = tableName, 
            index        = pKey, 
            # query      = sqlUpdate,    
            ... 
        )   #
        #
        # res <- lapply( 
        #     X   = 1:nrow( data ), 
        #     FUN = function(X){ 
        #         # Create the SQL update statement
        #         sqlUpdate <- paste( 
        #             sep = "", 
        #             "UPDATE [", tableName, "] ", 
        #             "SET ", paste( 
        #                 "[", tableName, "].[", dataCol, "]", 
        #                 " = ", 
        #                 data[X,], 
        #                 collapse = ", ", # Not AND 
        #                 sep      = ""
        #             ),  "\n", 
        #             const[ X ], ";\n\n"  
        #         )   #
        #         #
        #         if( verbose ){ 
        #              cat( sqlUpdate ) 
        #         }    # 
        #         #
        #         msg <- sprintf( 
        #             fmt = "Error detected in dbGetQuery() in edbWrite.RODBC_Access() (database: %s; table: %s; row: %s). Database connection closed.\n", 
        #             edb[["dbName"]], tableName, as.character(X) 
        #         )   #
        #         #
        #         res <- .edbOperation.RODBC_Access(
        #             edb          = edb, 
        #             expr         = expression({ 
        #                 exprOut <- sqlQuery( 
        #                     channel = dbCon, 
        #                     ... 
        #                 )   #
        #             }),  #
        #             errorClasses = c("simpleError","error","condition"),  
        #             stopOnError  = TRUE, 
        #             errorMessage = msg, 
        #             # ... options for expr:
        #             query        = sqlUpdate,    
        #             ... 
        #         )   #
        #         #
        #         return( res ) 
        #     }   #
        # )   #
        # #
        # res <- unlist( res ) 
        # #
        # options( "warn" = oldOptions )
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
            stop( "Warning(s) detected in MS Access transaction. type warnings() to see it/them." ) 
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
            fun             = "edbWrite.RODBC_Access", 
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






"[<-.RODBC_Access" <- function(# "[<-" method for MS Access databases. Write data in a MS Access table in a database (referenced by 'edb').
### "[<-" method for MS Access databases. Write data in a table in a 
### MS Access database (referenced by 'edb'). 

##seealso<< \code{\link{edb}}, \code{\link{edbWrite.RODBC_Access}}, 
## \code{\link{edbRead.RODBC_Access}}.

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
### Only used if getKey is not NULL.

 dateFormat="", 
### Single character string. 'format' argument of the functions 
### format.Date() used to convert "Date" 
### dates into character strings when writing into the database.
### Only used if getKey is not NULL.

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
### \code{!is.null(getKey)} (not the default). If \code{sRow} is not 
### NULL, then data must contain the column names given in \code{sRow}.

){  #
    if( !is.null(getKey) ){ 
         stop( "'getKey' must be NULL to use '[<-' methods." )
    }   #
    #
    res <- edbWrite.RODBC_Access( 
        edb         = edb,
        tableName   = tableName, 
        data        = value, 
        mode        = mode, 
        pKey        = pKey, 
        getKey      = getKey, 
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






edbDelete.RODBC_Access <- function(# Delete all or some rows in a table in a MS Access database (referenced by 'edb').
### Delete all or some rows in a table in a MS Access database (referenced by 'edb'). 

##seealso<< \code{link{edb}}, \code{link{edbWrite.RODBC_Access}}, 
## \code{link{edbNames.RODBC_Access}}, 
## \code{link{edbColnames.RODBC_Access}}.

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
                        const <- paste( "'", const, "'", sep = "" ) 
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
    msg <- sprintf( 
        fmt = "Error detected in sqlQuery() in edbDelete.RODBC_Access() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    out <- .edbOperation.RODBC_Access(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- sqlQuery( 
                channel = dbCon, 
                ...  
            )   #
        }),  #
        errorClasses = c("simpleError","error","condition"),  
        stopOnError  = TRUE, 
        errorMessage = msg, 
        # ... options for expr:
        query        = statement, 
        #case        = "nochange", 
        ... 
    )   #
    #
    if( logOp )
    {   #
        tmp <- edbLog(
            edb             = edb,
            tableName       = tableName, 
            fun             = "edbDelete.RODBC_Access", 
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






edbDrop.RODBC_Access <- function(# Drop a table in a MS Access database (referenced by 'edb').
### Drop a table in a MS Access database (referenced by 'edb'). 

##seealso<< \code{link{edb}}, \code{link{edbWrite.RODBC_Access}}, 
## \code{link{edbNames.RODBC_Access}}, 
## \code{link{edbColnames.RODBC_Access}}.

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
            "DROP TABLE [", tableName, "];\n"  
        )   #
    #
    if( verbose ){ 
        cat( "SQL statement:\n" ) 
        cat( statement, sep = "\n" )
    }   #
    #
    msg <- sprintf( 
        fmt = "Error detected in sqlQuery() in edbDrop.RODBC_Access() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    out <- .edbOperation.RODBC_Access(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- sqlQuery( 
                channel = dbCon, 
                ...  
            )   #
        }),  #
        errorClasses = c("simpleError","error","condition"),  
        stopOnError  = TRUE, 
        errorMessage = msg, 
        # ... options for expr:
        query        = statement, 
        #case        = "nochange", 
        ... 
    )   #
    #
    if( logOp )
    {   #
        tmp <- edbLog(
            edb             = edb,
            tableName       = tableName, 
            fun             = "edbDrop.RODBC_Access", 
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


