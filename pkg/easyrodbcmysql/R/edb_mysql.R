
.edbSearchPath <- function(# Utility function. Search if a given file is present along the system PATH. 
### Utility function. Search if a given file is present along the 
### system PATH (MS Windows?). Function created after some code 
### from Vladimir Eremeev, who created it after Brian Ripley. 
### <http://finzi.psych.upenn.edu/R/Rhelp02/archive/101078.html> 
### or <http://finzi.psych.upenn.edu/R/Rhelp02/archive/92829.html> 

 fileName, 
### Name of the file to be tested.

 sPath=Sys.getenv("PATH"),
### Value of windwos PATH variable. Default is set to 
### Sys.getenv("PATH") 

 pSep=.Platform$path.sep, 
### Platform specific folder separator in pathes. Default set to 
### .Platform$path.sep 
 
 returnPath=FALSE
### Single logical. If TRUE, a text string is returned with the 
### path of the folder in which 'fileName' is located, and an 
### empty string is returned if nothing is found. If FALSE, then 
### only a logical TRUE / FALSE is returned.

){  #
    sPath <- strsplit(x = sPath, split = pSep)[[1]] 
    #
    if(returnPath)
    {   #
        tmp <- lapply(
            X   = sPath, 
            FUN = function(X){
                fp <- file.path(X, fileName) 
                #
                if(file.exists(fp))
                {   #
                    res <- fp
                }else{
                    res <- ""
                }   #
                #
                return( res ) 
            }   #
        )   #
        #
        tmp <- unlist(tmp)
        #
        tmp <- tmp[ tmp != "" ]
        #
        if( length(tmp) == 0 ){ 
            tmp <- "" 
        }   #
    }else{
        tmp <- lapply(
            X   = sPath, 
            FUN = function(X){ 
                fp <- file.path(X, fileName) 
                #
                return( file.exists(fp) ) 
            }   #
        )   #
        tmp <- any(unlist(tmp))
    }   #
    #
    return( tmp[1] ) 
}   #






edbDataSource.RODBC_MySQL <- function(# Create an RODBC MySQL data source (from an 'edb' reference). Only for MS Windows RODBC databases.
### Create an RODBC MySQL data source (from an 'edb' reference). 
### Only for MS Windows RODBC databases. Generic function that call 
### class-specific method corresponding to the class of the 
### \code{edb} object provided. Notice that the exectutable 
### odbcconf.exe should be on Windows path, otherwise the command 
### will fail.

##seealso<<\code{\link[easydb]{edbDataSource}}, \code{\link[easydb]{edb}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

 trash=FALSE, 
### Single logical. If TRUE, then a dummy ODBC data source will 
### be created

 mysqlOdbcDriver="MySQL ODBC 5.1 Driver", 
### Single character string. Name of the MySQL ODBC Driver, as 
### displayed in the ODBC data source management GUI.

 verbose=FALSE, 
### Single logical. Print the command passed to odbcconf.exe?

 ...
### Additional parameters to be passed to \code{system2}.

){  #
    if( Sys.info()[["sysname"]] != "Windows" )
    {   #
        stop( "edbDataSource.RODBC_MySQL curremtly only work under MS Windows." )
    }   #
    #
    odbcCmd <- .edbSearchPath( 
        fileName   = "odbcconf.exe", 
        returnPath = TRUE 
    )   #
    #
    if( odbcCmd == "" )
    {   #
        stop( "odbcconf.exe could not be found on Windows PATH. Please check this." ) 
    }   #
    #
    if( trash ) 
    {   #
        odbcArgs <- paste(
            sep = "", 
            " /a {", "CONFIGDSN \"", mysqlOdbcDriver, "\" ", 
            "\"DSN=", edb[["dbSourceName"]], ";", 
            "SERVER=", "127.0.0.1", ";", 
            "DESCRIPTION=", edb[["dbSourceName"]], ";", 
            "DATABASE=", "trashDb", ";", 
            "PORT=", 666, 
            "\"}" 
        )   #
    }else{
        odbcArgs <- paste(
            sep = "", 
            " /a {", "CONFIGDSN \"", mysqlOdbcDriver, "\" ", 
            "\"DSN=", edb[["dbSourceName"]], ";", 
            "SERVER=", edb[["dbHost"]], ";", 
            "DESCRIPTION=", edb[["dbSourceName"]], ";", 
            "DATABASE=", edb[["dbName"]], ";", 
            "PORT=", edb[["dbPort"]], 
            "\"}" 
        )   #
    }   #
    #
    if( verbose ){ 
        cat( odbcCmd, "\n") 
        cat( odbcArgs, "\n" ) 
    }   #
    #
    sysRes <- system2(
        command   = odbcCmd, 
        args      = odbcArgs, 
        wait      = TRUE, 
        minimized = FALSE, 
        invisible = TRUE, 
        ... 
    )   #
    #
    if( verbose ) 
    {   #
        if( trash )
        {   #
            cat("MySQL ODBC data source presumably overwritten with dummy info.\n")
        }else{
            cat("MySQL ODBC data source presumably set-up.\n")
        }   #
    }   #
    #
    cat("Check under Windows start menu > control panel > Administrative tools > Data sources (ODBC) > User DSN.\n" ) 
    #
    return( sysRes ) 
### Returns the output of \code{system2}.
}   #






.edbOperation.RODBC_MySQL <- function(# Connect to a MySQL database (referenced by 'edb'), do some operation and close the database.
### Connect to a database (referenced by 'edb'), do some 
### operation and close the database. Generic function 
### that call class-specific method corresponding to the 
### class of the \code{edb} object provided. In more details, 
### the function open a connection to the database, then 

##seealso<<\code{\link[easydb]{.edbOperation}}, \code{\link[easydb]{edb}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

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

 maxCon=1,  
### Single integer. See ?dbDriver 

 ...
### Additional parameters to be passed to some function in \code{expr}.

){  # Empty output:
    exprOut <- NULL 
    #
    require( "RODBC" ) 
    #
    # Test that the ODBC datasource has been set:
    sourceTest <- edb[[ "dbSourceName" ]] %in% names( odbcDataSources() ) 
    if( !sourceTest ) 
    {   #
        stop( paste(
            sep = "", 
            "The ODBC data source demanded (edb$'dbSourceName' = '", 
            edb[[ "dbSourceName" ]], 
            "') could not be found in the list of ODBC data sources ", 
            "(odbcDataSources()).\nRun edbDataSource(edb) or fix the source manually in ODBC." 
        ) ) #
    }   #
    #
    dbCon <- odbcConnect( 
        dsn  = edb[[ "dbSourceName" ]], 
        uid  = edb[[ "dbLogin" ]], 
        pwd  = edb[[ "dbPwd" ]], 
        case = "nochange"  
    )   #
    #
    if( any( dbCon == -1 ) ){ 
        stop( sprintf( "Connexion to ODBC source %s failed.", edb[[ "dbName" ]] ) ) 
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






edbColnames.RODBC_MySQL <- function(# Retrieve column names of a table in a MySQL database (referenced by 'edb').
### Retrieve column names of a table in a MySQL database 
### (referenced by 'edb'). Wrapper around RODBC::sqlColumns().
### Notice that the method does NOT retrieve the full table to 
### get its column names (so it should work even if the table is big).

##seealso<< \code{\link[easydb]{edb}}, \code{\link{edbRead.RODBC_MySQL}}, 
## \code{\link{edbWrite.RODBC_MySQL}}, 
## \code{\link{edbNames.RODBC_MySQL}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 onlyNames=TRUE, 
### Single logical. If TRUE only returns the column names (vector), 
### and if FALSE returns a detailed table as in 
### \code{\link[RODBC]{sqlColumns}}.

 ...
### Additional parameters to be passed to dbListFields(). See 
### \code{?dbListFields}.

){  #
    msg <- sprintf( 
        fmt = "Error detected in sqlColumns() in edbColnames.RODBC_MySQL() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    tbl <- .edbOperation.RODBC_MySQL(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- sqlColumns(
                channel = dbCon, 
                ... 
            )   #
        }), #
        maxCon       = 1,  
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
### columns / fields of the original MySQL table, or a table with 
### full details on the columns (see \code{onlyNames}).
}   #






edbRead.RODBC_MySQL <- function(# Read all or part of a table in a MySQL database (referenced by 'edb').
### Read all or part of a table in a MySQL database (referenced by 'edb'). 

##seealso<< \code{\link[easydb]{edb}}, \code{\link{edbWrite.RODBC_MySQL}}, 
## \code{\link{edbNames.RODBC_MySQL}}, 
## \code{\link{edbColnames.RODBC_MySQL}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

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
### \link[base]{as.Date} to the column "DATE".

 distinct=FALSE, 
### Single logical. If TRUE, unique values in the result table will 
### be returned, using the \code{SELECT DISTINCT} SQL statement. 
### This is equivalent to applying \code{\link[base]{unique}} to the 
### data.frame returned by the function, except that the action is 
### performed inside the database (not in R).

 orderBy=NULL, 
### Vector of character strings, or NULL (the default). If non NULL, 
### vector of column names that must be used to sort the result table. 
### Column names may be followed by a space and 'DESC' if the column 
### must be sorted in a descending order ('ASC', ascending, is the 
### default). This operation is performed in the database with 
### SQL ORDER BY statement and is equivalent to ordering the 
### data in R with \code{\link[base]{order}}. You may write the 
### column names between `` delimiters if they contain spaces.

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
    # Prepare the list of columns to choose in the table:
    selectWhat <- easydb:::.edb.sCol( 
        edb       = edb, 
        sCol      = sCol, 
        tableName = tableName, 
        colQ = c("`","`") 
    )   #
    sCol       <- selectWhat[[ "sCol" ]] 
    selectWhat <- selectWhat[[ "selectWhat" ]] 
    # 
    # Prepare the 1st series of constrains:
    sRow <- easydb:::.edb.sRow( # Create row constrains
        sRow    = sRow, 
        sRowOp  = sRowOp, 
        charQ   = "\"", 
        colQ    = c("`","`") 
    )   #
    #
    # DISTINCT statement
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
            "FROM `", tableName, "`\n", 
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
    msg <- sprintf( 
        fmt = "Error detected in sqlQuery() in edbRead.RODBC_MySQL() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    tbl <- .edbOperation.RODBC_MySQL(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- sqlQuery( 
                channel = dbCon, 
                ...  
            )   #
        }),  #
        maxCon       = 1,  
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
        fieldsRes <- edbColnames.RODBC_MySQL( 
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
    tbl <- easydb:::.formatCol( 
        x         = tbl, 
        formatCol = formatCol 
    )   #
    #
    return( tbl ) 
### The function returns the requested table. 
}   #






edbNames.RODBC_MySQL <- function(# Retrieve table names in a MySQL database (referenced by 'edb').
### Retrieve table names in a MySQL database (referenced by 'edb'). 

##seealso<< \code{\link[easydb]{edb}}, \code{\link{edbRead.RODBC_MySQL}}, 
## \code{\link{edbWrite.RODBC_MySQL}}, 
## \code{\link{edbColnames.RODBC_MySQL}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

 onlyNames=TRUE, 
### Single logical. If TRUE only returns the table names (vector), 
### and if FALSE returns a detailed table as in 
### \code{\link[RODBC]{sqlTables}}.

 ...
### Additional parameters to be passed to \code{dbListTables}.

){  #
    msg <- sprintf( 
        fmt = "Error detected in sqlTables() in edbNames.RODBC_MySQL() (database: %s). Database connection closed.\n", 
        edb[["dbName"]] 
    )   #
    #
    tbl <- .edbOperation.RODBC_MySQL(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- sqlTables(
                channel = dbCon,  
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
    if( onlyNames ){ 
        tbl <- tbl[,"TABLE_NAME"] 
    }   #
    #
    return( tbl ) 
### The function returns the list of tables found in the database. 
}   #






"[.RODBC_MySQL" <- function(# "[" method for reading all or part of a table in a MySQL database (referenced by 'edb').
### "[" method for reading all or part of a table in a MySQL 
### database (referenced by 'edb'). Wrapper for 
### \code{\link{edbRead.RODBC_MySQL}}. 

##seealso<< \code{\link[easydb]{edb}}, \code{\link{edbRead.RODBC_MySQL}}, 
## \code{\link{edbNames.RODBC_MySQL}}, 
## \code{\link{edbColnames.RODBC_MySQL}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

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
### \link[base]{as.Date} to the column "DATE".

 distinct=FALSE, 
### Single logical. If TRUE, unique values in the result table will 
### be returned, using the \code{SELECT DISTINCT} SQL statement. 
### This is equivalent to applying \code{\link[base]{unique}} to the 
### data.frame returned by the function, except that the action is 
### performed inside the database (not in R).

 verbose=FALSE, 
### Single logical. If TRUE, information on what is done are output 
### on screen.

 ...
### Additional parameters to be passed to \code{dbGetQuery}.

){  
    if( missing( edb ) ){ stop( "Argument 'edb' is missing, with no default" ) } 
    
    if( missing( tableName ) ){ stop( "Argument 'tableName' is missing, with no default" ) } 
    
    tbl <- edbRead.RODBC_MySQL(       
        edb       = edb, 
        tableName = tableName, 
        sRow      = sRow,
        sCol      = sCol,
        sRowOp    = sRowOp,
        verbose   = verbose, 
        formatCol = formatCol, 
        distinct  = distinct, 
        ...
    )   #
    
    return( tbl ) 
}   






.edbSendGetQuery.RODBC_MySQL <- function(# Internal. Mixes dbSendQuery() and dbGetQuery()
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






edbWrite.RODBC_MySQL <- function(# Write data in a MySQL table in a database (referenced by 'edb').
### Write data in a table in a MySQL database (referenced by 'edb'). 

##seealso<< \code{\link[easydb]{edb}}, \code{\link{edbRead.RODBC_MySQL}}, 
## \code{\link{edbNames.RODBC_MySQL}}, 
## \code{\link{edbColnames.RODBC_MySQL}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

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
### \link[base]{as.Date} to the column "DATE".

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
### into the database, using the function \code{\link[easydb]{edbLog}}. 
### See the arguments below and \code{\link[easydb]{edbLog}} for more details.

 logRandId=rnorm(1), 
### Single numerical. See \code{\link[easydb]{edbLog}}.

 logMsg=as.character(NA), 
### Single character string. See \code{\link[easydb]{edbLog}}.

 logTableName="edbLog", 
### Single character string. See \code{\link[easydb]{edbLog}}.

 logCreateTableIfNotExist=TRUE, 
### Single logical. See \code{\link[easydb]{edbLog}}.

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
    data <- easydb:::.formatCol( 
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
                fmt = "Error detected in dbWriteTable() in edbWrite.RODBC_MySQL() (database: %s; table: %s). Database connection closed.\n", 
                edb[["dbName"]], tableName 
            )   #
            #
            oldOptions <- options( "warn" )[[ 1 ]] 
            options( "warn" = 1 )  
            #
            res <- .edbOperation.RODBC_MySQL(
                edb          = edb, 
                expr         = expression({ 
                    exprOut <- sqlSave( 
                        channel = dbCon, 
                        ... 
                    )   #
                }),  #
                maxCon       = 1,  
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
            data <- easydb:::.formatTable4Query( 
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
                        "INSERT INTO `", tableName, "`\n", 
                        "(", paste( "`", dataCol, "`", collapse = ",", sep = "" ), ")\n", 
                         "VALUES(", paste( as.character( data[X,] ), collapse = "," ), ");\n"    
                    )   #
                    #
                    if( verbose ){ 
                         cat( sqlUpdate ) 
                    }    # 
                    #
                    sqlUpdate2 <- paste( 
                        sep = "", 
                        "SELECT `", 
                        getKey, 
                        "` FROM `", 
                        tableName, 
                        "` WHERE `", 
                        getKey, 
                        "` = last_insert_id();\n" 
                    )   #
                    #
                    if( verbose ){ 
                         cat( sqlUpdate2 ) 
                    }    # 
                    #
                    msg <- sprintf( 
                        fmt = "Error detected in .edbSendGetQuery.RODBC_MySQL() in edbWrite.RODBC_MySQL() (database: %s; table: %s; row: %s). Database connection closed.\n", 
                        edb[["dbName"]], tableName, as.character(X) 
                    )   #
                    #
                    newId <- .edbOperation.RODBC_MySQL(
                        edb          = edb, 
                        expr         = expression({ 
                            exprOut <- .edbSendGetQuery.RODBC_MySQL( channel = dbCon, ... )
                        }), #
                        maxCon       = 1,  
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
        data <- easydb:::.formatTable4Query( 
            data        = data, 
            del         = "'", 
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
        if( pKey %in% colnames(data) ){ 
            const <- paste( "WHERE (`", pKey, "` = ", data[, pKey ], ")", sep = "" )  
        }else{ 
            stop( "When 'pKey' is not NULL, 'pKey' must be provided in 'data' (column names)." )
        }   #
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
                    "UPDATE `", tableName, "`\n", 
                    "SET ", paste( 
                        "`", dataCol, "`", 
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
                    fmt = "Error detected in dbGetQuery() in edbWrite.RODBC_MySQL() (database: %s; table: %s; row: %s). Database connection closed.\n", 
                    edb[["dbName"]], tableName, as.character(X) 
                )   #
                #
                res <- .edbOperation.RODBC_MySQL(
                    edb          = edb, 
                    expr         = expression({ 
                        exprOut <- sqlQuery( 
                            channel = dbCon, 
                            ... 
                        )   #
                    }),  #
                    maxCon       = 1,  
                    errorClasses = c("simpleError","error","condition"),  
                    stopOnError  = TRUE, 
                    errorMessage = msg, 
                    # ... options for expr:
                    query        = sqlUpdate,    
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
            stop( "Warning(s) detected in MySQL transaction. type warnings() to see it/them." ) 
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
            fun             = "edbWrite.RODBC_MySQL", 
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






"[<-.RODBC_MySQL" <- function(# "[<-" method for MySQL databases. Write data in a MySQL table in a database (referenced by 'edb').
### "[<-" method for MySQL databases. Write data in a table in a 
### MySQL database (referenced by 'edb'). 

##seealso<< \code{\link[easydb]{edb}}, \code{\link{edbWrite.RODBC_MySQL}}, 
## \code{\link{edbRead.RODBC_MySQL}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

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
### \link[base]{as.Date} to the column "DATE".

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

#  logOp=FALSE, 
# ### Single logical. If TRUE, then a log of the operation is written 
# ### into the database, using the function \code{\link[easydb]{edbLog}}. 
# ### See the arguments below and \code{\link[easydb]{edbLog}} for more details.

#  logRandId=rnorm(1), 
# ### Single numerical. See \code{\link[easydb]{edbLog}}.

#  logMsg=as.character(NA), 
# ### Single character string. See \code{\link[easydb]{edbLog}}.

#  logTableName="edbLog", 
# ### Single character string. See \code{\link[easydb]{edbLog}}.

#  logCreateTableIfNotExist=TRUE, 
# ### Single logical. See \code{\link[easydb]{edbLog}}.

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

){  
    if( missing( edb ) ){ stop( "Argument 'edb' is missing, with no default" ) } 
    
    if( missing( tableName ) ){ stop( "Argument 'tableName' is missing, with no default" ) } 
    
    if( !is.null(getKey) ){ 
         stop( "'getKey' must be NULL to use '[<-' methods." )
    }   #
    
    res <- edbWrite.RODBC_MySQL( 
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
        # logOp       = logOp, 
        # logRandId   = logRandId, 
        # logMsg      = logMsg, 
        # logTableName= logTableName, 
        # logCreateTableIfNotExist=logCreateTableIfNotExist, 
        parano      = parano, 
        ...
    )   
    
    return( edb ) 
}   






edbDelete.RODBC_MySQL <- function(# Delete all or some rows in a table in a MySQL database (referenced by 'edb').
### Delete all or some rows in a table in a MySQL database (referenced by 'edb'). 

##seealso<< \code{\link[easydb]{edb}}, \code{\link{edbWrite.RODBC_MySQL}}, 
## \code{\link{edbNames.RODBC_MySQL}}, 
## \code{\link{edbColnames.RODBC_MySQL}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

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
### into the database, using the function \code{\link[easydb]{edbLog}}. 
### See the arguments below and \code{\link[easydb]{edbLog}} for more details.

 logRandId=rnorm(1), 
### Single numerical. See \code{\link[easydb]{edbLog}}.

 logMsg=as.character(NA), 
### Single character string. See \code{\link[easydb]{edbLog}}.

 logTableName="edbLog", 
### Single character string. See \code{\link[easydb]{edbLog}}.

 logCreateTableIfNotExist=TRUE, 
### Single logical. See \code{\link[easydb]{edbLog}}.

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
    # Prepare the 1st series of constrains:
    sRow <- easydb:::.edb.sRow( # Create row constrains
        sRow    = sRow, 
        sRowOp  = sRowOp, 
        charQ   = "\"", 
        colQ    = c("`","`") 
    )   #
    #
    # Create the full querry statement:
    statement <- paste( 
            sep = "", 
            "DELETE FROM `", tableName, "`\n", 
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
        fmt = "Error detected in sqlQuery() in edbRead.RODBC_MySQL() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    out <- .edbOperation.RODBC_MySQL(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- sqlQuery( 
                channel = dbCon, 
                ...  
            )   #
        }),  #
        maxCon       = 1,  
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






edbDrop.RODBC_MySQL <- function(# Drop a table in a MySQL database (referenced by 'edb').
### Drop a table in a MySQL database (referenced by 'edb'). 

##seealso<< \code{\link[easydb]{edb}}, \code{\link{edbWrite.RODBC_MySQL}}, 
## \code{\link{edbNames.RODBC_MySQL}}, 
## \code{\link{edbColnames.RODBC_MySQL}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 logOp=FALSE, 
### Single logical. If TRUE, then a log of the operation is written 
### into the database, using the function \code{\link[easydb]{edbLog}}. 
### See the arguments below and \code{\link[easydb]{edbLog}} for more details.

 logRandId=rnorm(1), 
### Single numerical. See \code{\link[easydb]{edbLog}}.

 logMsg=as.character(NA), 
### Single character string. See \code{\link[easydb]{edbLog}}.

 logTableName="edbLog", 
### Single character string. See \code{\link[easydb]{edbLog}}.

 logCreateTableIfNotExist=TRUE, 
### Single logical. See \code{\link[easydb]{edbLog}}.

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
    # Create the full querry statement:
    statement <- paste( 
            sep = "", 
            "DROP TABLE `", tableName, "`;\n"  
        )   #
    #
    if( verbose ){ 
        cat( "SQL statement:\n" ) 
        cat( statement, sep = "\n" )
    }   #
    #
    msg <- sprintf( 
        fmt = "Error detected in sqlQuery() in edbRead.RODBC_MySQL() (database: %s; table: %s). Database connection closed.\n", 
        edb[["dbName"]], tableName 
    )   #
    #
    out <- .edbOperation.RODBC_MySQL(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- sqlQuery( 
                channel = dbCon, 
                ...  
            )   #
        }),  #
        maxCon       = 1,  
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
            fun             = "edbDrop.RODBC_MySQL", 
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






edbQuery.RODBC_MySQL <- function(# Send and retrieve a query in a SQLite database (referenced by 'edb').
### Send and retrieve a query in a SQLIte database (referenced by 'edb'). 

##seealso<< \code{\link[easydb]{edb}}, \code{link[RODBC]{sqlQuery}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

 statement, 
### Single character string. SQL statement / SQL query to be passed 
### to \code{link[RODBC]{sqlQuery}}.

 formatCol=NULL, 
### If not NULL, a named list of functions to be applied to certain columns 
### after the data has been extracted from the database. The name of each list 
### item gives the column to process, and the value of each item gives the 
### function that must be applied. For instance 
### formatCol = list("DATE"=as.Date) will apply the function 
### \link[base]{as.Date} to the column "DATE".

#  testFiles=TRUE,  
# ### Single logical. Should the function test for the presence 
# ### (file.exist()) of the needed files in the folder before trying 
# ### to fetch information from the database? 

 verbose=FALSE, 
### Single logical. If TRUE, information on what is done are output 
### on screen.

 ...
### Additional parameters to be passed to \code{link[RODBC]{sqlQuery}}.

){  #
#     if( testFiles ) 
#     {   # Check if the database files is present:
#         easydb:::.edbFileExists( edb[[ "dbName" ]] ) 
#     }   #
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
        fmt = "Error detected in sqlQuery() in edbQuery.RSQLite_SQLite() (database: %s). Database connection closed.\n", 
        edb[["dbName"]] 
    )   #
    #
    qRes <- .edbOperation.RODBC_MySQL(
        edb          = edb, 
        expr         = expression({ 
            exprOut <- sqlQuery( 
                channel = dbCon, 
                ...  
            )   #
        }),  #
        # maxCon     = 1,  
        errorClasses = c("simpleError","error","condition"),  
        stopOnError  = TRUE, 
        errorMessage = msg, 
        # ... options for expr:
        query        = statement, 
        ... 
    )   #
    #
    qRes <- easydb:::.formatCol( 
        x         = qRes, 
        formatCol = formatCol 
    )   #
    #
    return( qRes ) 
### The function returns the requested table. 
}   #






edbNRow.RODBC_MySQL <- function(# Retrieve the number of rows of a table in a database (referenced by 'edb').
### Retrieve the number of rows of a table in a database (referenced by 
### 'edb'). Notice that the methods do NOT retrieve the full table to 
### get its column names (so it should work even if the table is big).

##seealso<< \code{\link[easydb]{edb}}, \code{\link[easydb]{edbColnames}}, 
## \code{\link[easydb]{edbRead}}.

 edb,
### An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.

 tableName, 
### Single character string. Name of the table to read in 'edb'.

 sRow=NULL, 
### A list of named items. List of contrains/criterion to be applied 
### on a table to retrieve a subset of rows. Each item in 
### \code{sRow} must be named after the columns on which the constrain 
### apply. The (vector of) value(s) of each items are the possible values 
### that can be retrieved. Values can be character or numeric. If NULL 
### (the default), all values are returned.

 sRowOp=c("AND","OR")[1], 
### A single character string. Operator to be used to combine multiple 
### constrains in sRow. Possible values are "OR" or "AND". Default value 
### is "AND".

 verbose=FALSE, 
### Single logical. If TRUE, information on what is done are output 
### on screen.

 ...
### Additional parameters to be passed to \code{edbQuery}.

){  # Retrieve the dimention of the table:
    statement <- paste("SELECT Count(*) FROM `", tableName, "`", sep = "" ) 
    #
    sRow <- easydb:::.edb.sRow( # Create row constrains
        sRow    = sRow, 
        sRowOp  = sRowOp, 
        charQ   = "\"", 
        colQ    = c("`","`") 
    )   #
    #
    # statement and row constrains:
    statement <- paste( statement, sRow, "\n", sep = " " ) 
    #
    if( verbose ){ cat( statement ) } 
    #
    # Send the query and fetch the result:
    nbrow <- edbQuery( edb = edb, statement = statement, ... )[,] 
    #
    return( nbrow ) 
### Returns the number of rows in the table, rows that respect \code{sRow} 
### constrains if \code{sRow} is not \code{NULL}.
}   #


