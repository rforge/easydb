





ML.create.trigger <- function(# Internal. Create an SQLite trigger for referential integrity check.
### Internal. Create an SQLite trigger for integrity check. 
### The trigger will make sure that future insert, delete and update 
### statements respect referential integrity (foreign keys)

 table.name,
### Single character string. Name of the table for which a trigger 
### must be created;

 foreign.key,
### Single character string. Name of the foreign key in 'table.name'.

 primary.table.name,
### Single character string. Name of the table that has a primary key 
### that serves as a reference for 'foreign.key' in 'table.name'.

 primary.key,
### Single character string. Name of the primary key in 
### 'primary.table.name' that is linked to 'foreign.key' in 
### 'table.name'.

 db.dir=NULL,  
### Single character string, containing the path of the folder where 
### is located the SQLite database named 'db.name' in which the parameters 
### will be written. If no database is found there it is created. 
### Default value ML.get("db.dir"). 
 
 db.name=NULL,  
### Single character string, name (and extension) of the SQLite database 
### where the parameters should be written. The path of this database is 
### 'db.dir'. If no database is found, it is created.
### Default value ML.get("db.name"). 

 sqlite3.exe=NULL,  
### Name of the SQLite3 command line program. 

 test.files=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 error.keywords=NULL,  
### Vector of character strings. List of the keywords that must be 
### searched in 'shell.res'. 

 pkgname=NULL,  
### Single character string. Name of the package. Used to fetch some 
### files in the package directory. 

 libname=NULL   
### Single character string. name of the library = folder in which 
### the pacakge is installef. Used to fetch some files in the package 
### directory. 

){  # Automatically set the NULL parameters to default:
    ML.auto.set() 
    #
    if( test.files ) 
    {   # Check if the database files is present:
        ML.file.check.warn( 
            file.wd    = db.dir,
            file.names = db.name  
        )   #
    }   #
    #
    require( "DBI" ) 
    require( "RSQLite" ) 
    #
    # Retreive the scenario table here
    #
    # Write the SQL statement here.
    statement <- paste( 
        sep = "", 
        "CREATE TRIGGER IF NOT EXISTS FK_INSERT_", table.name, "_", foreign.key, "\n", 
        "    BEFORE INSERT ON ", table.name, "\n", 
        "    FOR EACH ROW BEGIN\n", 
        "        SELECT CASE\n", 
        "        WHEN ((SELECT ", primary.key, " FROM ", primary.table.name, " WHERE ", primary.table.name, ".", primary.key, " = NEW.", foreign.key, ") IS NULL)\n", 
        "        THEN RAISE(ROLLBACK, \"Insert on table [", table.name, "] violates foreign key constraint\")\n", 
        "    END;\n", 
        "END;\n", 
        "CREATE TRIGGER IF NOT EXISTS FK_UPDATE_", table.name, "_", foreign.key, "\n", 
        "    BEFORE UPDATE ON ", table.name, "\n", 
        "    FOR EACH ROW BEGIN\n", 
        "        SELECT CASE\n", 
        "        WHEN ((SELECT ", primary.key, " FROM ", primary.table.name, " WHERE ", primary.table.name, ".", primary.key, " = NEW.", foreign.key, ") IS NULL)\n", 
        "        THEN RAISE(ROLLBACK, \"Update on table [", table.name, "] violates foreign key constraint\")\n", 
        "    END;\n", 
        "END;\n", 
        "CREATE TRIGGER IF NOT EXISTS FK_DELETE_", table.name, "_", foreign.key, "\n", 
        "    BEFORE DELETE ON ", primary.table.name, "\n", 
        "    FOR EACH ROW BEGIN\n", 
        "    DELETE FROM ", table.name, " WHERE ", table.name, ".", foreign.key, " = OLD.", primary.key, ";\n", 
        "END;\n"  
    )   #
    # cat( statement ) 
    #
    # Send the statement to the database (create triggers if they don't exists yet)
    tbl <- ML.db.connect.and.tryCatch( 
        db.dir          = db.dir,  
        db.name         = db.name,  
        expr            = expression({ 
            expr.out <- dbGetQuery( # Instead of dbSendQuery
                conn    = db.con, 
                ...
            )   #
        }), #
        drvName         = "SQLite",
        max.con         = 1,  
        error.classes   = c("simpleError","error","condition"),  
        stop.on.error   = TRUE, 
        error.message   = "Error detected in dbGetQuery() in ML.create.trigger(). Database safely closed.", 
        # ... options passed to expr:
        statement       = statement 
    )   #
    #
    return( invisible( statement ) ) 
### The function invisibly returns the SQL statement that was send 
### to the database.
}   #











ML.db.connect.and.tryCatch <- function(# Open the driver, connect to the database and send an expression with tryCatch
### Internal function. Open a MACRO SQLite database driver, connect 
### to a database, send an expression to be ran on the database, 
### but wrap it in tryCatch to catch eventual errors, and eventually 
### stop() with a proper error message, after safely closing the 
### database and driver (clean error). 

 db.dir,
### Single character string, containing the path of the folder where 
### is located the SQLite database named 'db.name'.
 
 db.name,
### Single character string, name (and extension) of the SQLite database 
### where the operation 'expr' will be performed.

 expr,
### Single expression, eventually between {}. Expression that will 
### be passed to tryCatch. Additional parameters can be passed to 
### ..., and the database connexion object must be named 'db.con'. 
### If an output is returned, it must be saved in an object called 
### 'expr.out' that is then returned by ML.db.connect.and.tryCatch() 

 drvName="SQLite",
### Single character string. See ?dbDriver 

 max.con=1,  
### Single integer. See ?dbDriver 

 error.classes=c("simpleError","error","condition"),  
### Vector of character strings. Error data classes to be found in 
### tryCatch result.

 stop.on.error=TRUE, 
### Single logical. If TRUE and an error is detected, the function stops 
### AFTER closing the database and driver. If FALSE it just returns 
### the error as an object.

 error.message="An error was detected by tryCatch", 
### Error message to be send if an error is detected. Either as 
### stop message if stop.on.error == TRUE or as a warning 
### stop.on.error == FALSE.

 ...
### Eventual additional parameters required by the expression.

){  # Empty output:
    expr.out <- NULL 
    #
    sqlite.con <- dbDriver( 
        drvName = drvName, 
        max.con = max.con  
    )   #
    #
    db.con <- dbConnect( 
        drv    = sqlite.con, 
        dbname = file.path( db.dir, db.name )  
    )   #
    #
    # Initiate the error catching object:
    catch.res <- NULL 
    #
    catch.res <- tryCatch( 
        expr = eval( expr ),  #
        # What to do with an eventual error message catched (the.error)?
        error = function(the.error){ 
            the.error # just return it.
        },  #
        ... 
    )   #
    #
    ex.res <- dbGetException( db.con ) 
    #
    dbDisconnect( conn = db.con ) 
    #
    dbUnloadDriver( drv = sqlite.con ) 
    #
    if( any( class(catch.res) %in% error.classes ) )
    {   #
        warning( catch.res ) 
        #
        if( stop.on.error )
        {   #
            stop( error.message ) 
        }else{ 
            warning( error.message ) 
        }   #
    }   #
    #
    if( ex.res[["errorNum"]] != 0 )
    {   #
        warning( ex.res[["errorMsg"]] ) 
        #
        if( stop.on.error )
        {   #
            stop( error.message ) 
        }else{ 
            warning( error.message ) 
        }   #
    }   #
    #
    return( expr.out ) 
### The function returns the object 'expr.out' eventually outputed 
### by expr, and NULL otherwise.
}   #







ML.db.connectAccess.and.tryCatch <- function(# Connect to the Access database and send an expression with tryCatch
### Internal function. Connect to an Access database, send an 
### expression to be ran on the database, but wrap it in tryCatch 
### to catch eventual errors, and eventually stop() with a proper 
### error message, after safely closing the database (clean error). 

 db.dir,
### Single character string, containing the path of the folder where 
### is located the Access or DBase database named 'db.name'.
 
 db.name,
### Single character string, name (and extension) of the Access or 
### DBase database where the operation 'expr' is to be performed.

 expr, 
### Single expression, eventually between {}. Expression that will 
### be passed to tryCatch. Additional parameters can be passed to 
### ..., and the database connexion object must be named 'db.con'. 
### If an output is returned, it must be saved in an object called 
### 'expr.out' that is then returned by ML.db.connect.and.tryCatch() 

 error.classes=c("simpleError","error","condition"),  
### Vector of character strings. Error data classes to be found in 
### tryCatch result.

 stop.on.error=TRUE, 
### Single logical. If TRUE and an error is detected, the function stops 
### AFTER closing the database and driver. If FALSE it just returns 
### the error as an object.

 error.message="An error was detected by tryCatch", 
### Error message to be send if an error is detected. Either as 
### stop message if stop.on.error == TRUE or as a warning 
### stop.on.error == FALSE.

 ...
### Eventual additional parameters required by the expression.

){  # Empty output:
    require( "RODBC" ) 
    #
    expr.out <- NULL 
    #
    db.con <- odbcConnectAccess( 
        access.file = file.path( db.dir, db.name ) ) 
    #
    # Initiate the error catching object:
    catch.res <- NULL 
    #
    catch.res <- tryCatch( 
        expr = eval( expr ),  #
        # What to do with an eventual error message catched (the.error)?
        error = function(the.error){ 
            the.error # just return it.
        },  #
        ... 
    )   #
    #
    odbcClose( channel = db.con ) 
    #
    if( any( class(catch.res) %in% error.classes ) )
    {   #
        warning( catch.res ) 
        #
        if( stop.on.error )
        {   #
            stop( error.message ) 
        }else{ 
            warning( error.message ) 
        }   #
        #
        # return( catch.res ) 
    }   #
    #
    return( expr.out ) 
### The function returns the object 'expr.out' eventually outputed 
### by expr, and NULL otherwise.
}   #










ML.dbListFields <- function(# List the fields of a table in an SQLite database.
### Utility function. This function returns a table with the list 
### of columns names, their data types and their default values for 
### a given table in the MACRO SQLite database (or any other SQLite 
### database). 
### Type ML.get("optioname"), with 'optioname' the name of the option, 
### to know its 'real' default value (when NULL).

 table.name="SITE_irrigation_single",  
### Single character string. Name of the table whose fields (columns) 
### characteristics should be fetched. 

 db.dir=NULL,  
### Single character string, containing the path of the folder where 
### is located the SQLite database named 'db.name' in which the parameters 
### will be written. If no database is found there it is created. 
### Default value ML.get("db.dir"). 
 
 db.name=NULL,  
### Single character string, name (and extension) of the SQLite database 
### where the parameters should be written. The path of this database is 
### 'db.dir'. If no database is found, it is created.
### Default value ML.get("db.name"). 

 sqlite3.exe=NULL,  
### Name of the SQLite3 command line program. 

 test.files=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 error.keywords=NULL,  
### Vector of character strings. List of the keywords that must be 
### searched in 'shell.res'. 

 pkgname=NULL,
### Single character string. Name of the package. Used to fetch some 
### files in the package directory. 

 libname=NULL,  
### Single character string. name of the library = folder in which 
### the pacakge is installef. Used to fetch some files in the package 
### directory. 

 PRAGMA=FALSE
### Single logical. If TRUE uses a shell call to SQLIte PRAGMA 
### table_info(). If FALSE uses DBI function dbListFields. 
### A call to PRAGMA returns a table with more information.

){  # Automatically set the default parameters that are NULL:
    ML.auto.set() 
    #
    if( test.files ) 
    {   # Check if the database files is present:
        ML.file.check.warn( 
            file.wd    = db.dir,
            file.names = db.name  
        )   #
        #
        # Copy the sqlite.exe program if it is not already there:
        ML.file.copy.if.absent( 
            from.dir   = file.path( libname, pkgname ), 
            to.dir     = db.dir, 
            file.names = sqlite3.exe, 
            overwrite  = TRUE, 
            recursive  = FALSE  
        )   #
    }   #
    #
    if( PRAGMA )
    {   #
        # Create the sqlite command that will retrieve the 
        # table column names:
        sqlite.cmd <- paste( 
            sep = " ", 
            sqlite3.exe, 
            db.name, 
            "\"PRAGMA table_info(", 
            table.name, 
            ")\""  
        )   #
        #
        # Run this batch file (error catch included):
        shell.res <- ML.shell( 
            cmd            = sqlite.cmd,  
            #intern        = TRUE, 
            #wait          = TRUE, 
            #mustWork      = FALSE, 
            error.keywords = error.keywords, 
            shell.dir      = db.dir,  
            error.file.nm  = "MACRO_LIST_FIELDS_ERROR_LOG.TXT"  
        )   #
        #
        if( length(shell.res) == 0 ) 
        {   #
            stop( 
                paste( 
                    sep = "", 
                    "The call to sqlite3.exe PRAGMA table_info on ", 
                    table.name, 
                    " did not return aything. The table probably does not exist." 
                )   #
            )   #
        }   #
        #
        # Split the results into sub-items, and bind then as
        # table rows:
        tbl <- do.call( 
            what = "rbind", 
            args = strsplit( 
                x        = shell.res, 
                split    = "|", 
                fixed    = TRUE  
            )   #
        )   #
        
        # Convert into a data.frame:
        tbl <- as.data.frame( tbl, stringsAsFactors = FALSE ) 
        #
        #
        # Add column names:
        colnames(tbl) <- c( "NB", "NAME", "TYPE", "NOT_NULL", "DEFAULT", "PRIMARY" ) 
        #
        # Convert numerical data:
        tbl[,"NB"]       <- as.integer( tbl[,"NB"] ) 
        tbl[,"NOT_NULL"] <- as.integer( tbl[,"NOT_NULL"] ) 
        tbl[,"PRIMARY"]  <- as.integer( tbl[,"PRIMARY"] ) 
    }else{ 
        tbl <- ML.db.connect.and.tryCatch(
            db.dir          = db.dir,  
            db.name         = db.name,  
            expr            = expression({ 
                expr.out <- dbListFields(
                    conn      = db.con, 
                    ... 
                )   #
            }),  #
            drvName         = "SQLite",
            max.con         = 1,  
            error.classes   = c("simpleError","error","condition"),  
            stop.on.error   = TRUE, 
            error.message   = "Error detected in dbListFields() in ML.dbListFields(). Database safely closed.", 
            # ... options passed to expr:
            name            = table.name  
        )   #
        #
        tbl <- data.frame( 
            "NAME"           = tbl, 
            stringsAsFactors = FALSE
        )   #
    }   #
    #
    return( tbl ) 
### The function returns a data frame (table) where each row is 
### data about one column / field of the original sqlite table and each 
### column is a characteristics of the field  "NB", "NAME", "TYPE", 
### "NOT_NULL", "DEFAULT" and "PRIMARY". 
}   #






ML.dbListTables <- function(# Utility function. List _all_ the tables in a MACRO SQLite database.
### Utility function. List _all_ the tables in a MACRO SQLite 
### database.This function is a wrapper around R DBI 
### dbListTables function, that initiate the driver and opens a 
### connexion to the database, fetch the table list and close the 
### connexion and the driver. 
### Type ML.get("optioname"), with 'optioname' the name of the option, 
### to know its 'real' default value (when NULL).

 db.dir=NULL,  
### Single character string, containing the path of the folder where 
### is located the SQLite database named 'db.name' in which the parameters 
### will be written. If no database is found there it is created. 
### Default value ML.get("db.dir"). 
 
 db.name=NULL,  
### Single character string, name (and extension) of the SQLite database 
### where the parameters should be written. The path of this database is 
### 'db.dir'. If no database is found, it is created.
### Default value ML.get("db.name"). 

 sqlite3.exe=NULL,  
### Name of the SQLite3 command line program. 

 test.files=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 error.keywords=NULL,  
### Vector of character strings. List of the keywords that must be 
### searched in 'shell.res'. 

 pkgname=NULL,  
### Single character string. Name of the package. Used to fetch some 
### files in the package directory. 

 libname=NULL   
### Single character string. name of the library = folder in which 
### the pacakge is installef. Used to fetch some files in the package 
### directory. 

){  # Automatically set the default parameters that are NULL:
    ML.auto.set() 
    #
    if( test.files ) 
    {   # Check if the database files is present:
        ML.file.check.warn( 
            file.wd    = db.dir,
            file.names = db.name  
        )   #
    }   #
    #
    require( "DBI" ) 
    require( "RSQLite" ) 
    #
    tbl <- ML.db.connect.and.tryCatch(
        db.dir          = db.dir,  
        db.name         = db.name,  
        expr            = expression({ 
            expr.out <- dbListTables(
                conn      = db.con  
            )   #
        }),  #
        drvName         = "SQLite",
        max.con         = 1,  
        error.classes   = c("simpleError","error","condition"),  
        stop.on.error   = TRUE, 
        error.message   = "Error detected in dbListTables() in ML.dbListTables(). Database safely closed."  
        # ... options passed to expr: none
    )   #
    #
    return( tbl ) 
### The function returns the list of tables found in the database. 
}   #










ML.ReadTable <- function(# Read the content of a table (all the table) in SQLite.
### Utility function. This function is a wrapper around R DBI 
### dbReadTable function, that initiate the driver and opens a 
### connexion to the database, fetch the table and close the 
### connexion and the driver. If the the table has no data, it returns 
### a table with 0 rows and the correct number of columns, instead 
### of 0 rows and 0 columns as the default behaviour.
### Type ML.get("optioname"), with 'optioname' the name of the option, 
### to know its 'real' default value (when NULL).

 table.name,  
### Single character string. Name of the table whose fields (columns) 
### characteristics should be fetched. 

 db.dir=NULL,  
### Single character string, containing the path of the folder where 
### is located the SQLite database named 'db.name' in which the parameters 
### will be written. If no database is found there it is created. 
### Default value ML.get("db.dir"). 
 
 db.name=NULL,  
### Single character string, name (and extension) of the SQLite database 
### where the parameters should be written. The path of this database is 
### 'db.dir'. If no database is found, it is created.
### Default value ML.get("db.name"). 

 sqlite3.exe=NULL,  
### Name of the SQLite3 command line program. 

 test.files=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 error.keywords=NULL,  
### Vector of character strings. List of the keywords that must be 
### searched in 'shell.res'. 

 pkgname=NULL,  
### Single character string. Name of the package. Used to fetch some 
### files in the package directory. 

 libname=NULL   
### Single character string. name of the library = folder in which 
### the pacakge is installef. Used to fetch some files in the package 
### directory. 

){  # Automatically set the default parameters that are NULL:
    ML.auto.set() 
    #
    if( test.files ) 
    {   # Check if the database files is present:
        ML.file.check.warn( 
            file.wd    = db.dir,
            file.names = db.name  
        )   #
    }   #
    #
    require( "DBI" ) 
    require( "RSQLite" ) 
    #
    tbl <- ML.db.connect.and.tryCatch(
        db.dir          = db.dir,  
        db.name         = db.name,  
        expr            = expression({ 
            expr.out <- dbReadTable(
                conn      = db.con, 
                ... 
            )   #
        }),  #
        drvName         = "SQLite",
        max.con         = 1,  
        error.classes   = c("simpleError","error","condition"),  
        stop.on.error   = TRUE, 
        error.message   = "Error detected in dbReadTable() in ML.ReadTable(). Database safely closed.", 
        # ... options passed to expr:
        name            = table.name, 
        row.names       = ""  
    )   #
    #
    # # Retrieve the scenario table in the sqlite database:
    # sqlite.con <- dbDriver( 
    #     drvName = "SQLite", 
    #     max.con = 1  
    # )   #
    # #
    # db.con <- dbConnect( 
    #     drv    = sqlite.con, 
    #     dbname = file.path( db.dir, db.name )  
    # )   #
    # #
    # # Initiate the error catching object:
    # catch.res <- NULL 
    # #
    # catch.res <- tryCatch( 
    #     expr = { 
    #         tbl <- dbReadTable(
    #             conn      = db.con, 
    #             name      = table.name, 
    #             row.names = ""
    #         )   #
    #     },  #
    #     # What to do with an eventual error message catched (the.error)?
    #     error = function(the.error){ 
    #         the.error # just return it.
    #     }   #
    # )   #
    # #
    # dbDisconnect( conn = db.con ) 
    # #
    # dbUnloadDriver( drv = sqlite.con ) 
    # #
    # if( any( class(catch.res) %in% c("simpleError","error","condition") ) )
    # {   #
    #     warning( catch.res ) 
    #     stop( "Error detected in dbReadTable() in ML.ReadTable(). Database safely closed." )
    # }   #
    #
    if( dim(tbl)[2] == 0 ) 
    {   #
        fields.res <- ML.dbListFields( 
            table.name      = table.name,  
            db.dir          = db.dir,  
            db.name         = db.name,  
            sqlite3.exe     = sqlite3.exe,  
            test.files      = test.files,  
            error.keywords  = error.keywords,  
            pkgname         = pkgname,  
            libname         = libname   
        )   #
        #
        tbl <- as.data.frame( 
            matrix( 
                data = vector(mode = "numeric", length = 0), 
                nrow = 0, 
                ncol = dim(fields.res)[1] 
            )   #
        )   #
        #
        colnames(tbl) <- fields.res[, "NAME" ] 
    }   #
    #
    return( tbl ) 
### The function returns the requested table. 
}   #






ML.ReadTable.where <- function(# Read all or part (some rows or columns) of a table in SQLite.
### Utility function. This function is a wrapper around R DBI 
### dbGetQuery function, that initiate the driver and opens a 
### connexion to the database, fetch part of a table and close the 
### connexion and the driver. This function exectures an 'SQL SELECT 
### FROM TABLE WHERE ... = ...' query, over one or several fields. 
### If the the table has no data, it returns 
### a table with 0 rows and the correct number of columns, instead 
### of 0 rows and 0 columns as the default behaviour.
### Type ML.get("optioname"), with 'optioname' the name of the option, 
### to know its 'real' default value (when NULL).

 table.name,  
### Single character string. Name of the table where the data 
### should be fetched.

 db.dir=NULL,  
### Single character string, containing the path of the folder where 
### is located the SQLite database named 'db.name' in which the parameters 
### will be written. If no database is found there it is created. 
### Default value ML.get("db.dir"). 
 
 db.name=NULL,  
### Single character string, name (and extension) of the SQLite database 
### where the parameters should be written. The path of this database is 
### 'db.dir'. If no database is found, it is created.
### Default value ML.get("db.name"). 

 sqlite3.exe=NULL,  
### Name of the SQLite3 command line program. 

 test.files=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 error.keywords=NULL,  
### Vector of character strings. List of the keywords that must be 
### searched in 'shell.res'. 

 pkgname=NULL,  
### Single character string. Name of the package. Used to fetch some 
### files in the package directory. 

 libname=NULL,  
### Single character string. name of the library = folder in which 
### the pacakge is installef. Used to fetch some files in the package 
### directory. 

 sel.cols=vector(length=0),  
### Vector of character strings. Names of the columns that must 
### be retrived in the table 'table.name'. If a vector of length 0, 
### all the columns are retrieved.

 sel.cond.nm1=vector(length=0),  
### Single character string. Name of the table on which the first 
### WHERE constrain will be applied. If vector of length 0, no 
### constrain is applied.
 
 sel.cond.val1=vector(length=0),  
### Vector of values, from 1 to n. The format depends on the field 
### format of the table 'sel.cond.nm1'. The value are the list of 
### possible value that can be retrieved in the table 'sel.cond.nm1'. 
### If vector of length 0, no constrain is applied.

 sel.cond.nm2=vector(length=0),  
### Single character string. Name of the table on which the first 
### WHERE constrain will be applied. If vector of length 0, no 
### constrain is applied.

 sel.cond.val2=vector(length=0)   
### Vector of values, from 1 to n. The format depends on the field 
### format of the table 'sel.cond.nm2'. The value are the list of 
### possible value that can be retrieved in the table 'sel.cond.nm2'. 
### If vector of length 0, no constrain is applied.

){  # Automatically set the default parameters that are NULL:
    ML.auto.set() 
    #
    if( test.files ) 
    {   # Check if the database files is present:
        ML.file.check.warn( 
            file.wd    = db.dir,
            file.names = db.name  
        )   #
    }   #
    # 
    # Prepare the list of columns to choose in the table:
    if( length(sel.cols) != 0 )
    {   #
        select.what <- paste( sep="", "[", sel.cols, "]" ) 
        #
        select.what <- paste( sel.cols, collapse = ", " ) 
    }else{ 
        select.what <- "*"
    }   #
    # 
    # Prepare the 1st series of constrains:
    if( (length(sel.cond.nm1) != 0) & (length(sel.cond.val1) != 0) )
    {   #
        if( class(sel.cond.val1) == "character" )
        {   #
            sel.cond.val1 <- paste( 
                "\"", 
                sel.cond.val1, 
                "\"", 
                sep = "" 
            )   #
        }   #
        #
        const1 <- paste( 
            sep = "", 
            "([", sel.cond.nm1, "] = ", 
            sel.cond.val1, ")" 
        )   #
        #
        const1 <- paste( const1, collapse = " OR " )  
        #
        const1 <- paste( "(", const1, ")", sep = "" )  
    }else{ 
        const1 <- NULL 
    }   #
    #
    # Prepare the 2nd series of constrains:
    if( (length(sel.cond.nm2) != 0) & (length(sel.cond.val2) != 0) )
    {   #
        if( class(sel.cond.val2) == "character" )
        {   #
            sel.cond.val2 <- paste( 
                "\"", 
                sel.cond.val2, 
                "\"", 
                sep = "" 
            )   #
        }   #
        #
        const2 <- paste( 
            sep = "", 
            "([", sel.cond.nm2, "] = ", 
            sel.cond.val2, ")" 
        )   #
        #
        const2 <- paste( const2, collapse = " OR " )  
        #
        const2 <- paste( "(", const2, ")", sep = "" )  
    }else{ 
        const2 <- NULL 
    }   #
    #
    # Bind the 2 constrains:
    if( (!is.null(const1)) & (!is.null(const2)) ) 
    {   #
        const <- paste( const1, const2, sep = " AND " ) 
    }else{ 
        const <- c(const1, const2) 
    }   #
    #
    # Add a WHERE statement if some constrains are not null:
    if( !is.null(const) )
    {   #
        const <- paste( "WHERE", const, sep = " " ) 
    }   #
    #
    # Create the full querry statement:
    statement <- paste( 
            sep = "", 
            "SELECT ", select.what, "\n", 
            "FROM [", table.name, "]\n", 
            const, "\n", 
            ";\n" 
        )   #
    #
    # cat( statement, sep = "\n" )
    #
    require( "DBI" ) 
    require( "RSQLite" ) 
    #
    tbl <- ML.db.connect.and.tryCatch(
        db.dir          = db.dir,  
        db.name         = db.name,  
        expr            = expression({ 
            expr.out <- dbGetQuery( 
                conn      = db.con, 
                ...  
            )   #
        }),  #
        drvName         = "SQLite",
        max.con         = 1,  
        error.classes   = c("simpleError","error","condition"),  
        stop.on.error   = TRUE, 
        error.message   = "Error detected in dbGetQuery() in ML.ReadTable.where(). Database safely closed.", 
        # ... options for expr:
        statement       = statement  
    )   #
    # 
    # # Retrieve the scenario table in the sqlite database:
    # sqlite.con <- dbDriver( 
    #     drvName = "SQLite", 
    #     max.con = 1  
    # )   #
    # #
    # db.con <- dbConnect( 
    #     drv    = sqlite.con, 
    #     dbname = file.path( db.dir, db.name )  
    # )   #
    # #
    # # Initiate the error catching object:
    # catch.res <- NULL 
    # #
    # catch.res <- tryCatch( 
    #     expr = { 
    #         tbl <- dbGetQuery( 
    #             conn      = db.con, 
    #             statement = statement  
    #         )   #
    #     },  #
    #     # What to do with an eventual error message catched (the.error)?
    #     error = function(the.error){ 
    #         the.error # just return it.
    #     }   #
    # )   #
    # #
    # dbDisconnect( conn = db.con ) 
    # #
    # dbUnloadDriver( drv = sqlite.con ) 
    # #
    # if( any( class(catch.res) %in% c("simpleError","error","condition") ) )
    # {   #
    #     warning( catch.res ) 
    #     stop( "Error detected in dbGetQuery() in ML.ReadTable.where(). Database safely closed." )
    # }   #
    #
    if( dim(tbl)[2] == 0 ) 
    {   #
        fields.res <- ML.dbListFields( 
            table.name      = table.name,  
            db.dir          = db.dir,  
            db.name         = db.name,  
            sqlite3.exe     = sqlite3.exe,  
            test.files      = test.files,  
            error.keywords  = error.keywords,  
            pkgname         = pkgname,  
            libname         = libname   
        )   #
        #
        tbl <- as.data.frame( 
            matrix( 
                data = vector(mode = "numeric", length = 0), 
                nrow = 0, 
                ncol = dim(fields.res)[1] 
            )   #
        )   #
        #
        colnames(tbl) <- fields.res[, "NAME" ] 
        #
        if( length(sel.cols) != 0 ) 
        {   #
            tbl <- tbl[, sel.cols, drop = FALSE ] 
        }   #
    }   #
    #
    return( tbl ) 
### The function returns the requested table. 
}   #







ML.WriteTable <- function(# Write a table in an SQLIte database.
### Utility function. This function is a wrapper around R DBI 
### dbWriteTable function, that initiate the driver and opens a 
### connexion to the database, write data in the table and close the 
### connexion and the driver. This function also make sure that 
### the identifiers do not already exist in the MySQL database, 
### and eventually creates these identifiers. Because of this, it 
### can only be used on MACRO-SE parameter tables.
### Type ML.get("optioname"), with 'optioname' the name of the option, 
### to know its 'real' default value (when NULL).

 table.name,  
### Single character string. Name of the table where the data 
### should be written.

 table.data,  
### Data frame or matrix. Table with column names containg the 
### data to be written in the table 'table.name'.

#  id.col.nm=NA,   
# ### Single character string. Name of the column containing the 
# ### ID's. If equal to NA, then it is assumed that the ID's are 
# ### in the table 'table.data' or that there is no ID's needed. 
# ### If not equal to NA, and if the ID column is not present in 
# ### 'table.data' or contains no values, then 'best possible ID's' 
# ### are chosen and returned.

 db.dir=NULL,  
### Single character string, containing the path of the folder where 
### is located the SQLite database named 'db.name' in which the parameters 
### will be written. If no database is found there it is created. 
### Default value ML.get("db.dir"). 
 
 db.name=NULL,  
### Single character string, name (and extension) of the SQLite database 
### where the parameters should be written. The path of this database is 
### 'db.dir'. If no database is found, it is created.
### Default value ML.get("db.name"). 

 sqlite3.exe=NULL,  
### Name of the SQLite3 command line program. 

 test.files=TRUE,  
### Single logical. Should the function test for the presence 
### (file.exist()) of the needed files in the folder before trying 
### to fetch information from the database? 

 error.keywords=NULL,  
### Vector of character strings. List of the keywords that must be 
### searched in 'shell.res'. 

 pkgname=NULL,  
### Single character string. Name of the package. Used to fetch some 
### files in the package directory. 

 libname=NULL,  
### Single character string. name of the library = folder in which 
### the pacakge is installef. Used to fetch some files in the package 
### directory. 

 overwrite.tbl=FALSE,   
### Single logical. See ?dbWriteTable. Set to TRUE if the table 
### is to be overwritten

 append.op=TRUE,  
### Single logical. See ?dbWriteTable. Set to TRUE (default) when 
### the data must be added at the end of the table.

 update.val=FALSE   
### Single logical. If TRUE, the values in the table with the same 
### ID (in id.col.nm) will be updated instead of being added. If 
### FALSE the values are added or an error is returned if some 
### values have the same ID.

){  # Automatically set the default parameters that are NULL:
    ML.auto.set() 
    #
    if( test.files ) 
    {   # Check if the database files is present:
        ML.file.check.warn( 
            file.wd    = db.dir, 
            file.names = db.name  
        )   #
    }   #
    #
    require( "DBI" ) 
    require( "RSQLite" ) 
    #
    #    * Get the list of ID columns
    id.col.nm <- ML.ReadTable.where( 
        table.name      = "MACROSYS_tables_list",  
        db.dir          = db.dir,  
        db.name         = db.name,  
        sel.cols        = c("TBL_NAME","ID_COL"),  
        sqlite3.exe     = sqlite3.exe,  
        test.files      = test.files,  
        error.keywords  = error.keywords,  
        pkgname         = pkgname,  
        libname         = libname,  
        sel.cond.nm1    = "TBL_NAME",  # Select only the RUNIDs 
        sel.cond.val1   = table.name   # asked
    )   #
    # 
    id.col.test <- nrow( id.col.nm ) == 0 
    # 
    if( !id.col.test ) 
    {   #
        #   * Make it a vector (instead of single string)
        id.col.nm <- strsplit( 
            x       = id.col.nm[, "ID_COL" ], 
            split   = ",", 
            fixed   = TRUE, 
            perl    = FALSE
        )[[1]]
        #
        #   * Take the last one
        id.col.nm <- id.col.nm[ 1 ] 
        #
        #   * Get the IDs already in the database / table:
        ID.in <- ML.ReadTable.where( 
            table.name      = table.name,  
            db.dir          = db.dir,  
            db.name         = db.name,  
            sqlite3.exe     = sqlite3.exe,  
            test.files      = FALSE,  #  already done
            error.keywords  = error.keywords,  
            pkgname         = pkgname,  
            libname         = libname,  
            sel.cols        = id.col.nm   
        )[,]#
        #
        #   * If no ID in the table (0 record), then set ID.in to 0
        if( length(ID.in) == 0 ){ ID.in <- 0 }
        #
        #   * If that ID is not in the table, or has NA values, 
        #     then new NAs need to be attributed:
        #     a. Is the ID already in columns names?
        col.test    <- id.col.nm %in% colnames(table.data) 
        #
        if( col.test )
        {   # b. If yes, is there NA values in these IDs 
            if( any( is.na( table.data[,id.col.nm] ) ) )
            {   #
                # Attribute new ID later
                new.id <- TRUE 
            }else{ 
                # Just test that the IDs are not already in the table
                test.id <- table.data[,id.col.nm] %in% ID.in
                #
                if( any( test.id ) & (!update.val) ) # Stop if the ID are already used (no update case) 
                {   #
                    stop( 
                        paste( 
                            sep = "", 
                            "Some of the ID in 'table.data' (", 
                            id.col.nm, " = ", 
                            paste( table.data[test.id,id.col.nm], collapse = ", " ), 
                            ") are already in the table, ", table.name, 
                            " while 'update.val' = FALSE. Please use other IDs." 
                        )   #
                    )   #
                }   #
                #
                if( (!all( test.id )) & (update.val) ) # Stop if the ID are not already used (update case)
                {   #
                    stop( 
                        paste( 
                            sep = "", 
                            "Some of the ID in 'table.data' (", 
                            id.col.nm, " = ", 
                            paste( table.data[test.id,id.col.nm], collapse = ", " ), 
                            ") are NOT in the table, ", table.name, 
                            " while 'update.val' = TRUE. Please use existing IDs." 
                        )   #
                    )   #
                }   #
                #
                new.id <- FALSE 
            }   #
        }else{ 
            # Attribute new ID later
            new.id <- TRUE 
        }   #
        #
    }else{ 
        new.id <- FALSE 
        #
        if( update.val ) 
        {   #
            stop( 
                paste( 
                    sep = "", 
                    "ML.WriteTable() do not accept 'update.val' ", 
                    "for tables not referenced in [MACROSYS_tables_list]", 
                    "('table.name': ", table.name, ")."
                )   #
            )   #
        }   #
    }   #
    #
    if( new.id ) 
    {   #   * Number of new IDs that will be written
        n <- dim( table.data )[1] 
        #
        #   * Define the new IDs (ID.out)
        ID.out  <- (1:(max(ID.in)+n)) 
        ID.out  <- ID.out[ !(ID.out %in% ID.in) ] 
        ID.out  <- as.integer( ID.out[1:n] ) 
        #
        #   * Either write these new ID in an existing column 
        #     or create a new column for them:
        if( col.test ) 
        {   #
            table.data[,id.col.nm] <- ID.out 
        }else{ 
            table.data <- data.frame( 
                "XXXXXX" = ID.out, 
                table.data  
            )   #
            #
            colnames( table.data )[ 1 ] <- id.col.nm 
        }   #
        #
        # Extract expected columns names from the database:
        expect.cols <- ML.dbListFields(
            table.name      = table.name,  
            db.dir          = db.dir,  
            db.name         = db.name,  
            sqlite3.exe     = sqlite3.exe,  
            test.files      = test.files,  
            error.keywords  = error.keywords,  
            pkgname         = pkgname,  
            libname         = libname   
        )[,"NAME"] 
        #
        # Check that expected and actual columns names do corresponds:
        test.col <- expect.cols %in% colnames( table.data )
        #
        if( any( !test.col ) ) 
        {   #
            stop( 
                paste( 
                    sep = "", 
                    "Some columns of [", table.name, "] where not", 
                    " found in the data provided in 'table.data'\n",
                    "(missing columns: ", paste( 
                        expect.cols[!test.col], 
                        collapse = ", "
                    ),  ")"  
                )   #
            )   #
        }   #
        #
        # Put the columns in the right order:
        table.data <- table.data[, expect.cols ] 
        #
        # if(){
        # test2 <- id.col.nm %in% colnames(table.data) 
        # #
        # if( test2 ) 
        # {   #
        #     test2 <- any( is.na( table.data[,id.col.nm] ) ) 
        #     #
        #     if( test2 ) 
        #     {   #
        #         table.data[,id.col.nm] <- ID.out 
        #     }   #
        # }else{ 
        #     table.data <- data.frame( 
        #         "XXXXXX" = ID.out, 
        #         table.data  
        #     )   #
        #     #
        #     colnames( table.data )[ 1 ] <- id.col.nm 
        #     #
        #     test2 <- TRUE 
        # }   #
        # }else{ 
        #     test2 <- FALSE 
        # }   #
    }   #
    #
    # Dont remember what is the use of this code
    # but now there are less than 100 rows...
    # if( table.name == "OUTPUTL_outputs_layered" )
    # {   #
    #     # table.data <- table.data[1:100,] 
    # }   #
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
    # if( update.val & any(is.na(id.col.nm)) )
    # {   #
    #     stop( "In ML.WriteTable, if update == TRUE, the condition !is.na(id.col.nm) must be TRUE (= provide an ID column)" )  
    # }   #
    #
    if( !update.val ) 
    {   ### Case 1: no update needed:
        # print( table.data ) 
        #
        # if( table.name %in% c("HORIZ_physical","HORIZ_properties") ) 
        # {   #
        #     print( table.data ) 
        #     #
        #     print( sapply( 1:ncol(table.data), function(X){ class( table.data[,X] ) } ) ) 
        # }   #
        #
        ML.db.connect.and.tryCatch(
            db.dir          = db.dir,  
            db.name         = db.name,  
            expr            = expression({ 
                dbWriteTable( 
                    conn        = db.con, 
                    ... 
                )   #
            }),  #
            drvName         = "SQLite",
            max.con         = 1,  
            error.classes   = c("simpleError","error","condition"),  
            stop.on.error   = TRUE, 
            error.message   = paste( 
                "Error detected in dbWriteTable() in ML.WriteTable() [not update], table ", 
                table.name, 
                ". Can not write the data. Database safely closed.", 
                sep = ""  
            ),  #
            # ... options for expr:
            name            = table.name, 
            value           = table.data, 
            row.names       = FALSE, 
            overwrite       = overwrite.tbl, 
            append          = append.op  
        )   #
        #
        # sqlite.con <- dbDriver( 
        #     drvName = "SQLite", 
        #     max.con = 1  
        # )   #
        # #
        # db.con <- dbConnect( 
        #     drv    = sqlite.con, 
        #     dbname = file.path( db.dir, db.name )  
        # )   #
        # #
        # # Initiate the error catching object:
        # catch.res <- NULL 
        # #
        # catch.res <- tryCatch( 
        #     expr = { 
        #         dbWriteTable( 
        #             conn        = db.con, 
        #             name        = table.name, 
        #             value       = table.data, 
        #             row.names   = FALSE, 
        #             overwrite   = overwrite.tbl, 
        #             append      = append.op  
        #         )   #
        #     },  #
        #     # What to do with an eventual error message catched (the.error)?
        #     error = function(the.error){ 
        #         the.error # just return it.
        #     }   #
        # )   #
        # #
        # dbDisconnect( conn = db.con ) 
        # #
        # dbUnloadDriver( drv = sqlite.con ) 
        # #
        # if( any( class(catch.res) %in% c("simpleError","error","condition") ) )
        # {   #
        #     warning( catch.res ) 
        #     stop( "Error detected in dbWriteTable() in ML.WriteTable() [not update]. Can not retrieve old data. Database safely closed." )
        # }   #
    }else{ ### Case Update needed.
        table.data.nm <- colnames( table.data ) 
        # table.data.nm <- table.data.nm[ table.data.nm != id.col.nm ] 
        #
        table.data.cl <- unlist( 
            lapply( 
                X   = 1:ncol(table.data), 
                FUN = function(X){ 
                    is.character( table.data[,X] ) | is.factor( table.data[,X] ) 
                }   #
            )   #
        )   #
        #
        # Wrap the character data into "" for the SQL statement
        if( any(table.data.cl) )
        {   #
            table.data[,table.data.cl] <- do.call( 
                what = "cbind", 
                args = lapply( 
                    X   = (1:ncol(table.data))[ table.data.cl ], 
                    FUN = function(X){ 
                        tmp <- paste( "\"", table.data[,X], "\"", sep = "" ) 
                        #
                        tmp[ tmp == "NA" ] <- "" 
                        #
                        tmp 
                    }   #
                )   #
            )   #
        }   #
        #
        if( any(!table.data.cl) )
        {   #
            table.data[,!table.data.cl] <- do.call( 
                what = "cbind", 
                args = lapply( 
                    X   = (1:ncol(table.data))[ !table.data.cl ], 
                    FUN = function(X){ 
                        tmp <- table.data[,X]
                        #
                        tmp[ is.na(tmp) ] <- "\"\"" 
                        #
                        tmp 
                    }   #
                )   #
            )   #
        }   #
        #
        silent <- lapply( 
            X   = 1:nrow( table.data ), 
            FUN = function(X){ 
                # Create the SQL update statement
                sql.update <- paste( 
                    sep = "", 
                    "UPDATE [", table.name, "]\n", 
                    "SET ", paste( 
                        "[", table.data.nm, "]", 
                        " = ", 
                        table.data[X,], 
                        collapse = ", ", # Not AND 
                        sep      = ""
                    ),  "\n", 
                    "WHERE [", id.col.nm, "] = ", table.data[X,id.col.nm], ";\n\n"  
                )   #
                #
                # cat( sql.update ) 
                #
                ML.db.connect.and.tryCatch(
                    db.dir          = db.dir,  
                    db.name         = db.name,  
                    expr            = expression({ 
                        dbGetQuery( # instead of dbSendQuery
                            conn        = db.con, 
                            ... 
                        )   #
                        #
                        
                    }),  #
                    drvName         = "SQLite",
                    max.con         = 1,  
                    error.classes   = c("simpleError","error","condition"),  
                    stop.on.error   = TRUE, 
                    error.message   = "Error detected in dbWriteTable() in ML.WriteTable(). Can not update the data. Database safely closed.", 
                    # ... options for expr:
                    statement       = sql.update  
                )   #
            }   #
        )   #
        #
        # # -- 4 steps update proceedure. As we are not sure there 
        # #    are the same number of rows to be updated than 
        # #    there was before, we remove and update instead, 
        # #    and eventually re-write the old data if the 
        # #    update failed.
        # # 
        # # -- Step 1: read the data to be deleted:
        # old.data <- ML.ReadTable.where( 
        #     table.name      = table.name,  
        #     db.dir          = db.dir,  
        #     db.name         = db.name,  
        #     sqlite3.exe     = sqlite3.exe,  
        #     test.files      = FALSE,  #  already done
        #     error.keywords  = error.keywords,  
        #     pkgname         = pkgname,  
        #     sel.cond.nm1    = id.col.nm,  
        #     sel.cond.val1   = table.data[,id.col.nm]  
        # )   #
        # #
        # # -- Step 2: Delete the data to be deleted
        # #
        # #    format an SQL DELETE Query:
        # delete.query <- paste( 
        #     sep = " ", 
        #     "DELETE FROM", table.name, "\n", 
        #     "WHERE", paste( 
        #         paste( 
        #             id.col.nm, 
        #             "= ", 
        #             table.data[,id.col.nm], 
        #             "" 
        #         ),  #
        #         collapse = " OR " 
        #      ), #
        #      ";" 
        # )   #
        # #
        # ML.db.connect.and.tryCatch(
        #     db.dir          = db.dir,  
        #     db.name         = db.name,  
        #     expr            = expression({ 
        #         dbGetQuery( 
        #             conn        = db.con, 
        #             ... 
        #         )   #
        #     }),  #
        #     drvName         = "SQLite",
        #     max.con         = 1,  
        #     error.classes   = c("simpleError","error","condition"),  
        #     stop.on.error   = TRUE, 
        #     error.message   = "Error detected in dbWriteTable() in ML.WriteTable() [update step 2]. Can not delete old data. Database safely closed.", 
        #     # ... options for expr:
        #     statement       = delete.query  
        # )   #
        # #
        # # sqlite.con <- dbDriver( 
        # #     drvName = "SQLite", 
        # #     max.con = 1  
        # # )   #
        # # #
        # # db.con <- dbConnect( 
        # #     drv    = sqlite.con, 
        # #     dbname = file.path( db.dir, db.name )  
        # # )   #
        # # #
        # # #    Initiate the error catching object:
        # # catch.res <- NULL 
        # # #
        # # catch.res <- tryCatch( 
        # #     expr = { 
        # #         dbGetQuery( 
        # #             conn        = db.con, 
        # #             statement   = delete.query  
        # #         )   #
        # #     },  #
        # #     # What to do with an eventual error message catched (the.error)?
        # #     error = function(the.error){ 
        # #         the.error # just return it.
        # #     }   #
        # # )   #
        # # #
        # # dbDisconnect( conn = db.con ) 
        # # #
        # # dbUnloadDriver( drv = sqlite.con ) 
        # # #
        # # if( any( class(catch.res) %in% c("simpleError","error","condition") ) )
        # # {   #
        # #     warning( catch.res ) 
        # #     stop( "Error detected in dbWriteTable() in ML.WriteTable() [update step 2]. Can not delete old data. Database safely closed." )
        # # }   #
        # #
        # # -- Step 3: Write the new data
        # #
        # err.obj <- ML.db.connect.and.tryCatch(
        #     db.dir          = db.dir,  
        #     db.name         = db.name,  
        #     expr            = expression({ 
        #         dbWriteTable( 
        #             conn        = db.con, 
        #             ...  
        #         )   #
        #     }),  #
        #     drvName         = "SQLite",
        #     max.con         = 1,  
        #     error.classes   = c("simpleError","error","condition"),  
        #     stop.on.error   = FALSE,  #  <<-- notice that!
        #     error.message   = "Error detected in dbWriteTable() in ML.WriteTable() [update step 3]. Updated data not written. Database safely closed.", 
        #     # ... options for expr:
        #     name            = table.name, 
        #     value           = table.data, 
        #     row.names       = FALSE, 
        #     overwrite       = overwrite.tbl, 
        #     append          = append.op  
        # )   #
        # #
        # # sqlite.con <- dbDriver( 
        # #     drvName = "SQLite", 
        # #     max.con = 1  
        # # )   #
        # # #
        # # db.con <- dbConnect( 
        # #     drv    = sqlite.con, 
        # #     dbname = file.path( db.dir, db.name )  
        # # )   #
        # # #
        # # #    Initiate the error catching object:
        # # catch.res <- NULL 
        # # #
        # # catch.res <- tryCatch( 
        # #     expr = { 
        # #         dbWriteTable( 
        # #             conn        = db.con, 
        # #             name        = table.name, 
        # #             value       = table.data, 
        # #             row.names   = FALSE, 
        # #             overwrite   = overwrite.tbl, 
        # #             append      = append.op  
        # #         )   #
        # #     },  #
        # #     # What to do with an eventual error message catched (the.error)?
        # #     error = function(the.error){ 
        # #         the.error # just return it.
        # #     }   #
        # # )   #
        # # #
        # # dbDisconnect( conn = db.con ) 
        # # #
        # # dbUnloadDriver( drv = sqlite.con ) 
        # #
        # # -- Step 4: If the new data writing failed, and if the 
        # #    old data had some rows, try to write again the old 
        # #    data!
        # #
        # if( (!is.null( err.obj )) & (dim(old.data)[1] != 0) ) 
        # {   #
        #     ML.db.connect.and.tryCatch(
        #         db.dir          = db.dir,  
        #         db.name         = db.name,  
        #         expr            = expression({ 
        #             dbWriteTable( 
        #                 conn        = db.con, 
        #                 ... 
        #             )   #
        #         }),  #
        #         drvName         = "SQLite",
        #         max.con         = 1,  
        #         error.classes   = c("simpleError","error","condition"),  
        #         stop.on.error   = TRUE,  
        #         error.message   = "Error detected in dbWriteTable() in ML.WriteTable() [update step 4]. Updated data not written + failed to re-write the old data. Database safely closed.", 
        #         # ... options for expr:
        #         name            = table.name, 
        #         value           = old.data, 
        #         row.names       = FALSE, 
        #         overwrite       = overwrite.tbl, 
        #         append          = append.op  
        #     )   #
        #     #
        #     # sqlite.con <- dbDriver( 
        #     #     drvName = "SQLite", 
        #     #     max.con = 1  
        #     # )   #
        #     # #
        #     # db.con <- dbConnect( 
        #     #     drv    = sqlite.con, 
        #     #     dbname = file.path( db.dir, db.name )  
        #     # )   #
        #     # #
        #     # #    Initiate the error catching object:
        #     # catch.res2 <- NULL 
        #     # #
        #     # catch.res2 <- tryCatch( 
        #     #     expr = { 
        #     #         dbWriteTable( 
        #     #             conn        = db.con, 
        #     #             name        = table.name, 
        #     #             value       = old.data, 
        #     #             row.names   = FALSE, 
        #     #             overwrite   = overwrite.tbl, 
        #     #             append      = append.op  
        #     #         )   #
        #     #     },  #
        #     #     # What to do with an eventual error message catched (the.error)?
        #     #     error = function(the.error){ 
        #     #         the.error # just return it.
        #     #     }   #
        #     # )   #
        #     # #
        #     # dbDisconnect( conn = db.con ) 
        #     # #
        #     # dbUnloadDriver( drv = sqlite.con ) 
        #     # #
        #     # if( any( class(catch.res2) %in% c("simpleError","error","condition") ) )
        #     # {   #
        #     #     warning( catch.res2 ) 
        #     #     stop( "Error detected in dbWriteTable() in ML.WriteTable() [update step 4]. Update failed + failed to re-write the old data. Database safely closed." )
        #     # }   #
        #     # #
        #     # # Process the previous error caught if the old data were successfully re-written.
        #     # warning( catch.res ) 
        #     # stop( "Error detected in dbWriteTable() in ML.WriteTable() [update step 3]. Old data not deleted. Database safely closed." )
        # }   #
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
            stop( "Warning(s) detected in SQLite transaction. type warnings() to see it\them." ) 
        }   #
    }   #
    #
    if( last.warning.exist ) 
    {   #
        assign( 
            x     = "last.warning", 
            value = old.warn, 
            envir = baseenv() 
        )   #
    }   #
    #
    # if( exists( "last.warning", envir = baseenv() ) ) 
    # {   #
    #     if( length(old.warn) < length(last.warning) )
    #     {   #
    #         stop( "Warning(s) detected in SQLite transaction. type warnings() to see it\them." ) 
    #     }   #
    # }   #
    #
    if( new.id ) 
    {   #
        ID.list <- list( table.data[,id.col.nm] ) 
        #
        names( ID.list ) <- id.col.nm 
        #
        return( ID.list ) 
    }else{ 
        return( list() ) 
    }   #
### If id.col.nm is not NA, the function returns a list containing
### a vector of ID values, and named after 'id.col.nm'. 
### If an error message is detected the function stops.
}   #






MJ.ReadTable.where <- function(# Read all or part (some rows or columns) of a table in MySQL.
### Utility function. This function is a wrapper around R RODBC 
### sqlQuery function, that initiate the driver and opens a 
### connexion to the database, fetch part of a table and close the 
### connexion and the driver. This function exectures an 'SQL SELECT 
### FROM TABLE WHERE ... = ...' query, over one or several fields. 
### If the the table has no data, it returns 
### a table with 0 rows and the correct number of columns, instead 
### of 0 rows and 0 columns as the default behaviour.
### Type ML.get("optioname"), with 'optioname' the name of the option, 
### to know its 'real' default value (when NULL).

 table.name,  
### Single character string. Name of the table where the data 
### should be fetched.

 mysqlOdbcDSN=NULL,  
### Single character string. Name and description of the MySQL ODBC 
### data source that will be set. Choose any name you want. This 
### is the name / label under which the MySQL database will be 
### found under ODBC, but is does not need to be the same name as 
### the MySQL database in the remote server.

 mysqlInfo=NULL,  
### List of values. A list containing 5 named elements: "mysqlUser"
### [1 character string] = name of the database user, "mysqlPwd" 
### [1 character string] = password for this user, "mysqlHost" 
### [1 character string] = the IP address of the remote database 
### server, "mysqlDb" [1 character string] = the name of the 
### database, "mysqlPort" [1 integer] = the connexion port. 

 error.classes=c("simpleError","error","condition"),  
### Vector of character strings. Error data classes to be found in 
### tryCatch result.

 stop.on.error=TRUE, 
### Single logical. If TRUE and an error is detected, the function stops 
### AFTER closing the database and driver. If FALSE it just returns 
### the error as an object.

 case="nochange",  
### Single character string. See ?odbcDriverConnect in RODBC.

#  db.dir=NULL,  
# ### Single character string, containing the path of the folder where 
# ### is located the SQLite database named 'db.name' in which the parameters 
# ### will be written. If no database is found there it is created. 
# ### Default value ML.get("db.dir"). 
#  
#  db.name=NULL,  
# ### Single character string, name (and extension) of the SQLite database 
# ### where the parameters should be written. The path of this database is 
# ### 'db.dir'. If no database is found, it is created.
# ### Default value ML.get("db.name"). 

#  sqlite3.exe=NULL,  
# ### Name of the SQLite3 command line program. 

#  test.files=TRUE,  
# ### Single logical. Should the function test for the presence 
# ### (file.exist()) of the needed files in the folder before trying 
# ### to fetch information from the database? 

#  error.keywords=NULL,  
# ### Vector of character strings. List of the keywords that must be 
# ### searched in 'shell.res'. 

#  pkgname=NULL,  
# ### Single character string. Name of the package. Used to fetch some 
# ### files in the package directory. 

#  libname=NULL,  
# ### Single character string. name of the library = folder in which 
# ### the pacakge is installef. Used to fetch some files in the package 
# ### directory. 

 sel.cols=vector(length=0),  
### Vector of character strings. Names of the columns that must 
### be retrived in the table 'table.name'. If a vector of length 0, 
### all the columns are retrieved.

 sel.cond.nm1=vector(length=0),  
### Single character string. Name of the table on which the first 
### WHERE constrain will be applied. If vector of length 0, no 
### constrain is applied.
 
 sel.cond.val1=vector(length=0),  
### Vector of values, from 1 to n. The format depends on the field 
### format of the table 'sel.cond.nm1'. The value are the list of 
### possible value that can be retrieved in the table 'sel.cond.nm1'. 
### If vector of length 0, no constrain is applied.

 sel.cond.nm2=vector(length=0),  
### Single character string. Name of the table on which the first 
### WHERE constrain will be applied. If vector of length 0, no 
### constrain is applied.

 sel.cond.val2=vector(length=0)   
### Vector of values, from 1 to n. The format depends on the field 
### format of the table 'sel.cond.nm2'. The value are the list of 
### possible value that can be retrieved in the table 'sel.cond.nm2'. 
### If vector of length 0, no constrain is applied.

){  # Automatically set the default parameters that are NULL:
    ML.auto.set() 
    # 
    # Prepare the list of columns to choose in the table:
    if( length(sel.cols) != 0 )
    {   #
        select.what <- paste( sep="", "`", sel.cols, "`" ) 
        #
        select.what <- paste( sel.cols, collapse = ", " ) 
    }else{ 
        select.what <- "*"
    }   #
    # 
    # Prepare the 1st series of constrains:
    if( (length(sel.cond.nm1) != 0) & (length(sel.cond.val1) != 0) )
    {   #
        if( class(sel.cond.val1) == "character" )
        {   #
            sel.cond.val1 <- paste( 
                "\"", 
                sel.cond.val1, 
                "\"", 
                sep = "" 
            )   #
        }   #
        #
        const1 <- paste( 
            sep = "", 
            "(`", sel.cond.nm1, "` = ", 
            sel.cond.val1, ")" 
        )   #
        #
        const1 <- paste( const1, collapse = " OR " )  
        #
        const1 <- paste( "(", const1, ")", sep = "" )  
    }else{ 
        const1 <- NULL 
    }   #
    #
    # Prepare the 2nd series of constrains:
    if( (length(sel.cond.nm2) != 0) & (length(sel.cond.val2) != 0) )
    {   #
        if( class(sel.cond.val2) == "character" )
        {   #
            sel.cond.val2 <- paste( 
                "\"", 
                sel.cond.val2, 
                "\"", 
                sep = "" 
            )   #
        }   #
        #
        const2 <- paste( 
            sep = "", 
            "(`", sel.cond.nm2, "` = ", 
            sel.cond.val2, ")" 
        )   #
        #
        const2 <- paste( const2, collapse = " OR " )  
        #
        const2 <- paste( "(", const2, ")", sep = "" )  
    }else{ 
        const2 <- NULL 
    }   #
    #
    # Bind the 2 constrains:
    if( (!is.null(const1)) & (!is.null(const2)) ) 
    {   #
        const <- paste( const1, const2, sep = " AND " ) 
    }else{ 
        const <- c(const1, const2) 
    }   #
    #
    # Add a WHERE statement if some constrains are not null:
    if( !is.null(const) )
    {   #
        const <- paste( "WHERE", const, sep = " " ) 
    }   #
    #
    # Create the full querry statement:
    statement <- paste( 
        sep = "", 
        "SELECT ", select.what, " ", #"\n", 
        "FROM `", table.name, "` ", #"\n", 
        const, #"\n", 
        ";" 
    )   #
    #
    # cat( statement )
    #
    # cat( statement, sep = "\n" )
    #
    tbl <- MJ.db.connect.and.tryCatch( 
        mysqlOdbcDSN    = mysqlOdbcDSN,  
        mysqlInfo       = mysqlInfo,  
        expr            = expression({ 
            expr.out <- sqlQuery( 
                channel = db.con, 
                ...  
            )   #
        }),  #
        error.classes   = error.classes,  
        stop.on.error   = stop.on.error, 
        error.message   = paste( 
            sep = "", 
            "Error detected in sqlQuery() in MJ.ReadTable.where(). Not possible to read table [", 
            table.name, 
            "]. Database safely closed."  
        ),  #
        case            = case, 
        query           = statement  
    )   #
    #
    # if( dim(tbl)[2] == 0 ) 
    # {   #
    #     fields.res <- ML.dbListFields( 
    #         table.name      = table.name,  
    #         db.dir          = db.dir,  
    #         db.name         = db.name,  
    #         sqlite3.exe     = sqlite3.exe,  
    #         test.files      = test.files,  
    #         error.keywords  = error.keywords,  
    #         pkgname         = pkgname,  
    #         libname         = libname   
    #     )   #
    #     #
    #     tbl <- as.data.frame( 
    #         matrix( 
    #             data = vector(mode = "numeric", length = 0), 
    #             nrow = 0, 
    #             ncol = dim(fields.res)[1] 
    #         )   #
    #     )   #
    #     #
    #     colnames(tbl) <- fields.res[, "NAME" ] 
    #     #
    #     if( length(sel.cols) != 0 ) 
    #     {   #
    #         tbl <- tbl[, sel.cols, drop = FALSE ] 
    #     }   #
    # }   #
    #
    return( tbl ) 
### The function returns the requested table. 
}   #






MJ.db.connect.and.tryCatch <- function(# Open the driver, connect to the database and send an expression with tryCatch (MySQL version)
### Internal function. MySQL version. Open a MACRO SQLite database 
### driver, connect to a database, send an expression to be ran 
### on the database, but wrap it in tryCatch to catch eventual 
### errors, and eventually stop() with a proper error message, 
### after safely closing the database and driver (clean error). 

 mysqlOdbcDSN=NULL,  
### Single character string. Name and description of the MySQL ODBC 
### data source that will be set. Choose any name you want. This 
### is the name / label under which the MySQL database will be 
### found under ODBC, but is does not need to be the same name as 
### the MySQL database in the remote server.

 mysqlInfo=NULL,  
### List of values. A list containing 5 named elements: "mysqlUser"
### [1 character string] = name of the database user, "mysqlPwd" 
### [1 character string] = password for this user, "mysqlHost" 
### [1 character string] = the IP address of the remote database 
### server, "mysqlDb" [1 character string] = the name of the 
### database, "mysqlPort" [1 integer] = the connexion port. 

 expr,
### Single expression, eventually between {}. Expression that will 
### be passed to tryCatch. Additional parameters can be passed to 
### ..., and the database connexion object must be named 'db.con'. 
### If an output is returned, it must be saved in an object called 
### 'expr.out' that is then returned by ML.db.connect.and.tryCatch() 

 error.classes=c("simpleError","error","condition"),  
### Vector of character strings. Error data classes to be found in 
### tryCatch result.

 stop.on.error=TRUE, 
### Single logical. If TRUE and an error is detected, the function stops 
### AFTER closing the database and driver. If FALSE it just returns 
### the error as an object.

 error.message="An error was detected by tryCatch", 
### Error message to be send if an error is detected. Either as 
### stop message if stop.on.error == TRUE or as a warning 
### stop.on.error == FALSE.

 case="nochange",
### Single character string. See ?odbcDriverConnect in RODBC.

 ...
### Eventual additional parameters required by the expression.

){  # Automatically set NULL parameters to their default value:
    ML.auto.set() 
    #
    # Empty output:
    expr.out <- NULL 
    #
    db.con <- odbcConnect( 
        dsn  = mysqlOdbcDSN, 
        uid  = mysqlInfo[["mysqlUser"]], 
        pwd  = mysqlInfo[["mysqlPwd"]], 
        case = case  
    )   #
    #
    if( any( db.con == -1 ) ){ stop( paste( "Connexion to ODBC source", mysqlOdbcDSN, "failed." ) ) } 
    #
    # mysqlTblList <- sqlTables( channel = mysqlCon ) 
    # odbcClose( channel = mysqlCon ) 
    #
    # Initiate the error catching object:
    catch.res <- NULL 
    #
    catch.res <- tryCatch( 
        expr = eval( expr ),  #
        # What to do with an eventual error message catched (the.error)?
        error = function(the.error){ 
            the.error # just return it.
        },  #
        ... 
    )   #
    #
    # ex.res <- dbGetException( db.con ) 
    #
    odbcClose( channel = db.con ) 
    #
    if( any( class(catch.res) %in% error.classes ) )
    {   #
        warning( catch.res ) 
        #
        if( stop.on.error )
        {   #
            stop( error.message ) 
        }else{ 
            warning( error.message ) 
        }   #
    }   #
    #
    return( expr.out ) 
### The function returns the object 'expr.out' eventually outputed 
### by expr, and NULL otherwise.
}   #



### Write MySQL table
# res <- MJ.db.connect.and.tryCatch( 
#     mysqlOdbcDSN    = mysqlOdbcDSN,  
#     mysqlInfo       = mysqlInfo,  
#     expr = expression({ 
#         expr.out <- sqlSave(
#             channel   = db.con, 
#             append    = TRUE,
#             rownames  = FALSE, 
#             colnames  = FALSE, 
#             fast      = TRUE, 
#             ... # <- here comes the table name and content
#         )   #
#     }), #
#     error.classes   = error.classes,  
#     stop.on.error   = stop.on.error, 
#     error.message   = 
#         paste( 
#             sep = "", 
#             "Error detected in sqlSave() in MACROJUMBO.send2server(). Could not write table [", 
#             sqliteParTbl[i,"sqliteParTbl"], 
#             "]. Database safely closed."   
#         ),  #
#     case            = case, 
#     # ... options passed to expr:
#     dat             = sqliteTbl, 
#     tablename       = sqliteParTbl[i,"sqliteParTbl"]  
# )   #


