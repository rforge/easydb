\name{edbWrite.RODBC_Access}
\alias{edbWrite.RODBC_Access}
\title{Write data in a MS Access table in a database (referenced by 'edb').}
\description{Write data in a table in a MS Access database (referenced by 'edb'). }
\usage{\method{edbWrite}{RODBC_Access}(edb, tableName, data, mode = c("a", 
    "u", "o")[1], pKey = NULL, getKey = NULL, formatCol = NULL, 
    posixFormat = "", dateFormat = "", logOp = FALSE, 
    logRandId = rnorm(1), logMsg = as.character(NA), 
    logTableName = "edbLog", logCreateTableIfNotExist = TRUE, 
    parano = TRUE, testFiles = TRUE, verbose = FALSE, 
    ...)}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{data}{data.frame. Data to be writen in \code{tableName}. If the table 
has a PRIMARY KEY, and if it is AUTOINCREMENT, then the column 
can be omitted, and the attributed ID's will be retrieved if 
\code{!is.null(getKey)} (not the default). If \code{sRow} is not 
NULL, then data must contain the column names given in \code{sRow}.}
  \item{mode}{Single character string. If \code{"a"} (default), the data are 
appened to the table (added after the last row), and \code{sRow} 
is ignored. If \code{"u"}, the data are updated according to some 
critearia in \code{sRow} (that can't be NULL). If \code{"o"}, 
the table is overwritten and \code{sRow} is ignored. }
  \item{pKey}{Single character string (if mode == "u") or NULL. Column name that 
is PRIMARY KEY in the table.}
  \item{getKey}{Single character string or NULL. If non NULL, name of the PRIMARY 
KEY whose latest attributed values should be retrieved.}
  \item{formatCol}{If not NULL, a named list of functions to be applied to certain columns 
before the data are written to the database. The name of each list 
item gives the column to process, and the value of each item gives the 
function that must be applied. For instance 
formatCol = list("DATE"=as.Date) will apply the function 
\link[base]{as.Date} to the column "DATE".}
  \item{posixFormat}{Single character string. 'format' argument of the functions 
format.POSIXlt() or format.POSIXct() used to convert POSIX 
date-time into character strings when writing into the database.
Only used if getKey is not NULL.}
  \item{dateFormat}{Single character string. 'format' argument of the functions 
format.Date() used to convert "Date" 
dates into character strings when writing into the database.
Only used if getKey is not NULL.}
  \item{logOp}{Single logical. If TRUE, then a log of the operation is written 
into the database, using the function \code{\link[easydb]{edbLog}}. 
See the arguments below and \code{\link[easydb]{edbLog}} for more details.}
  \item{logRandId}{Single numerical. See \code{\link[easydb]{edbLog}}.}
  \item{logMsg}{Single character string. See \code{\link[easydb]{edbLog}}.}
  \item{logTableName}{Single character string. See \code{\link[easydb]{edbLog}}.}
  \item{logCreateTableIfNotExist}{Single logical. See \code{\link[easydb]{edbLog}}.}
  \item{parano}{Single logical. If set to TRUE (the default), the function is 
run on "paranoia mode", that is additional tests are performed 
before the data are written into the database. This slows down 
a bit (more) the function, but it may avoid some mistakes.}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbWrite")}}
}

\value{If id.col.nm is not NA, the function returns a list containing
a vector of ID values, and named after 'id.col.nm'. 
If an error message is detected the function stops.}

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link[easydb]{edb}}, \code{\link{edbRead.RODBC_Access}}, 
\code{\link{edbNames.RODBC_Access}}, 
\code{\link{edbColnames.RODBC_Access}}.}
\examples{library( "easyrodbcaccess" ) 



# System check. Ingnore this or set 'testODBC' to TRUE 
testODBC <- (class(try(odbcDataSources())) != "try-error") & # Check (R)ODBC 
            (Sys.info()[[ "sysname" ]] == "Windows") &       # Only MS Windows
            (.Machine$"sizeof.pointer" == 4)                 # Only 32-bits 



### Windows only:
if( testODBC ){ 
    ### Make a copy of MS Access example database:
    #   (a database of soil profile description) 
    file.copy( 
        from = system.file( "soils.mdb", package = "easyrodbcaccess" ), 
        to   = "soils.mdb" 
    )   
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb <- edb( dbType = "RODBC_Access", dbName = "soils.mdb" ) 
    
    
    
    ### Use the database:

    ## Write data in a table in the database
    
    # First retrieve the table profile
    profileTbl <- myDb[ "PROFILE" ] 
    
    # Change the ID's (pseudo new profiles)
    profileTbl[, "ID_PROFILE" ] <- 3:4 
    
    # Write the 'new' data in the database:
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE", 
        data      = profileTbl, 
        mode      = "a" # append
    )   
    
    # Alternative method:
    profileTbl[, "ID_PROFILE" ] <- 5:6 # Change IDs.
    myDb[ "PROFILE", mode = "a" ] <- profileTbl 
    
    myDb[ "PROFILE" ] # Look at the result
    
    # This would not work, because one column is missing:
    # (because the underlying function use dbWriteTable)
    try( 
        edbWrite( 
            edb       = myDb, 
            tableName = "PROFILE", 
            data      = profileTbl[, -1 ], 
            mode      = "a"  
        )   
    )   # Error
    
    # But as "ID_PROFILE" is a primary key, it can be omittted 
    # if 'getKey' is specified:
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE", 
        data      = profileTbl[, -1 ], 
        mode      = "a", 
        #verbose   = TRUE,
        getKey    = "ID_PROFILE" 
    )   #
    
    ## Create a new table:
    
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE2", 
        data      = profileTbl, 
        mode      = "o"  
    )   #
    
    # Alternative metod:
    myDb[ "PROFILE3", mode = "o" ] <- profileTbl 
    
    edbNames( myDb ) 
    
    
    ## Update some values:
    profileTbl[, "COMMENTS" ] <- "My comment" 
    
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE", 
        data      = profileTbl, 
        mode      = "u", # update
        pKey      = "ID_PROFILE" # Primary key
    )   
    
    myDb[ "PROFILE" ]
    
    # Alternative method:
    profileTbl[, "COMMENTS" ] <- "My other comment" 
    myDb[ "PROFILE", mode = "u", pKey = "ID_PROFILE" ] <- profileTbl 
    
    myDb[ "PROFILE" ]
    
    
    ## Delete some rows
    edbDelete( 
        edb       = myDb, 
        tableName = "PROFILE", 
        sRow      = list("SQL" = "ID_PROFILE > 2")
    )   #
    
    
    
    ## Drop tables
    edbDrop( edb = myDb, tableName = "PROFILE2" ) 
    edbDrop( edb = myDb, tableName = "PROFILE3" ) 
    
    
    
    # It may be useful to transform some columns 'on-the-fly', before 
    # they are written to the database. In the example below we 
    # have some dates and times values, as well as some boolean stored 
    # as integers (seconds or days since 1970-01-01 or 0/1 values, 
    # respectively). We want to transform them when writing in the database.
    
    myDb[ "MISCFORMAT" ]
    # NB: although Yes/No format, the last column is read as integer too...
    
    # So date variables have to be converted when written to the database.
    # The code below show how to do that.
    
    # Prepare a new record to be written:
    newRecord <- data.frame( 
        "ID_RECORD"   = 2, 
        "DAT_TIM_SEC" = as.POSIXct( "2011-12-15 12:00:00", tz = "GMT" ), 
        "DAT_DAY"     = as.Date( "2011-12-15" ), 
        "TEST_BOOL"   = FALSE, 
        "DAT_TIM"     = as.POSIXct( "2011-12-15 12:00:00", tz = "GMT" ), 
        "DAT"         = as.Date( "2011-12-15" ), 
        "TEST_BOOL2"  = FALSE  
    )   #
    newRecord 
    
    # Write the record:
    myDb[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = as.integer, 
        "DAT_DAY" = as.integer, "TEST_BOOL" = as.integer, 
        "TEST_BOOL2" = as.integer ) ] <- newRecord 
    
    # The records have been written as integers:
    myDb[ "MISCFORMAT" ]
    
    # But we can convert them on-the-fly
    
    # Function to convert POSIX integer "seconds from 1970-01-01" into 
    # R POSIXct date format.
    formatDT <- function( x, tz = "GMT" ){ 
        res <- ISOdatetime( year = 1970, month = 1, day = 1, 
            hour = 0, min = 0, sec = 0, tz = tz ) 
        res <- res + x 
        return( res ) } 
            
    
    # Function to convert integer "days from 1970-01-01" into 
    # R Date format.
    formatD <- function( x, tz = "GMT" ){ 
        res <- ISOdate( year = 1970, month = 1, day = 1, tz = tz ) 
        res <- res + (x * 24 * 60 * 60 ) 
        res <- as.Date( res ) 
        return( res ) } 
    
    myDb[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = formatDT, 
        "DAT_DAY" = formatD, "TEST_BOOL" = as.logical ) ] 
    
    # Misc test: just to make sure that this works too:
    edbWrite( 
        edb       = myDb, 
        tableName = "MISCFORMAT", 
        data      = newRecord[, -1 ], 
        mode      = "a", 
        verbose   = TRUE,
        getKey    = "ID_RECORD", 
        formatCol = list( "DAT_TIM_SEC" = as.integer, 
            "DAT_DAY" = as.integer, "TEST_BOOL" = as.integer, 
            "TEST_BOOL2" = as.integer )
    )   #    
    


    # It is possible to write an operation "log" every time edbWrite() 
    # is used (or edbDelete() or edbDrop()). The exact operation is 
    # not logged, but rather the function name, the table concerned, 
    # the version of R and easydb, the date, an eventual log message, 
    # etc. Set the argument 'logOp' to TRUE to log operations:
    
    # - Fetch some data
    profileTbl <- myDb[ "PROFILE", sRow = list( "ID_PROFILE" = 1 ) ] 
    
    # - Write it back, with a log
    myDb[ "PROFILE", mode = "u", pKey = "ID_PROFILE", logOp = TRUE, 
        logMsg = "Some log message" ] <- profileTbl 
    
    # Now check the log:
    myDb[ "edbLog" ]
    
    
    
    ### Clean-up
    file.remove( "soils.mdb" ) 
    
    
    
    ### Access 2007 ---------------------------------------------
    file.copy( 
        from = system.file( "soils.accdb", package = "easyrodbcaccess" ), 
        to   = "soils.accdb" 
    )   
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb2 <- edb( dbType = "RODBC_Access", dbName = "soils.accdb", 
        accessVersion = 2007 ) 
    
    
    
    ### Use the database:

    ## Write data in a table in the database
    
    # First retrieve the table profile
    profileTbl <- myDb2[ "PROFILE" ] 
    
    # Change the ID's (pseudo new profiles)
    profileTbl[, "ID_PROFILE" ] <- 3:4 
    
    # Write the 'new' data in the database:
    edbWrite( 
        edb       = myDb2, 
        tableName = "PROFILE", 
        data      = profileTbl, 
        mode      = "a" # append
    )   
    
    # Alternative method:
    profileTbl[, "ID_PROFILE" ] <- 5:6 # Change IDs.
    myDb2[ "PROFILE", mode = "a" ] <- profileTbl 
    
    myDb2[ "PROFILE" ] # Look at the result
    
    # This would not work, because one column is missing:
    # (because the underlying function use dbWriteTable)
    try( 
        edbWrite( 
            edb       = myDb2, 
            tableName = "PROFILE", 
            data      = profileTbl[, -1 ], 
            mode      = "a"  
        )   
    )   # Error
    
    # But as "ID_PROFILE" is a primary key, it can be omittted 
    # if 'getKey' is specified:
    edbWrite( 
        edb       = myDb2, 
        tableName = "PROFILE", 
        data      = profileTbl[, -1 ], 
        mode      = "a", 
        #verbose   = TRUE,
        getKey    = "ID_PROFILE" 
    )   #
    
    ## Create a new table:
    
    edbWrite( 
        edb       = myDb2, 
        tableName = "PROFILE2", 
        data      = profileTbl, 
        mode      = "o"  
    )   #
    
    # Alternative metod:
    myDb2[ "PROFILE3", mode = "o" ] <- profileTbl 
    
    edbNames( myDb2 ) 
    
    
    ## Update some values:
    profileTbl[, "COMMENTS" ] <- "My comment" 
    
    edbWrite( 
        edb       = myDb2, 
        tableName = "PROFILE", 
        data      = profileTbl, 
        mode      = "u", # update
        pKey      = "ID_PROFILE" # Primary key
    )   
    
    myDb2[ "PROFILE" ]
    
    # Alternative method:
    profileTbl[, "COMMENTS" ] <- "My other comment" 
    myDb2[ "PROFILE", mode = "u", pKey = "ID_PROFILE" ] <- profileTbl 
    
    myDb2[ "PROFILE" ]
    
    
    ## Delete some rows
    edbDelete( 
        edb       = myDb2, 
        tableName = "PROFILE", 
        sRow      = list("SQL" = "ID_PROFILE > 2")
    )   #
    
    
    
    ## Drop tables
    edbDrop( edb = myDb2, tableName = "PROFILE2" ) 
    edbDrop( edb = myDb2, tableName = "PROFILE3" ) 
    
    
    
    # It may be useful to transform some columns 'on-the-fly', before 
    # they are written to the database. In the example below we 
    # have some dates and times values, as well as some boolean stored 
    # as integers (seconds or days since 1970-01-01 or 0/1 values, 
    # respectively). We want to transform them when writing in the database.
    
    myDb2[ "MISCFORMAT" ]
    # NB: although Yes/No format, the last column is read as integer too...
    
    # So date variables have to be converted when written to the database.
    # The code below show how to do that.
    
    # Prepare a new record to be written:
    newRecord <- data.frame( 
        "ID_RECORD"   = 2, 
        "DAT_TIM_SEC" = as.POSIXct( "2011-12-15 12:00:00", tz = "GMT" ), 
        "DAT_DAY"     = as.Date( "2011-12-15" ), 
        "TEST_BOOL"   = FALSE, 
        "DAT_TIM"     = as.POSIXct( "2011-12-15 12:00:00", tz = "GMT" ), 
        "DAT"         = as.Date( "2011-12-15" ), 
        "TEST_BOOL2"  = FALSE  
    )   #
    newRecord 
    
    # Write the record:
    myDb2[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = as.integer, 
        "DAT_DAY" = as.integer, "TEST_BOOL" = as.integer, 
        "TEST_BOOL2" = as.integer ) ] <- newRecord 
    
    # The records have been written as integers:
    myDb2[ "MISCFORMAT" ]
    
    # But we can convert them on-the-fly
    
    # Function to convert POSIX integer "seconds from 1970-01-01" into 
    # R POSIXct date format.
    formatDT <- function( x, tz = "GMT" ){ 
        res <- ISOdatetime( year = 1970, month = 1, day = 1, 
            hour = 0, min = 0, sec = 0, tz = tz ) 
        res <- res + x 
        return( res ) } 
            
    
    # Function to convert integer "days from 1970-01-01" into 
    # R Date format.
    formatD <- function( x, tz = "GMT" ){ 
        res <- ISOdate( year = 1970, month = 1, day = 1, tz = tz ) 
        res <- res + (x * 24 * 60 * 60 ) 
        res <- as.Date( res ) 
        return( res ) } 
    
    myDb2[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = formatDT, 
        "DAT_DAY" = formatD, "TEST_BOOL" = as.logical ) ] 
    
    # Misc test: just to make sure that this works too:
    edbWrite( 
        edb       = myDb2, 
        tableName = "MISCFORMAT", 
        data      = newRecord[, -1 ], 
        mode      = "a", 
        verbose   = TRUE,
        getKey    = "ID_RECORD", 
        formatCol = list( "DAT_TIM_SEC" = as.integer, 
            "DAT_DAY" = as.integer, "TEST_BOOL" = as.integer, 
            "TEST_BOOL2" = as.integer )
    )   #    
    


    # It is possible to write an operation "log" every time edbWrite() 
    # is used (or edbDelete() or edbDrop()). The exact operation is 
    # not logged, but rather the function name, the table concerned, 
    # the version of R and easydb, the date, an eventual log message, 
    # etc. Set the argument 'logOp' to TRUE to log operations:
    
    # - Fetch some data
    profileTbl <- myDb2[ "PROFILE", sRow = list( "ID_PROFILE" = 1 ) ] 
    
    # - Write it back, with a log
    myDb2[ "PROFILE", mode = "u", pKey = "ID_PROFILE", logOp = TRUE, 
        logMsg = "Some log message" ] <- profileTbl 
    
    # Now check the log:
    myDb2[ "edbLog" ]
    
    
    
    ### Clean-up
    file.remove( "soils.accdb" ) 
}   #}
