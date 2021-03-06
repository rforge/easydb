\name{edbWrite.RSQLite_SQLite}
\alias{edbWrite.RSQLite_SQLite}
\title{Write data in a SQLite table in a database (referenced by 'edb').}
\description{Write data in a table in a SQLite database (referenced by 'edb'). }
\usage{\method{edbWrite}{RSQLite_SQLite}(edb, tableName, data, mode = c("a", 
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
critearia in \code{pKey} (that can't be NULL). If \code{"o"}, 
the table is overwritten and \code{pKey} is ignored. }
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
Only used if getKey is not NULL or when mode == "u" in SQLite.}
  \item{dateFormat}{Single character string. 'format' argument of the functions 
format.Date() used to convert "Date" 
dates into character strings when writing into the database.
Only used if getKey is not NULL or when mode == "u" in SQLite.}
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



\seealso{\code{\link[easydb]{edb}}, \code{\link{edbRead.RSQLite_SQLite}}, 
\code{\link{edbNames.RSQLite_SQLite}}, 
\code{\link{edbColnames.RSQLite_SQLite}}.}
\examples{library( "easyrsqlite" ) 



### Make a copy of SQLite example database:
#   (a database of soil profile description) 
file.copy( 
    from = system.file( "soils.db", package = "easyrsqlite" ), 
    to   = "soils.db" 
)   

# soils.db is now in your working directory.



library( "RSQLite" ) # Because soils.db is a SQLite database...



### Describe the database (NB: this is not a connection)
myDb <- edb( dbType = "RSQLite_SQLite", dbName = "soils.db" ) 



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



# Dates and times can not be stored as DATES in sqlite databases.
# They have been stored as integers (seconds or days since 1970-01-01)
# Similarly, boolean have to be stored as integers

myDb[ "MISCFORMAT" ]

# So date variables have to be converted when written to the database.
# The code below show how to do that.

# Prepare a new record to be written:
newRecord <- data.frame( 
    "ID_RECORD"   = 2, 
    "DAT_TIM_SEC" = as.POSIXct( "2011-12-15 12:00:00", tz = "GMT" ), 
    "DAT_DAY"     = as.Date( "2011-12-15" ), 
    "TEST_BOOL"   = FALSE 
)   #
newRecord 

# Write the record:
myDb[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = as.integer, 
    "DAT_DAY" = as.integer, "TEST_BOOL" = as.integer ) ] <- newRecord 

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
file.remove( "soils.db" ) 
}
