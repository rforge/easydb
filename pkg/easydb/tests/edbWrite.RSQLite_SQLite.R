library( "easydb" ) 



### Make a copy of SQLite example database:
#   (a database of soil profile description) 
file.copy( 
    from = system.file( "soils.db", package = "easydb" ), 
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

