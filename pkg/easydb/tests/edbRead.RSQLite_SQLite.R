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

## Read data in a table in the database

# Retrieve a table (data.frame style subsetting):
myDb[ "WRB_SOIL_GROUP" ]

# Same operation, but with edbRead()
edbRead( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 

# Retrieve part of a table (with row constrains)
myDb[ "WRB_SOIL_GROUP", list("ABBREV" = c("AC","CR","PL")) ] 

# Same operation, but with edbRead()
edbRead( 
    edb       = myDb, 
    tableName = "WRB_SOIL_GROUP", 
    sRow      = list("ABBREV" = c("AC","CR","PL")) 
)   

# Retrieve part of a table (row constrains + select only some columns)
myDb[ 
    "WRB_SOIL_GROUP", 
    list("ABBREV" = c("AC","CR","PL")), 
    c("ID_WRB_SOIL_GROUP","NAME") 
]   

# Same operation, but with edbRead()
edbRead( 
    edb       = myDb, 
    tableName = "WRB_SOIL_GROUP", 
    sRow      = list("ABBREV" = c("AC","CR","PL")), 
    sCol      = c("ID_WRB_SOIL_GROUP","NAME") 
)   

# Use some SQL constrains
myDb[ 
    "WRB_SOIL_GROUP", 
    list( 
        "ABBREV" = c("AC","AB","AL","AN","AT"), 
        "SQL" = "NAME LIKE 'Al%'"
    )   
]   

# NB: the different elements in the list are separated by 
# "AND" statement

# Multiple row constrains, alternative (constrain 1 OR constrin 2) 
myDb[ 
    "WRB_SOIL_GROUP", 
    list( 
        "ABBREV" = c("AC","AB"), 
        "ID_WRB_SOIL_GROUP" = 25:30
    ),  
    sRowOp = "OR" 
]   



# Check the dimension of a table
# - Number of columns:
edbNCol( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
# - Number of rows:
edbNRow( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
# - Number of rows, with constrains: 
edbNRow( edb = myDb, tableName = "WRB_SOIL_GROUP", 
    sRow = list("ABBREV" = c("AC","AB")) ) 
# - Dimensions:
edbDim( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 



# More ways to select columns:

# - Indexes
myDb[ "WRB_SOIL_GROUP", sCol = 2:3 ] 

# - Logicals
myDb[ "WRB_SOIL_GROUP", sCol = c(FALSE,TRUE,TRUE) ] 



# Dates and times can not be stored as DATES in sqlite databases.
# They have been stored as integers (seconds or days since 1970-01-01)
# Similarly, boolean have to be stored as integers

myDb[ "MISCFORMAT" ]

# So date variables stored as integers have to be converted.
# The code below show how to do that.

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

# Now we can convert the columns on-the-fly
myDb[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = formatDT, 
    "DAT_DAY" = formatD, "TEST_BOOL" = as.logical ) ] 



# SQL SELECT DISTINCT, equivalent of R unique().
# To select unique / distinct values in a (group of) columns, 
# set 'distinct' to TRUE:
myDb[ "PROFILE", sCol = "COMMENTS", distinct = TRUE ] 



# SQL ORDER BY, equivalent of R order(). 
# To sort the table according to some columns (ascending or 
# descending), use 'orderBy':
myDb[ "WRB_SOIL_GROUP", orderBy = "ABBREV DESC" ] 



### Clean-up
file.remove( "soils.db" ) 

