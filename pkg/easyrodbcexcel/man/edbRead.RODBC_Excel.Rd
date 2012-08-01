\name{edbRead.RODBC_Excel}
\alias{edbRead.RODBC_Excel}
\title{Read all or part of a table in a MS Excel file (referenced by 'edb').}
\description{Read all or part of a table in a MS Excel file (referenced by 'edb'). }
\usage{
\method{edbRead}{RODBC_Excel}(edb,
tableName,
sRow = NULL,
sCol = NULL,
sRowOp = c("AND",
"OR")[1],
formatCol = NULL,
distinct = FALSE,
orderBy = NULL,
testFiles = TRUE,
tableNameSuffix = "$",
verbose = FALSE,
...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{sRow}{A list of named items. List of contrains/criterion to be applied 
on \code{tableName} to retrieve a subset of rows. Each item in 
\code{rowC} must be named after the columns on which the constrain 
apply. The (vector of) value(s) of each items are the possible values 
that can be retrieved. Values can be character or numeric. If NULL 
(the default), all values are returned.}
  \item{sCol}{Either (1) a vector of character strings with the name of the 
columns to retrieve or (2) a vector of logical of the same 
length as the number of columns or (3) a vector of indexes / 
integers giving the indexes of the column to retrieve. If 
negative, then it indicates the indexes of the column to leave 
out.}
  \item{sRowOp}{A single character string. Operator to be used to combine multiple 
constrains in sRow. Possible values are "OR" or "AND". Default value 
is "AND".}
  \item{formatCol}{If not NULL, a named list of functions to be applied to certain columns 
after the data has been extracted from the database. The name of each list 
item gives the column to process, and the value of each item gives the 
function that must be applied. For instance 
formatCol = list("DATE"=as.Date) will apply the function 
\link[base]{as.Date} to the column "DATE".}
  \item{distinct}{Single logical. If TRUE, unique values in the result table will 
be returned, using the \code{SELECT DISTINCT} SQL statement. 
This is equivalent to applying \code{\link[base]{unique}} to the 
data.frame returned by the function, except that the action is 
performed inside the database (not in R).}
  \item{orderBy}{Vector of character strings, or NULL (the default). If non NULL, 
vector of column names that must be used to sort the result table. 
Column names may be followed by a space and 'DESC' if the column 
must be sorted in a descending order ('ASC', ascending, is the 
default). This operation is performed in the database with 
SQL ORDER BY statement and is equivalent to ordering the 
data in R with \code{\link[base]{order}}. You may write the 
column names between square brackets [] if they contain spaces.}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{tableNameSuffix}{Single logical. Suffix added at the end of the table name, if 
not already present. For some reasons ODBC Excel functions need 
column names ending with a  suffix. Set to  
to skip the suffix.}
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to \code{dbGetQuery}.}
}

\value{The function returns the requested table. }

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link[easydb]{edb}}, \code{\link{edbWrite.RODBC_Excel}}, 
\code{\link{edbNames.RODBC_Excel}}, 
\code{\link{edbColnames.RODBC_Excel}}.}
\examples{library( "easyrodbcexcel" ) 



# System check. Ingnore this or set 'testODBC' to TRUE 
testODBC <- (class(try(odbcDataSources())) != "try-error") & # Check (R)ODBC 
            (Sys.info()[[ "sysname" ]] == "Windows") &       # Only MS Windows
            (.Machine$"sizeof.pointer" == 4)                 # Only 32-bits 



### Windows only:
if( testODBC ){ 
    ### Make a copy of Excel example file:
    #   (a database of soil profile description) 
    file.copy( 
        from = system.file( "soils.xls", package = "easyrodbcexcel" ), 
        to   = "soils.xls" 
    )   
    
    # soils.xls is now in your working directory.
    
    
    
    library( "RODBC" ) # Because soils.db is a SQLite database...
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb <- edb( dbType = "RODBC_Excel", dbName = "soils.xls" ) 
    
    
    
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
            "SQL" = "NAME LIKE 'Al\%'"
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
    
    
    
    # It may be useful to transform some columns 'on-the-fly', after 
    # they have been read from the database. In the example below we 
    # have some dates and times values, as well as some boolean stored 
    # as integers (seconds or days since 1970-01-01 or 0/1 values, 
    # respectively). We want to transform them into dates or boolean.
    
    myDb[ "MISCFORMAT" ]
    # NB: although Yes/No format, the last column is read as integer too...
    
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
    file.remove( "soils.xls" ) 
    
    
    
    ### Access 2007 ---------------------------------------------
    file.copy( 
        from = system.file( "soils.xlsx", package = "easyrodbcexcel" ), 
        to   = "soils.xlsx" 
    )   
    
    # soils.db is now in your working directory.
    
    
    
    library( "RODBC" ) # Because soils.db is a SQLite database...
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb2 <- edb( dbType = "RODBC_Excel", dbName = "soils.xlsx", 
        excelVersion = 2007 ) 
    
    
    
    ### Use the database:

    ## Read data in a table in the database
    
    # Retrieve a table (data.frame style subsetting):
    myDb2[ "WRB_SOIL_GROUP" ]
    
    # Same operation, but with edbRead()
    edbRead( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    
    # Retrieve part of a table (with row constrains)
    myDb2[ "WRB_SOIL_GROUP", list("ABBREV" = c("AC","CR","PL")) ] 
    
    # Same operation, but with edbRead()
    edbRead( 
        edb       = myDb2, 
        tableName = "WRB_SOIL_GROUP", 
        sRow      = list("ABBREV" = c("AC","CR","PL")) 
    )   
    
    # Retrieve part of a table (row constrains + select only some columns)
    myDb2[ 
        "WRB_SOIL_GROUP", 
        list("ABBREV" = c("AC","CR","PL")), 
        c("ID_WRB_SOIL_GROUP","NAME") 
    ]   
    
    # Same operation, but with edbRead()
    edbRead( 
        edb       = myDb2, 
        tableName = "WRB_SOIL_GROUP", 
        sRow      = list("ABBREV" = c("AC","CR","PL")), 
        sCol      = c("ID_WRB_SOIL_GROUP","NAME") 
    )   
    
    # Use some SQL constrains
    myDb2[ 
        "WRB_SOIL_GROUP", 
        list( 
            "ABBREV" = c("AC","AB","AL","AN","AT"), 
            "SQL" = "NAME LIKE 'Al\%'"
        )   
    ]   
    
    # NB: the different elements in the list are separated by 
    # "AND" statement
    
    # Multiple row constrains, alternative (constrain 1 OR constrin 2) 
    myDb2[ 
        "WRB_SOIL_GROUP", 
        list( 
            "ABBREV" = c("AC","AB"), 
            "ID_WRB_SOIL_GROUP" = 25:30
        ),  
        sRowOp = "OR" 
    ]   
    
    
    
    # Check the dimension of a table
    # - Number of columns:
    edbNCol( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    # - Number of rows:
    edbNCol( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    # - Dimensions:
    edbDim( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    
    
    
    # More ways to select columns:
    
    # - Indexes
    myDb2[ "WRB_SOIL_GROUP", sCol = 2:3 ] 
    
    # - Logicals
    myDb2[ "WRB_SOIL_GROUP", sCol = c(FALSE,TRUE,TRUE) ] 
    
    
    
    # It may be useful to transform some columns 'on-the-fly', after 
    # they have been read from the database. In the example below we 
    # have some dates and times values, as well as some boolean stored 
    # as integers (seconds or days since 1970-01-01 or 0/1 values, 
    # respectively). We want to transform them into dates or boolean.
    
    myDb2[ "MISCFORMAT" ]
    # NB: although Yes/No format, the last column is read as integer too...
    
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
    myDb2[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = formatDT, 
        "DAT_DAY" = formatD, "TEST_BOOL" = as.logical ) ] 
    
    
    
    # SQL SELECT DISTINCT, equivalent of R unique().
    # To select unique / distinct values in a (group of) columns, 
    # set 'distinct' to TRUE:
    myDb2[ "PROFILE", sCol = "COMMENTS", distinct = TRUE ] 
    
    
    
    # SQL ORDER BY, equivalent of R order(). 
    # To sort the table according to some columns (ascending or 
    # descending), use 'orderBy':
    myDb2[ "WRB_SOIL_GROUP", orderBy = "ABBREV DESC" ] 
    
    
    
    ### Clean-up
    file.remove( "soils.xlsx" ) 
}   #}
