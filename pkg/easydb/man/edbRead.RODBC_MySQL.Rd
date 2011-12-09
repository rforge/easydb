\name{edbRead.RODBC_MySQL}
\alias{edbRead.RODBC_MySQL}
\title{Read all or part of a table in a MySQL database (referenced by 'edb').}
\description{Read all or part of a table in a MySQL database (referenced by 'edb'). }
\usage{
\method{edbRead}{RODBC_MySQL}(edb,
tableName,
sRow = NULL,
sCol = NULL,
sRowOp = c("AND",
"OR")[1],
formatCol = NULL,
testFiles = TRUE,
verbose = FALSE,
...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
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
\link{as.Date} to the column "DATE".}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to \code{dbGetQuery}.}
}

\value{The function returns the requested table. }

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbWrite.RODBC_MySQL}}, 
\code{link{edbNames.RODBC_MySQL}}, 
\code{link{edbColnames.RODBC_MySQL}}.}
\examples{library( "easydb" ) 



testMySQL <- FALSE # Set to TRUE to run some tests
                   # after modifying the edb() part below 
                   # with your own database setting



### Windows only:
if( (Sys.info()[[ "sysname" ]] == "Windows") & testMySQL ){ 
    
    
    
    library( "RODBC" ) 
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb <- edb( 
        dbType       = "RODBC_MySQL", 
        dbSourceName = "nameOfODBCSource", # or any name you like
        dbName       = "nameOfDatabase", 
        dbLogin      = "yourUserName", 
        dbPwd        = "yourPassword", 
        dbHost       = "127.0.0.1", 
        dbPort       = 3306 
    )   #    
    
    
    
    ### Register the data source in ODBC 
    edbDataSource( myDb, verbose = TRUE ) 
    
    
    
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
    
    
    
    ### Un-register the data source in ODBC (windows only)
    edbDataSource( myDb, trash = TRUE ) 
}   #
}
