\name{edbRead.RODBC_MySQL}
\alias{edbRead.RODBC_MySQL}
\title{Read all or part of a table in a MySQL database (referenced by 'edb').}
\description{Read all or part of a table in a MySQL database (referenced by 'edb'). }
\usage{
\method{edbRead}{RODBC_MySQL}(edb, tableName, sRow = NULL, sCol = NULL, sRowOp = c("AND", "OR")[1], testFiles = TRUE, verbose = FALSE, ...)
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
  \item{sCol}{A vector of character strings. Names of the columns to retrieve.}
  \item{sRowOp}{A single character string. Operator to be used to combine multiple 
constrains in sRow. Possible values are "OR" or "AND". Default value 
is "AND".}
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
    myDb[ "WRB_SOIL_GROUP", list("NAME" = c("AC","CR","PL")) ] 
    
    # Same operation, but with edbRead()
    edbRead( 
        edb       = myDb, 
        tableName = "WRB_SOIL_GROUP", 
        sRow      = list("NAME" = c("AC","CR","PL")) 
    )   
    
    # Retrieve part of a table (row constrains + select only some columns)
    myDb[ 
        "WRB_SOIL_GROUP", 
        list("NAME" = c("AC","CR","PL")), 
        c("ID_WRB_SOIL_GROUP","NAME") 
    ]   
    
    # Same operation, but with edbRead()
    edbRead( 
        edb       = myDb, 
        tableName = "WRB_SOIL_GROUP", 
        sRow      = list("NAME" = c("AC","CR","PL")), 
        sCol      = c("ID_WRB_SOIL_GROUP","NAME") 
    )   
    
    # Use some SQL constrains
    myDb[ 
        "WRB_SOIL_GROUP", 
        list( 
            "NAME" = c("AC","AB","AL","AN","AT"), 
            "SQL" = "ABBREV LIKE 'Al\%'"
        )   
    ]   
    
    # NB: the different elements in the list are separated by 
    # "AND" statement
    
    # Multiple row constrains, alternative (constrain 1 OR constrin 2) 
    myDb[ 
        "WRB_SOIL_GROUP", 
        list( 
            "NAME" = c("AC","AB"), 
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
    
    
    
    ### Un-register the data source in ODBC (windows only)
    edbDataSource( myDb, trash = TRUE ) 
}   #
}
