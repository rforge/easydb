\name{edbColnames.RODBC_MySQL}
\alias{edbColnames.RODBC_MySQL}
\title{Retrieve column names of a table in a MySQL database (referenced by 'edb').}
\description{Retrieve column names of a table in a MySQL database 
(referenced by 'edb'). Wrapper around RODBC::sqlColumns().
Notice that the method does NOT retrieve the full table to 
get its column names (so it should work even if the table is big).}
\usage{
\method{edbColnames}{RODBC_MySQL}(edb, tableName, onlyNames = TRUE, ...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{onlyNames}{Single logical. If TRUE only returns the column names (vector), 
and if FALSE returns a detailed table as in 
\code{\link[RODBC]{sqlColumns}}.}
  \item{\dots}{Additional parameters to be passed to dbListFields(). See 
\code{?dbListFields}.}
}

\value{The function returns a vector of character strings with the 
columns / fields of the original MySQL table, or a table with 
full details on the columns (see \code{onlyNames}).}

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbRead.RODBC_MySQL}}, 
\code{link{edbwrite.RODBC_MySQL}}, 
\code{link{edbNames.RODBC_MySQL}}.}
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
    
    # List the column names in a table:
    edbColnames( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    
    # More details:
    edbColnames( 
        edb       = myDb, 
        tableName = "WRB_SOIL_GROUP", 
        onlyNames = FALSE # different too!
    )   #
    
    
    
    ### Un-register the data source in ODBC (windows only)
    edbDataSource( myDb, trash = TRUE ) 
}   #
}
