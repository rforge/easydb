\name{edbNames.RODBC_MySQL}
\alias{edbNames.RODBC_MySQL}
\title{Retrieve table names in a MySQL database (referenced by 'edb').}
\description{Retrieve table names in a MySQL database (referenced by 'edb'). }
\usage{
\method{edbNames}{RODBC_MySQL}(edb,
onlyNames = TRUE,
...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{onlyNames}{Single logical. If TRUE only returns the table names (vector), 
and if FALSE returns a detailed table as in 
\code{\link[RODBC]{sqlTables}}.}
  \item{\dots}{Additional parameters to be passed to \code{dbListTables}.}
}

\value{The function returns the list of tables found in the database. }

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link{edb}}, \code{\link{edbRead.RODBC_MySQL}}, 
\code{\link{edbWrite.RODBC_MySQL}}, 
\code{\link{edbColnames.RODBC_MySQL}}.}
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
    
    # List the table names in the database:
    edbNames( edb = myDb ) 
    
    # with more details
    edbNames( edb = myDb, onlyNames = FALSE ) 
    
    
    
    ### Un-register the data source in ODBC (windows only)
    edbDataSource( myDb, trash = TRUE ) 
}   #

}
