\name{edbNames.RODBC_Access}
\alias{edbNames.RODBC_Access}
\title{Retrieve table names in a MS Access database (referenced by 'edb').}
\description{Retrieve table names in a MS Access database (referenced by 'edb'). }
\usage{
\method{edbNames}{RODBC_Access}(edb, onlyNames = TRUE, testFiles = TRUE, ...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{onlyNames}{Single logical. If TRUE only returns the table names (vector), 
and if FALSE returns a detailed table as in 
\code{\link[RODBC]{sqlTables}}.}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{\dots}{Additional parameters to be passed to \code{dbListTables}.}
}

\value{The function returns the list of tables found in the database. }

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbRead.RODBC_Access}}, 
\code{link{edbWrite.RODBC_Access}}, 
\code{link{edbColnames.RRODBC_Access}}.}
\examples{library( "easydb" ) 
library( "RODBC" ) 



# System check. Ingnore this or set 'testODBC' to TRUE 
testODBC <- (class(try(odbcDataSources())) != "try-error") & 
            (Sys.info()[[ "sysname" ]] == "Windows")



### Windows only:
if( testODBC ){ 
    ### Make a copy of SQLite example database:
    #   (a database of soil profile description) 
    file.copy( 
        from = system.file( "soils.mdb", package = "easydb" ), 
        to   = "soils.mdb" 
    )   
    
    # soils.db is now in your working directory.
    
    
    
    library( "RODBC" ) # Because soils.db is a SQLite database...
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb <- edb( dbType = "RODBC_Access", dbName = "soils.mdb" ) 
    
    
    
    ### Use the database:
    
    # List the tables:
    edbNames( myDb ) 
    
    
    
    ### Clean-up
    file.remove( "soils.mdb" ) 
}   #
}
