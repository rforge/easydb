\name{edbNames.RODBC_Access}
\alias{edbNames.RODBC_Access}
\title{Retrieve table names in a MS Access database (referenced by 'edb').}
\description{Retrieve table names in a MS Access database (referenced by 'edb'). }
\usage{\method{edbNames}{RODBC_Access}(edb, onlyNames = TRUE, testFiles = TRUE, 
    ...)}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.}
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



\seealso{\code{\link[easydb]{edb}}, \code{\link{edbRead.RODBC_Access}}, 
\code{\link{edbWrite.RODBC_Access}}, 
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
    
    # soils.db is now in your working directory.
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb <- edb( dbType = "RODBC_Access", dbName = "soils.mdb" ) 
    
    
    
    ### Use the database:
    
    # List the tables:
    edbNames( myDb ) 
    
    
    
    ### Clean-up
    file.remove( "soils.mdb" ) 
    
    
    
    ### Access 2007 ---------------------------------------------
    file.copy( 
        from = system.file( "soils.accdb", package = "easyrodbcaccess" ), 
        to   = "soils.accdb" 
    )   
    
    # soils.db is now in your working directory.
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb2 <- edb( dbType = "RODBC_Access", dbName = "soils.accdb", 
        accessVersion = 2007 ) 
    
    
    
    ### Use the database:
    
    # List the tables:
    edbNames( myDb2 ) 
    
    
    
    ### Clean-up
    file.remove( "soils.accdb" ) 
}   #
}
