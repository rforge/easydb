\name{edbNames.RODBC_Excel}
\alias{edbNames.RODBC_Excel}
\title{Retrieve table names in a MS Excel file (referenced by 'edb').}
\description{Retrieve table names in a MS Excel file (referenced by 'edb'). }
\usage{
\method{edbNames}{RODBC_Excel}(edb,
onlyNames = TRUE,
testFiles = TRUE,
...)
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



\seealso{\code{\link{edb}}, \code{\link{edbRead.RODBC_Excel}}, 
\code{\link{edbWrite.RODBC_Excel}}, 
\code{\link{edbColnames.RODBC_Excel}}.}
\examples{library( "easydb" ) 
library( "RODBC" ) 



# System check. Ingnore this or set 'testODBC' to TRUE 
testODBC <- (class(try(odbcDataSources())) != "try-error") & # Check (R)ODBC 
            (Sys.info()[[ "sysname" ]] == "Windows") &       # Only MS Windows
            (.Machine$"sizeof.pointer" == 4)                 # Only 32-bits 



### Windows only:
if( testODBC ){ 
    ### Make a copy of Excel example file:
    #   (a database of soil profile description) 
    file.copy( 
        from = system.file( "soils.xls", package = "easydb" ), 
        to   = "soils.xls" 
    )   
    
    # soils.xls is now in your working directory.
    
    
    
    library( "RODBC" ) # Because soils.db is a SQLite database...
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb <- edb( dbType = "RODBC_Excel", dbName = "soils.xls" ) 
    
    
    
    ### Use the database:
    
    # List the tables:
    edbNames( myDb ) 
    
    
    
    ### Clean-up
    file.remove( "soils.xls" ) 
    
    
    
    ### Access 2007 ---------------------------------------------
    file.copy( 
        from = system.file( "soils.xlsx", package = "easydb" ), 
        to   = "soils.xlsx" 
    )   
    
    # soils.db is now in your working directory.
    
    
    
    library( "RODBC" ) # Because soils.db is a SQLite database...
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb2 <- edb( dbType = "RODBC_Excel", dbName = "soils.xlsx", 
        excelVersion = 2007 ) 
    
    
    
    ### Use the database:
    
    # List the tables:
    edbNames( myDb2 ) 
    
    
    
    ### Clean-up
    file.remove( "soils.xlsx" ) 
}   #
}
