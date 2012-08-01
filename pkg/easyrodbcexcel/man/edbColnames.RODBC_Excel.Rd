\name{edbColnames.RODBC_Excel}
\alias{edbColnames.RODBC_Excel}
\title{Retrieve column names of a table in a MS Excel file (referenced by 'edb').}
\description{Retrieve column names of a table in a MS Excel file 
(referenced by 'edb'). Wrapper around RODBC::sqlColumns().
Notice that the method does NOT retrieve the full table to 
get its column names (so it should work even if the table is big).}
\usage{
\method{edbColnames}{RODBC_Excel}(edb,
tableName,
onlyNames = TRUE,
testFiles = TRUE,
...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{onlyNames}{Single logical. If TRUE only returns the column names (vector), 
and if FALSE returns a detailed table as in 
\code{\link[RODBC]{sqlColumns}}.}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{\dots}{Additional parameters to be passed to dbListFields(). See 
\code{?dbListFields}.}
}

\value{The function returns a vector of character strings with the 
columns / fields of the original MS Excel table, or a table with 
full details on the columns (see \code{onlyNames}).}

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link[easydb]{edb}}, \code{\link{edbRead.RODBC_Excel}}, 
\code{\link{edbWrite.RODBC_Excel}}, 
\code{\link{edbNames.RODBC_Excel}}.}
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
    
    # List the column names in a table:
    edbColnames( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    
    
    
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
    
    # List the column names in a table:
    edbColnames( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    
    
    
    ### Clean-up
    file.remove( "soils.xlsx" ) 
}   #}
