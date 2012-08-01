\name{edbQuery.RODBC_Excel}
\alias{edbQuery.RODBC_Excel}
\title{Send and retrieve a query in an MS Excel database (referenced by 'edb').}
\description{Send and retrieve a query in a SQLIte database (referenced by 'edb'). }
\usage{
\method{edbQuery}{RODBC_Excel}(edb,
statement,
formatCol = NULL,
testFiles = TRUE,
readOnly = TRUE,
verbose = FALSE,
...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.}
  \item{statement}{Single character string. SQL statement / SQL query to be passed 
to \code{link[RODBC]{sqlQuery}}.}
  \item{formatCol}{If not NULL, a named list of functions to be applied to certain columns 
after the data has been extracted from the database. The name of each list 
item gives the column to process, and the value of each item gives the 
function that must be applied. For instance 
formatCol = list("DATE"=as.Date) will apply the function 
\link[base]{as.Date} to the column "DATE".}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{readOnly}{Single logical. If \code{TRUE}, the Excel file will be considered 
as read only. See \code{\link[RODBC]{odbcConnectExcel}} or 
\code{\link[RODBC]{odbcConnectExcel2007}}.}
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to \code{link[RODBC]{sqlQuery}}.}
}

\value{The function returns the requested table. }

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link[easydb]{edb}}, \code{link[RODBC]{sqlQuery}}.}
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
    myDb[ "WRB_SOIL_GROUP", list("ABBREV" = c("AC","CR","PL")), 
        verbose = TRUE ] 
    
    # Same operation, but with edbQuery() 
    edbQuery( edb = myDb, statement = 
        "SELECT * FROM [WRB_SOIL_GROUP] WHERE [ABBREV] IN ('AC','CR','PL')" ) 

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
    myDb2[ "WRB_SOIL_GROUP", list("ABBREV" = c("AC","CR","PL")), 
        verbose = TRUE ] 
    
    # Same operation, but with edbQuery() 
    edbQuery( edb = myDb2, statement = 
        "SELECT * FROM [WRB_SOIL_GROUP] WHERE [ABBREV] IN ('AC','CR','PL')" ) 
    
    ### Clean-up
    file.remove( "soils.xlsx" ) 
}   #}
