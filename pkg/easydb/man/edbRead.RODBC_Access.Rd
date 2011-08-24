\name{edbRead.RODBC_Access}
\alias{edbRead.RODBC_Access}
\title{Read all or part of a table in a MS Access database (referenced by 'edb').}
\description{Read all or part of a table in a MS Access database (referenced by 'edb'). }
\usage{
\method{edbRead}{RODBC_Access}(edb, tableName, sRow = NULL, sCol = NULL, sRowOp = c("AND", "OR")[1], testFiles = TRUE, verbose = FALSE, ...)
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



\seealso{\code{link{edb}}, \code{link{edbWrite.RODBC_Access}}, 
\code{link{edbNames.RODBC_Access}}, 
\code{link{edbColnames.RODBC_Access}}.}
\examples{library( "easydb" ) 



### Windows only:
if( Sys.info()[[ "sysname" ]] == "Windows" ){ 
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
    edbNCol( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    # - Dimensions:
    edbDim( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    
    
    
    ### Clean-up
    file.remove( "soils.mdb" ) 
}   #}
