\name{edbWrite.RODBC_Access}
\alias{edbWrite.RODBC_Access}
\title{Write data in a MS Access table in a database (referenced by 'edb').}
\description{Write data in a table in a MS Access database (referenced by 'edb'). }
\usage{
\method{edbWrite}{RODBC_Access}(edb, tableName, data, mode = c("a", "u", "o")[1], pKey = NULL, getKey = NULL, testFiles = TRUE, verbose = FALSE, ...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{data}{data.frame. Data to be writen in \code{tableName}. If the table 
has a PRIMARY KEY, and if it is AUTOINCREMENT, then the column 
can be omitted, and the attributed ID's will be retrieved if 
\code{getKey = TRUE} (not the default). If \code{sRow} is not 
NULL, then data must contain the column names given in \code{sRow}.}
  \item{mode}{Single character string. If \code{"a"} (default), the data are 
appened to the table (added after the last row), and \code{sRow} 
is ignored. If \code{"u"}, the data are updated according to some 
critearia in \code{sRow} (that can't be NULL). If \code{"o"}, 
the table is overwritten and \code{sRow} is ignored. }
  \item{pKey}{Single character string (if mode == "u") or NULL. Column name that 
is PRIMARY KEY in the table.}
  \item{getKey}{Single logical. If TRUE, the latest attributed primary keys will be 
retrieved.}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbWrite")}}
}

\value{If id.col.nm is not NA, the function returns a list containing
a vector of ID values, and named after 'id.col.nm'. 
If an error message is detected the function stops.}

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbRead.RODBC_Access}}, 
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

    ## Write data in a table in the database
    
    # First retrieve the table profile
    profileTbl <- myDb[ "PROFILE" ] 
    
    # Change the ID's (pseudo new profiles)
    profileTbl[, "ID_PROFILE" ] <- 3:4 
    
    # Write the 'new' data in the database:
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE", 
        data      = profileTbl, 
        mode      = "a" # append
    )   
    
    # Alternative method:
    profileTbl[, "ID_PROFILE" ] <- 5:6 # Change IDs.
    myDb[ "PROFILE", mode = "a" ] <- profileTbl 
    
    myDb[ "PROFILE" ] # Look at the result
    
    # This would not work, because one column is missing:
    # (because the underlying function use dbWriteTable)
    try( 
        edbWrite( 
            edb       = myDb, 
            tableName = "PROFILE", 
            data      = profileTbl[, -1 ], 
            mode      = "a"  
        )   
    )   # Error
    
    # But as "ID_PROFILE" is a primary key, it can be omittted 
    # if 'getKey' is specified:
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE", 
        data      = profileTbl[, -1 ], 
        mode      = "a", 
        #verbose   = TRUE,
        getKey    = "ID_PROFILE" 
    )   #
    
    ## Create a new table:
    
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE2", 
        data      = profileTbl, 
        mode      = "o"  
    )   #
    
    # Alternative metod:
    myDb[ "PROFILE3", mode = "o" ] <- profileTbl 
    
    edbNames( myDb ) 
    
    
    ## Update some values:
    profileTbl[, "COMMENTS" ] <- "My comment" 
    
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE", 
        data      = profileTbl, 
        mode      = "u", # update
        pKey      = "ID_PROFILE" # Primary key
    )   
    
    myDb[ "PROFILE" ]
    
    # Alternative method:
    profileTbl[, "COMMENTS" ] <- "My other comment" 
    myDb[ "PROFILE", mode = "u", pKey = "ID_PROFILE" ] <- profileTbl 
    
    myDb[ "PROFILE" ]
    
    
    ## Delete some rows
    edbDelete( 
        edb       = myDb, 
        tableName = "PROFILE", 
        sRow      = list("SQL" = "ID_PROFILE > 2")
    )   #
    
    
    
    ## Drop tables
    edbDrop( edb = myDb, tableName = "PROFILE2" ) 
    edbDrop( edb = myDb, tableName = "PROFILE3" ) 
    
    
    
    ### Clean-up
    file.remove( "soils.mdb" ) 
}   #}
