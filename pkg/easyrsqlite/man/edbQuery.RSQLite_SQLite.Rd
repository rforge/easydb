\name{edbQuery.RSQLite_SQLite}
\alias{edbQuery.RSQLite_SQLite}
\title{Send and retrieve a query in a SQLIte database (referenced by 'edb').}
\description{Send and retrieve a query in a SQLIte database (referenced by 'edb'). }
\usage{\method{edbQuery}{RSQLite_SQLite}(edb, statement, formatCol = NULL, 
    testFiles = TRUE, verbose = FALSE, ...)}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.}
  \item{statement}{Single character string. SQL statement / SQL query to be passed 
to \code{link[DBI]{dbGetQuery}}.}
  \item{formatCol}{If not NULL, a named list of functions to be applied to certain columns 
after the data has been extracted from the database. The name of each list 
item gives the column to process, and the value of each item gives the 
function that must be applied. For instance 
formatCol = list("DATE"=as.Date) will apply the function 
\link[base]{as.Date} to the column "DATE".}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to \code{link[DBI]{dbGetQuery}}.}
}

\value{The function returns the requested table. }

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link[easydb]{edb}}, \code{link[DBI]{dbGetQuery}}.}
\examples{library( "easyrsqlite" ) 



### Make a copy of SQLite example database:
#   (a database of soil profile description) 
file.copy( 
    from = system.file( "soils.db", package = "easyrsqlite" ), 
    to   = "soils.db" 
)   

# soils.db is now in your working directory.



library( "RSQLite" ) # Because soils.db is a SQLite database...



### Describe the database (NB: this is not a connection)
myDb <- edb( dbType = "RSQLite_SQLite", dbName = "soils.db" ) 



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
file.remove( "soils.db" ) 
}
