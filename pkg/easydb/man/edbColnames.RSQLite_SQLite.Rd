\name{edbColnames.RSQLite_SQLite}
\alias{edbColnames.RSQLite_SQLite}
\title{Retrieve column names of a table in a SQLite database (referenced by 'edb').}
\description{Retrieve column names of a table in a SQLIte database 
(referenced by 'edb'). Wrapper around DBI::dbListFields().
Notice that the method does NOT retrieve the full table to 
get its column names (so it should work even if the table is big).}
\usage{
\method{edbColnames}{RSQLite_SQLite}(edb, tableName, testFiles = TRUE, ...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{\dots}{Additional parameters to be passed to dbListFields(). See 
\code{?dbListFields}.}
}

\value{The function returns a vector of character strings with the 
columns / fields of the original sqlite table.}

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbRead.RSQLite_SQLite}}, 
\code{link{edbwrite.RSQLite_SQLite}}, 
\code{link{edbNames.RSQLite_SQLite}}.}
\examples{library( "easydb" ) 



### Make a copy of SQLite example database:
#   (a database of soil profile description) 
file.copy( 
    from = system.file( "soils.db", package = "easydb" ), 
    to   = "soils.db" 
)   

# soils.db is now in your working directory.



library( "RSQLite" ) # Because soils.db is a SQLite database...



### Describe the database (NB: this is not a connection)
myDb <- edb( dbType = "RSQLite_SQLite", dbName = "soils.db" ) 



### Use the database:

# List the column names in a table:
edbColnames( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 



### Clean-up
file.remove( "soils.db" ) }
