\name{edbNames.RSQLite_SQLite}
\alias{edbNames.RSQLite_SQLite}
\title{Retrieve table names in a SQLIte database (referenced by 'edb').}
\description{Retrieve table names in a SQLite database (referenced by 'edb'). }
\usage{
\method{edbNames}{RSQLite_SQLite}(edb, testFiles = TRUE, ...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{\dots}{Additional parameters to be passed to \code{dbListTables}.}
}

\value{The function returns the list of tables found in the database. }

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbRead.RSQLite_SQLite}}, 
\code{link{edbWrite.RSQLite_SQLite}}, 
\code{link{edbColnames.RSQLite_SQLite}}.}
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



### 3. Use the database:

# List the tables:
edbNames( myDb ) 



### Clean-up
file.remove( "soils.db" ) 
}
