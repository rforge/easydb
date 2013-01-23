\name{edbQuery}
\alias{edbQuery}
\title{Read all or part of a table in a database (referenced by 'edb').}
\description{Read all or part of a table in a database (referenced by 'edb'). 
Generic function that call class-specific method corresponding 
to the class of the \code{edb} object provided.}
\usage{edbQuery(edb, statement, formatCol = NULL, ...)}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{statement}{Single character string. SQL statement / SQL query to be passed 
to the database.}
  \item{formatCol}{If not NULL, a named list of functions to be applied to certain columns 
after the data has been extracted from the database. The name of each list 
item gives the column to process, and the value of each item gives the 
function that must be applied. For instance 
formatCol = list("DATE"=as.Date) will apply the function 
\link{as.Date} to the column "DATE".}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbRead")}}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link{edb}}, \code{\link{edbWrite}}, 
\code{\link{edbNames}}, \code{\link{edbColnames}}.}
\examples{# See related methods for detailed examples:
# (for example ?edbQuery.RSQLite_SQLite) 
require( "easydb" ) 
methods( "edbRead" )
}
