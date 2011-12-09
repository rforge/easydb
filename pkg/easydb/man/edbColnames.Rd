\name{edbColnames}
\alias{edbColnames}
\title{Retrieve column names of a table in a database (referenced by 'edb').}
\description{Retrieve column names of a table in a database (referenced by 
'edb'). Generic function that call class-specific method 
corresponding to the class of the \code{edb} object provided.
Notice that the methods do NOT retrieve the full table to 
get its column names (so it should work even if the table is big).}
\usage{
edbColnames(edb,
tableName,
...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbColnames")}}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbRead}}, 
\code{link{edbWrite}}, 
\code{link{edbNames}}.}
\examples{# See related methods for detailed examples:
# (for example ?edbColnames.RSQLite_SQLite) 
require( "easydb" ) 
methods( "edbColnames" ) 
}
