\name{edbDrop}
\alias{edbDrop}
\title{Drop a table in a database (referenced by 'edb').}
\description{Drop a table in a database (referenced by 'edb'). 
Generic function that call class-specific method corresponding 
to the class of the \code{edb} object provided.}
\usage{
edbDrop(edb, tableName, ...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbDelete")}}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbWrite}}, 
\code{link{edbNames}}, \code{link{edbColnames}}, 
\code{link{edbRead}}.}
\examples{# See related methods for detailed examples:
# (for example ?edbDrop.RSQLite_SQLite) 
require( "easydb" ) 
methods( "edbDrop" )
}
