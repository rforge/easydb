\name{edbNames}
\alias{edbNames}
\title{Retrieve table names in a database (referenced by 'edb').}
\description{Retrieve table names in a database (referenced by 'edb'). 
Generic function that call class-specific method corresponding 
to the class of the \code{edb} object provided.}
\usage{edbNames(edb, ...)}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbColnames")}}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link{edb}}, \code{\link{edbRead}}, 
\code{\link{edbWrite}}, 
\code{\link{edbColnames}}.}
\examples{# See related methods for detailed examples:
# (for example ?edbNames.RSQLite_SQLite) 
require( "easydb" ) 
methods( "edbNames" )
}
