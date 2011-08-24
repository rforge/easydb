\name{edbWrite}
\alias{edbWrite}
\title{Write data in a table in a database (referenced by 'edb').}
\description{Write data in a table in a database (referenced by 'edb'). 
Generic function that call class-specific method corresponding 
to the class of the \code{edb} object provided.}
\usage{
edbWrite(edb, tableName, data, mode = c("a", "u", "o")[1], pKey = NULL, getKey = NULL, ...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{data}{data.frame. Data to be writen in \code{tableName}. If the table 
has a PRIMARY KEY, and if it is AUTOINCREMENT, then the column 
can be omitted, and the attributed ID's will be retrieved if 
\code{getKey = TRUE} (not the default).}
  \item{mode}{Single character string. If \code{"a"} (default), the data are 
appened to the table (added after the last row), and \code{sRow} 
is ignored. If \code{"u"}, the data are updated according to some 
critearia in \code{sRow} (that can't be NULL). If \code{"o"}, 
the table is overwritten and \code{sRow} is ignored. }
  \item{pKey}{Single character string (if mode == "u") or NULL. Column name that 
is PRIMARY KEY in the table.}
  \item{getKey}{Single logical. If TRUE, the latest attributed primary keys will be 
retrieved.}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbWrite")}}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbRead}}, 
\code{link{edbNames}}, 
\code{link{edbColnames}}.}
\examples{# See related methods for detailed examples:
# (for example ?edbWrite.RSQLite_SQLite) 
require( "easydb" ) 
methods( "edbWrite" )
}
