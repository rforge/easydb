\name{edbDrop}
\alias{edbDrop}
\title{Drop a table in a database (referenced by 'edb').}
\description{Drop a table in a database (referenced by 'edb'). 
Generic function that call class-specific method corresponding 
to the class of the \code{edb} object provided.}
\usage{edbDrop(edb, tableName, logOp = FALSE, logRandId = rnorm(1), 
    logMsg = as.character(NA), logTableName = "edbLog", 
    logCreateTableIfNotExist = TRUE, ...)}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{logOp}{Single logical. If TRUE, then a log of the operation is written 
into the database, using the function \code{\link{edbLog}}. 
See the arguments below and \code{\link{edbLog}} for more details.}
  \item{logRandId}{Single numerical. See \code{\link{edbLog}}.}
  \item{logMsg}{Single character string. See \code{\link{edbLog}}.}
  \item{logTableName}{Single character string. See \code{\link{edbLog}}.}
  \item{logCreateTableIfNotExist}{Single logical. See \code{\link{edbLog}}.}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbDelete")}}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link{edb}}, \code{\link{edbWrite}}, 
\code{\link{edbNames}}, \code{\link{edbColnames}}, 
\code{\link{edbRead}}.}
\examples{# See related methods for detailed examples:
# (for example ?edbDrop.RSQLite_SQLite) 
require( "easydb" ) 
methods( "edbDrop" )
}
