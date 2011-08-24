\name{edbDelete}
\alias{edbDelete}
\title{Delete all or some rows in a table in a database (referenced by 'edb').}
\description{Delete all or some rows in a table in a database (referenced by 'edb'). 
Generic function that call class-specific method corresponding 
to the class of the \code{edb} object provided.}
\usage{
edbDelete(edb, tableName, sRow = NULL, sRowOp = c("AND", "OR")[1], ...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{sRow}{A list of named items. List of contrains/criterion to be applied 
on \code{tableName} to retrieve a subset of rows. Each item in 
\code{rowC} must be named after the columns on which the constrain 
apply. The (vector of) value(s) of each items are the possible values 
that can be retrieved. Values can be character or numeric. If NULL 
(the default), all values are returned.}
  \item{sRowOp}{A single character string. Operator to be used to combine multiple 
constrains in sRow. Possible values are "OR" or "AND". Default value 
is "AND".}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbDelete")}}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbWrite}}, 
\code{link{edbNames}}, \code{link{edbColnames}}, 
\code{link{edbRead}}.}
\examples{# See related methods for detailed examples:
# (for example ?edbDelete.RSQLite_SQLite) 
require( "easydb" ) 
methods( "edbDelete" )
}
