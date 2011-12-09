\name{edbRead}
\alias{edbRead}
\title{Read all or part of a table in a database (referenced by 'edb').}
\description{Read all or part of a table in a database (referenced by 'edb'). 
Generic function that call class-specific method corresponding 
to the class of the \code{edb} object provided.}
\usage{
edbRead(edb,
tableName,
sRow = NULL,
sCol = NULL,
sRowOp = c("AND",
"OR")[1],
formatCol = NULL,
...)
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
  \item{sCol}{A vector of character strings. Names of the columns to retrieve.}
  \item{sRowOp}{A single character string. Operator to be used to combine multiple 
constrains in sRow. Possible values are "OR" or "AND". Default value 
is "AND".}
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



\seealso{\code{link{edb}}, \code{link{edbWrite}}, 
\code{link{edbNames}}, \code{link{edbColnames}}.}
\examples{# See related methods for detailed examples:
# (for example ?edbRead.RSQLite_SQLite) 
require( "easydb" ) 
methods( "edbRead" )
}
