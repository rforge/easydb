\name{edbNRow.RODBC_Excel}
\alias{edbNRow.RODBC_Excel}
\title{Retrieve the number of rows of a table in a database (referenced by 'edb').}
\description{Retrieve the number of rows of a table in a database (referenced by 
'edb'). Notice that the methods do NOT retrieve the full table to 
get its column names (so it should work even if the table is big).}
\usage{
\method{edbNRow}{RODBC_Excel}(edb,
tableName,
sRow = NULL,
sRowOp = c("AND",
"OR")[1],
verbose = FALSE,
...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{sRow}{A list of named items. List of contrains/criterion to be applied 
on a table to retrieve a subset of rows. Each item in 
\code{sRow} must be named after the columns on which the constrain 
apply. The (vector of) value(s) of each items are the possible values 
that can be retrieved. Values can be character or numeric. If NULL 
(the default), all values are returned.}
  \item{sRowOp}{A single character string. Operator to be used to combine multiple 
constrains in sRow. Possible values are "OR" or "AND". Default value 
is "AND".}
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to \code{edbQuery}.}
}

\value{Returns the number of rows in the table, rows that respect \code{sRow} 
constrains if \code{sRow} is not \code{NULL}.}

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbColnames}}, 
\code{link{edbRead}}.}

