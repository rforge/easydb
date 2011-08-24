\name{[.RODBC_Access}
\alias{[.RODBC_Access}
\title{"[" method for reading all or part of a table in a MS Access database (referenced by 'edb').}
\description{"[" method for reading all or part of a table in a MS Access 
database (referenced by 'edb'). Wrapper for 
\code{\link{edbRead.RODBC_Access}}. }
\usage{
\method{[}{RODBC_Access}(edb, tableName, sRow = NULL, sCol = NULL, sRowOp = c("AND", "OR")[1], verbose = FALSE, ...)
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
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to \code{dbGetQuery}.}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbRead.RODBC_Access}}, 
\code{link{edbNames.RODBC_Access}}, 
\code{link{edbColnames.RODBC_Access}}.}

