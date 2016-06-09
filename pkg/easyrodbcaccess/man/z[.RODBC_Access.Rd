\name{[.RODBC_Access}
\alias{[.RODBC_Access}
\title{"[]" method for reading all or part of a table in a MS Access database (referenced by 'edb').}
\description{"[" method for reading all or part of a table in a MS Access 
database (referenced by 'edb'). Wrapper for 
\code{\link{edbRead.RODBC_Access}}. }
\usage{

\method{[}{RODBC_Access}(edb, tableName, sRow = NULL, sCol = NULL, sRowOp = c("AND", "OR")[1], formatCol = NULL, distinct = FALSE, verbose = FALSE, ...)

}

\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.}
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
\link[base]{as.Date} to the column "DATE".}
  \item{distinct}{Single logical. If TRUE, unique values in the result table will 
be returned, using the \code{SELECT DISTINCT} SQL statement. 
This is equivalent to applying \code{\link[base]{unique}} to the 
data.frame returned by the function, except that the action is 
performed inside the database (not in R).}
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to \code{dbGetQuery}.}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link[easydb]{edb}}, \code{\link{edbRead.RODBC_Access}}, 
\code{\link{edbNames.RODBC_Access}}, 
\code{\link{edbColnames.RODBC_Access}}.}

