\name{edbDelete.RSQLite_SQLite}
\alias{edbDelete.RSQLite_SQLite}
\title{Delete all or some rows in a table in a SQLIte database (referenced by 'edb').}
\description{Delete all or some rows in a table in a SQLIte database (referenced by 'edb'). }
\usage{
\method{edbDelete}{RSQLite_SQLite}(edb,
tableName,
sRow = NULL,
sRowOp = c("AND",
"OR")[1],
logOp = FALSE,
logRandId = rnorm(1),
logMsg = as.character(NA),
logTableName = "edbLog",
logCreateTableIfNotExist = TRUE,
testFiles = TRUE,
verbose = FALSE,
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
  \item{sRowOp}{A single character string. Operator to be used to combine multiple 
constrains in sRow. Possible values are "OR" or "AND". Default value 
is "AND".}
  \item{logOp}{Single logical. If TRUE, then a log of the operation is written 
into the database, using the function \code{\link{edbLog}}. 
See the arguments below and \code{\link{edbLog}} for more details.}
  \item{logRandId}{Single numerical. See \code{\link{edbLog}}.}
  \item{logMsg}{Single character string. See \code{\link{edbLog}}.}
  \item{logTableName}{Single character string. See \code{\link{edbLog}}.}
  \item{logCreateTableIfNotExist}{Single logical. See \code{\link{edbLog}}.}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to \code{dbGetQuery}.}
}

\value{The function returns the requested table. }

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbWrite.RSQLite_SQLite}}, 
\code{link{edbNames.RSQLite_SQLite}}, 
\code{link{edbColnames.RSQLite_SQLite}}, \code{link{edbRead.RSQLite_SQLite}}}
\examples{# See ?edbWrite.RSQLite_SQLite for an example
}
