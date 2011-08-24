\name{edbDrop.RSQLite_SQLite}
\alias{edbDrop.RSQLite_SQLite}
\title{Delete all or some rows in a table in a SQLIte database (referenced by 'edb').}
\description{Delete all or some rows in a table in a SQLIte database (referenced by 'edb'). }
\usage{
\method{edbDrop}{RSQLite_SQLite}(edb, tableName, testFiles = TRUE, verbose = FALSE, ...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
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
