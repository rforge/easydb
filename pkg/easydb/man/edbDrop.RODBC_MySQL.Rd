\name{edbDrop.RODBC_MySQL}
\alias{edbDrop.RODBC_MySQL}
\title{Drop a table in a MySQL database (referenced by 'edb').}
\description{Drop a table in a MySQL database (referenced by 'edb'). }
\usage{
\method{edbDrop}{RODBC_MySQL}(edb, tableName, testFiles = TRUE, verbose = FALSE, ...)
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



\seealso{\code{link{edb}}, \code{link{edbWrite.RODBC_MySQL}}, 
\code{link{edbNames.RODBC_MySQL}}, 
\code{link{edbColnames.RODBC_MySQL}}.}
\examples{# See ?edbWrite.RODBC_MySQL for an example
}
