\name{edbDrop.RODBC_Access}
\alias{edbDrop.RODBC_Access}
\title{Drop a table in a MS Access database (referenced by 'edb').}
\description{Drop a table in a MS Access database (referenced by 'edb'). }
\usage{
\method{edbDrop}{RODBC_Access}(edb, tableName, logOp = FALSE, logRandId = rnorm(1), logMsg = as.character(NA), logTableName = "edbLog", logCreateTableIfNotExist = TRUE, testFiles = TRUE, verbose = FALSE, ...)
}
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
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to \code{dbGetQuery}.}
}

\value{The function returns the requested table. }

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link{edbWrite.RODBC_Access}}, 
\code{link{edbNames.RODBC_Access}}, 
\code{link{edbColnames.RODBC_Access}}.}
\examples{# See ?edbWrite.RODBC_Access for an example
}
