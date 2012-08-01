\name{edbDrop.RODBC_Excel}
\alias{edbDrop.RODBC_Excel}
\title{Drop a table in a MS Excel file (referenced by 'edb').}
\description{Drop a table in a MS Excel file (referenced by 'edb'). }
\usage{
\method{edbDrop}{RODBC_Excel}(edb,
tableName,
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
  \item{edb}{An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{logOp}{Single logical. If TRUE, then a log of the operation is written 
into the database, using the function \code{\link[easydb]{edbLog}}. 
See the arguments below and \code{\link[easydb]{edbLog}} for more details.}
  \item{logRandId}{Single numerical. See \code{\link[easydb]{edbLog}}.}
  \item{logMsg}{Single character string. See \code{\link[easydb]{edbLog}}.}
  \item{logTableName}{Single character string. See \code{\link[easydb]{edbLog}}.}
  \item{logCreateTableIfNotExist}{Single logical. See \code{\link[easydb]{edbLog}}.}
  \item{testFiles}{Single logical. Should the function test for the presence 
(file.exist()) of the needed files in the folder before trying 
to fetch information from the database? }
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to \code{dbGetQuery}.}
}

\value{The function returns the requested table. }

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link[easydb]{edb}}, \code{\link{edbWrite.RODBC_Excel}}, 
\code{\link{edbNames.RODBC_Excel}}, 
\code{\link{edbColnames.RODBC_Excel}}.}
\examples{# See ?edbWrite.RODBC_Excel for an example
}
