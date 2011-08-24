\name{[<-.RODBC_Access}
\alias{[<-.RODBC_Access}
\title{"[<-" method for MS Access databases. Write data in a MS Access table in a database (referenced by 'edb').}
\description{"[<-" method for MS Access databases. Write data in a table in a 
MS Access database (referenced by 'edb'). }
\usage{
\method{[}{RODBC_Access}(edb, tableName, mode = "a", pKey = NULL, getKey = NULL, verbose = FALSE, ...) <- value
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{mode}{Single character string. If \code{"a"} (default), the data are 
appened to the table (added after the last row), and \code{sRow} 
is ignored. If \code{"u"}, the data are updated according to some 
critearia in \code{sRow} (that can't be NULL). If \code{"o"}, 
the table is overwritten and \code{sRow} is ignored. }
  \item{pKey}{Single character string (if mode == "u") or NULL. Column name that 
is PRIMARY KEY in the table.}
  \item{getKey}{Single logical. If TRUE, the latest attributed primary keys will be 
retrieved.}
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbWrite")}}
  \item{value}{data.frame. Data to be writen in \code{tableName}. If the table 
has a PRIMARY KEY, and if it is AUTOINCREMENT, then the column 
can be omitted, and the attributed ID's will be retrieved if 
\code{getKey = TRUE} (not the default). If \code{sRow} is not 
NULL, then data must contain the column names given in \code{sRow}.}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link{edb}}, \code{\link{edbWrite.RODBC_Access}}, 
\code{\link{edbRead.RODBC_Access}}.}

