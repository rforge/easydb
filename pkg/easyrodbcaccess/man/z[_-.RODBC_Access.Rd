\name{[<-.RODBC_Access}
\alias{[<-.RODBC_Access}
\title{"[<-" method for MS Access databases. Write data in a MS Access table in a database (referenced by 'edb').}
\description{"[<-" method for MS Access databases. Write data in a table in a 
MS Access database (referenced by 'edb'). }
\usage{

\method{[}{RODBC_Access}(edb, tableName, mode = "a", pKey = NULL, getKey = NULL, formatCol = NULL, posixFormat = "", dateFormat = "", parano = TRUE, verbose = FALSE, ...) <- value

}

\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link[easydb]{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{mode}{Single character string. If \code{"a"} (default), the data are 
appened to the table (added after the last row), and \code{sRow} 
is ignored. If \code{"u"}, the data are updated according to some 
critearia in \code{sRow} (that can't be NULL). If \code{"o"}, 
the table is overwritten and \code{sRow} is ignored. }
  \item{pKey}{Single character string (if mode == "u") or NULL. Column name that 
is PRIMARY KEY in the table.}
  \item{getKey}{Single character string or NULL. If non NULL, name of the PRIMARY 
KEY whose latest attributed values should be retrieved.}
  \item{formatCol}{If not NULL, a named list of functions to be applied to certain columns 
before the data are written to the database. The name of each list 
item gives the column to process, and the value of each item gives the 
function that must be applied. For instance 
formatCol = list("DATE"=as.Date) will apply the function 
\link[base]{as.Date} to the column "DATE".}
  \item{posixFormat}{Single character string. 'format' argument of the functions 
format.POSIXlt() or format.POSIXct() used to convert POSIX 
date-time into character strings when writing into the database.
Only used if getKey is not NULL.}
  \item{dateFormat}{Single character string. 'format' argument of the functions 
format.Date() used to convert "Date" 
dates into character strings when writing into the database.
Only used if getKey is not NULL.}
  \item{parano}{Single logical. If set to TRUE (the default), the function is 
run on "paranoia mode", that is additional tests are performed 
before the data are written into the database. This slows down 
a bit (more) the function, but it may avoid some mistakes.}
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbWrite")}}
  \item{value}{data.frame. Data to be writen in \code{tableName}. If the table 
has a PRIMARY KEY, and if it is AUTOINCREMENT, then the column 
can be omitted, and the attributed ID's will be retrieved if 
\code{!is.null(getKey)} (not the default). If \code{sRow} is not 
NULL, then data must contain the column names given in \code{sRow}.}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link[easydb]{edb}}, \code{\link{edbWrite.RODBC_Access}}, 
\code{\link{edbRead.RODBC_Access}}.}

