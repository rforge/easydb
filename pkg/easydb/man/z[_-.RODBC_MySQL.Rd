\name{[<-.RODBC_MySQL}
\alias{[<-.RODBC_MySQL}
\title{"[<-" method for MySQL databases. Write data in a MySQL table in a database (referenced by 'edb').}
\description{"[<-" method for MySQL databases. Write data in a table in a 
MySQL database (referenced by 'edb'). }
\usage{
\method{[}{RODBC_MySQL}(edb,
tableName,
mode = "a",
pKey = NULL,
getKey = NULL,
formatCol = NULL,
posixFormat = "",
dateFormat = "",
verbose = FALSE,
...) <- 
value
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
  \item{formatCol}{If not NULL, a named list of functions to be applied to certain columns 
before the data are written to the database. The name of each list 
item gives the column to process, and the value of each item gives the 
function that must be applied. For instance 
formatCol = list("DATE"=as.Date) will apply the function 
\link{as.Date} to the column "DATE".}
  \item{posixFormat}{Single character string. 'format' argument of the functions 
format.POSIXlt() or format.POSIXct() used to convert POSIX 
date-time into character strings when writing into the database.
Only used if getKey is not NULL or when mode == "u" in SQLite or 
MySQL.}
  \item{dateFormat}{Single character string. 'format' argument of the functions 
format.Date() used to convert "Date" 
dates into character strings when writing into the database.
Only used if getKey is not NULL or when mode == "u" in SQLite or 
MySQL.}
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



\seealso{\code{\link{edb}}, \code{\link{edbWrite.RODBC_MySQL}}, 
\code{\link{edbRead.RODBC_MySQL}}.}

