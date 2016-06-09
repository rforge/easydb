\name{edbLog}
\alias{edbLog}
\title{Write an operation "log" (used when modifying the database)}
\description{Write an operation "log" (to be used when modifying the 
database). An operation log consists in information about (a) the function 
used, (b) the table modified, (c) the operation date, (d) R version, (e) 
easydb version, (f) the option useds ('mode' and 'getKey' if used with 
edbWrite()). This is not version control, and this is not comprehensive 
operation log.}
\usage{edbLog(edb, tableName = as.character(NA), fun = as.character(NA), 
    date = date(), R.version = R.version.string, nodename = Sys.info()[["nodename"]], 
    edbVersion = NULL, mode = as.character(NA), getKey = as.character(NA), 
    logRandId = rnorm(1), logMsg = as.character(NA), 
    logTableName = "edbLog", logCreateTableIfNotExist = TRUE)}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.
Database in which the log has to be written.}
  \item{tableName}{Single character string. The name of the table that was modified.
Not to be confused with 'logTableName'.}
  \item{fun}{Single character string. The name of the function that did the modification. }
  \item{date}{Single character string. The date and time of the operation.}
  \item{R.version}{Single character string. R version}
  \item{nodename}{Single character string. Name of the computer (node). }
  \item{edbVersion}{Single character string. Version of easydb. If NULL, the version is 
fetched using 'installed.packages()'.}
  \item{mode}{Single character string. Value of the argument 'mode', if fun is 
edbWrite().}
  \item{getKey}{Single character string. Value of the argument 'getKey', if fun is 
edbWrite().}
  \item{logRandId}{Single numerical. Some number, hopefully unique, that can be used 
to identify the log record, as a complement to the log date.}
  \item{logMsg}{Single character string. Aditional log message to be written in the 
log table (same record as the rest).}
  \item{logTableName}{Single character string. Name of the log table. If that table does not 
exist, it will be created.}
  \item{logCreateTableIfNotExist}{Single logical. If TRUE (the default), then the table is created if it 
does not exist yet. Please notice that this option is not multi-thread 
safe.}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link{edb}}, \code{\link{edbWrite}}, \code{\link{edbDelete}}, 
\code{\link{edbDrop}}.}

