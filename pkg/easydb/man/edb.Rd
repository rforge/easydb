\name{edb}
\alias{edb}
\title{Create a database description (class 'edb'), to be used by other functions.}
\description{Create a database description (class 'edb'), to be used by 
other functions. Notice that this is _not_ a database connection, 
so the function won't give a warning if the database does not 
exists (yet) or if some parameters are wrong.}
\usage{edb(dbType, dbName, dbSourceName = "edbDefault", dbHost = "", 
    dbLogin = "", dbPwd = "", dbPort = integer(0), ...)}
\arguments{
  \item{dbType}{Single character string. Type of the database to describe, 
formatted as \code{RPackageName_DatabaseType}. Possible values 
are \code{RSQLite_SQLite}, \code{RODBC_Access} or 
\code{RODBC_MySQL}. }
  \item{dbName}{Single character string. Name of the database to describe 
(should include the path to the database if it is not in the 
working directory. MySQL database have no path of course). 
Not to be confused with the data source name below.}
  \item{dbSourceName}{Single character string. Name of the data source (only ODBC 
databases) for this database.}
  \item{dbHost}{Single character string. The IP address of the remote database 
server (only for RODBC_MySQL).}
  \item{dbLogin}{Single character string. Database login (or user name). Only 
for certain dbType (only for RODBC_MySQL). }
  \item{dbPwd}{Single character string. Database password (eventually user 
specific). Only for certain dbType (only for RODBC_MySQL). }
  \item{dbPort}{Single integer. The connexion port to the database (only for 
RODBC_MySQL). }
  \item{\dots}{Additional named arguments, to be passed to the function used 
to connect to the database (internally).}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link{edbRead}}, \code{\link{edbWrite}}, 
\code{\link{edbNames}}, \code{\link{edbColnames}}.}

