\name{edbDataSource.RODBC_MySQL}
\alias{edbDataSource.RODBC_MySQL}
\title{Create an RODBC MySQL data source (from an 'edb' reference). Only for MS Windows RODBC databases.}
\description{Create an RODBC MySQL data source (from an 'edb' reference). 
Only for MS Windows RODBC databases. Generic function that call 
class-specific method corresponding to the class of the 
\code{edb} object provided. Notice that the exectutable 
odbcconf.exe should be on Windows path, otherwise the command 
will fail.}
\usage{
\method{edbDataSource}{RODBC_MySQL}(edb, trash = FALSE, mysqlOdbcDriver = "MySQL ODBC 5.1 Driver", verbose = FALSE, ...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{trash}{Single logical. If TRUE, then a dummy ODBC data source will 
be created}
  \item{mysqlOdbcDriver}{Single character string. Name of the MySQL ODBC Driver, as 
displayed in the ODBC data source management GUI.}
  \item{verbose}{Single logical. Print the command passed to odbcconf.exe?}
  \item{\dots}{Additional parameters to be passed to \code{system2}.}
}

\value{Returns the output of \code{system2}.}

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link{edbDataSource}}, \code{\link{edb}}.}

