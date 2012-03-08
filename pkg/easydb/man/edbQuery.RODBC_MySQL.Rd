\name{edbQuery.RODBC_MySQL}
\alias{edbQuery.RODBC_MySQL}
\title{Send and retrieve a query in a SQLite database (referenced by 'edb').}
\description{Send and retrieve a query in a SQLIte database (referenced by 'edb'). }
\usage{
\method{edbQuery}{RODBC_MySQL}(edb,
statement,
formatCol = NULL,
verbose = FALSE,
...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{statement}{Single character string. SQL statement / SQL query to be passed 
to \code{link[RODBC]{sqlQuery}}.}
  \item{formatCol}{If not NULL, a named list of functions to be applied to certain columns 
after the data has been extracted from the database. The name of each list 
item gives the column to process, and the value of each item gives the 
function that must be applied. For instance 
formatCol = list("DATE"=as.Date) will apply the function 
\link{as.Date} to the column "DATE".}
  \item{verbose}{Single logical. If TRUE, information on what is done are output 
on screen.}
  \item{\dots}{Additional parameters to be passed to \code{link[RODBC]{sqlQuery}}.}
}

\value{The function returns the requested table. }

\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{link{edb}}, \code{link[RODBC]{sqlQuery}}.}

