\name{edbDataSource}
\alias{edbDataSource}
\title{Create an RODBC data source (from an 'edb' reference). Only for MS Windows RODBC databases.}
\description{Create an RODBC data source (from an 'edb' reference). Only 
for MS Windows RODBC databases. Generic function that call 
class-specific method corresponding to the class of the 
\code{edb} object provided.}
\usage{
edbDataSource(edb,
...)
}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbDataSource")}}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link{edbDataSource.RODBC_MySQL}}, \code{\link{edb}}.}

