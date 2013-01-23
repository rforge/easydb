\name{edbDim}
\alias{edbDim}
\title{Retrieve the dimension of a table in a database (referenced by 'edb').}
\description{Retrieve the dimension of a table in a database (referenced by 
'edb'). Notice that the methods do NOT retrieve the full table to 
get its column names (so it should work even if the table is big).}
\usage{edbDim(edb, tableName, ...)}
\arguments{
  \item{edb}{An object of class 'edb', such as returned by \code{\link{edb}}.}
  \item{tableName}{Single character string. Name of the table to read in 'edb'.}
  \item{\dots}{Additional parameters to be passed to class-specific method. See 
\code{methods("edbColnames")} and \code{methods("edbNRow")}.}
}



\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}



\seealso{\code{\link{edb}}, \code{\link{edbColnames}}, 
\code{\link{edbRead}}.}
\examples{# See edbRead for an example.}
