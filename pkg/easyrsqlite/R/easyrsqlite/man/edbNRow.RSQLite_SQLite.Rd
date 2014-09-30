\name{edbNRow.RSQLite_SQLite}
\alias{edbNRow.RSQLite_SQLite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
edbNRow.RSQLite_SQLite(edb, tableName, sRow = NULL, sRowOp = c("AND", "OR")[1], verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{edb}{
%%     ~~Describe \code{edb} here~~
}
  \item{tableName}{
%%     ~~Describe \code{tableName} here~~
}
  \item{sRow}{
%%     ~~Describe \code{sRow} here~~
}
  \item{sRowOp}{
%%     ~~Describe \code{sRowOp} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (edb, tableName, sRow = NULL, sRowOp = c("AND", "OR")[1], 
    verbose = FALSE, ...) 
{
    statement <- paste("SELECT Count(*) FROM [", tableName, "]", 
        sep = "")
    sRow <- easydb:::.edb.sRow(sRow = sRow, sRowOp = sRowOp, 
        charQ = "\"", colQ = c("[", "]"))
    statement <- paste(statement, sRow, "\n", sep = " ")
    if (verbose) {
        cat(statement)
    }
    nbrow <- edbQuery(edb = edb, statement = statement, ...)[, 
        ]
    return(nbrow)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
