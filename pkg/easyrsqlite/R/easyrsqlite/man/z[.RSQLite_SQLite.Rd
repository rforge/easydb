\name{[.RSQLite_SQLite}
\alias{[.RSQLite_SQLite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
[.RSQLite_SQLite(edb, tableName, sRow = NULL, sCol = NULL, sRowOp = c("AND", "OR")[1], formatCol = NULL, distinct = FALSE, orderBy = NULL, testFiles = TRUE, verbose = FALSE, ...)
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
  \item{sCol}{
%%     ~~Describe \code{sCol} here~~
}
  \item{sRowOp}{
%%     ~~Describe \code{sRowOp} here~~
}
  \item{formatCol}{
%%     ~~Describe \code{formatCol} here~~
}
  \item{distinct}{
%%     ~~Describe \code{distinct} here~~
}
  \item{orderBy}{
%%     ~~Describe \code{orderBy} here~~
}
  \item{testFiles}{
%%     ~~Describe \code{testFiles} here~~
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
function (edb, tableName, sRow = NULL, sCol = NULL, sRowOp = c("AND", 
    "OR")[1], formatCol = NULL, distinct = FALSE, orderBy = NULL, 
    testFiles = TRUE, verbose = FALSE, ...) 
{
    if (missing(edb)) {
        stop("Argument 'edb' is missing, with no default")
    }
    if (missing(tableName)) {
        stop("Argument 'tableName' is missing, with no default")
    }
    tbl <- edbRead.RSQLite_SQLite(edb = edb, tableName = tableName, 
        sRow = sRow, sCol = sCol, sRowOp = sRowOp, testFiles = testFiles, 
        verbose = verbose, formatCol = formatCol, distinct = distinct, 
        orderBy = orderBy, ...)
    return(tbl)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
