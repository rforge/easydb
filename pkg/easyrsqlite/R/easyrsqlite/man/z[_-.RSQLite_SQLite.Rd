\name{[<-.RSQLite_SQLite}
\alias{[<-.RSQLite_SQLite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
[<-.RSQLite_SQLite(edb, tableName, mode = "a", pKey = NULL, getKey = NULL, formatCol = NULL, posixFormat = "", dateFormat = "", parano = TRUE, testFiles = TRUE, verbose = FALSE, ..., value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{edb}{
%%     ~~Describe \code{edb} here~~
}
  \item{tableName}{
%%     ~~Describe \code{tableName} here~~
}
  \item{mode}{
%%     ~~Describe \code{mode} here~~
}
  \item{pKey}{
%%     ~~Describe \code{pKey} here~~
}
  \item{getKey}{
%%     ~~Describe \code{getKey} here~~
}
  \item{formatCol}{
%%     ~~Describe \code{formatCol} here~~
}
  \item{posixFormat}{
%%     ~~Describe \code{posixFormat} here~~
}
  \item{dateFormat}{
%%     ~~Describe \code{dateFormat} here~~
}
  \item{parano}{
%%     ~~Describe \code{parano} here~~
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
  \item{value}{
%%     ~~Describe \code{value} here~~
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
function (edb, tableName, mode = "a", pKey = NULL, getKey = NULL, 
    formatCol = NULL, posixFormat = "", dateFormat = "", parano = TRUE, 
    testFiles = TRUE, verbose = FALSE, ..., value) 
{
    if (!is.null(getKey)) {
        stop("'getKey' must be NULL to use '[<-' methods.")
    }
    if (missing(edb)) {
        stop("Argument 'edb' is missing, with no default")
    }
    if (missing(tableName)) {
        stop("Argument 'tableName' is missing, with no default")
    }
    res <- edbWrite.RSQLite_SQLite(edb = edb, tableName = tableName, 
        data = value, mode = mode, pKey = pKey, getKey = getKey, 
        testFiles = testFiles, verbose = verbose, formatCol = formatCol, 
        posixFormat = posixFormat, dateFormat = dateFormat, parano = parano, 
        ...)
    return(edb)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
