\name{edbQuery.RSQLite_SQLite}
\alias{edbQuery.RSQLite_SQLite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
edbQuery.RSQLite_SQLite(edb, statement, formatCol = NULL, testFiles = TRUE, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{edb}{
%%     ~~Describe \code{edb} here~~
}
  \item{statement}{
%%     ~~Describe \code{statement} here~~
}
  \item{formatCol}{
%%     ~~Describe \code{formatCol} here~~
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
function (edb, statement, formatCol = NULL, testFiles = TRUE, 
    verbose = FALSE, ...) 
{
    if (testFiles) {
        easydb:::.edbFileExists(edb[["dbName"]])
    }
    if (verbose) {
        cat("SQL statement:\n")
        cat(statement, sep = "\n")
    }
    msg <- sprintf(fmt = "Error detected in dbGetQuery() in edbQuery.RSQLite_SQLite() (database: \%s). Database connection closed.\n", 
        edb[["dbName"]])
    qRes <- .edbOperation.RSQLite_SQLite(edb = edb, expr = expression({
        exprOut <- dbGetQuery(conn = dbCon, ...)
    }), maxCon = 1, errorMessage = msg, statement = statement, 
        ...)
    qRes <- easydb:::.formatCol(x = qRes, formatCol = formatCol)
    return(qRes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
