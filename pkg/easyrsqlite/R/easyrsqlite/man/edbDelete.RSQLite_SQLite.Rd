\name{edbDelete.RSQLite_SQLite}
\alias{edbDelete.RSQLite_SQLite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
edbDelete.RSQLite_SQLite(edb, tableName, sRow = NULL, sRowOp = c("AND", "OR")[1], logOp = FALSE, logRandId = rnorm(1), logMsg = as.character(NA), logTableName = "edbLog", logCreateTableIfNotExist = TRUE, testFiles = TRUE, verbose = FALSE, ...)
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
  \item{logOp}{
%%     ~~Describe \code{logOp} here~~
}
  \item{logRandId}{
%%     ~~Describe \code{logRandId} here~~
}
  \item{logMsg}{
%%     ~~Describe \code{logMsg} here~~
}
  \item{logTableName}{
%%     ~~Describe \code{logTableName} here~~
}
  \item{logCreateTableIfNotExist}{
%%     ~~Describe \code{logCreateTableIfNotExist} here~~
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
function (edb, tableName, sRow = NULL, sRowOp = c("AND", "OR")[1], 
    logOp = FALSE, logRandId = rnorm(1), logMsg = as.character(NA), 
    logTableName = "edbLog", logCreateTableIfNotExist = TRUE, 
    testFiles = TRUE, verbose = FALSE, ...) 
{
    if (testFiles) {
        easydb:::.edbFileExists(edb[["dbName"]])
    }
    sRow <- easydb:::.edb.sRow(sRow = sRow, sRowOp = sRowOp, 
        charQ = "\"", colQ = c("[", "]"))
    statement <- paste(sep = "", "DELETE FROM [", tableName, 
        "]\n", sRow, "\n", ";\n")
    if (verbose) {
        cat("SQL statement:\n")
        cat(statement, sep = "\n")
    }
    msg <- sprintf(fmt = "Error detected in dbGetQuery() in edbRead.RSQLite_SQLite() (database: \%s; table: \%s). Database connection closed.\n", 
        edb[["dbName"]], tableName)
    out <- .edbOperation.RSQLite_SQLite(edb = edb, expr = expression({
        exprOut <- dbGetQuery(conn = dbCon, ...)
    }), maxCon = 1, errorMessage = msg, statement = statement, 
        ...)
    if (logOp) {
        tmp <- edbLog(edb = edb, tableName = tableName, fun = "edbDelete.RSQLite_SQLite", 
            date = date(), R.version = R.version.string, nodename = Sys.info()[["nodename"]], 
            edbVersion = NULL, mode = as.character(NA), getKey = as.character(NA), 
            logRandId = logRandId, logMsg = logMsg, logTableName = logTableName, 
            logCreateTableIfNotExist = TRUE)
    }
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
