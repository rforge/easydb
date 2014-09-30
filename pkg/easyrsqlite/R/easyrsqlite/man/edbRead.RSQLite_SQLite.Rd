\name{edbRead.RSQLite_SQLite}
\alias{edbRead.RSQLite_SQLite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
edbRead.RSQLite_SQLite(edb, tableName, sRow = NULL, sCol = NULL, sRowOp = c("AND", "OR")[1], formatCol = NULL, distinct = FALSE, orderBy = NULL, testFiles = TRUE, verbose = FALSE, ...)
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
    if (testFiles) {
        easydb:::.edbFileExists(edb[["dbName"]])
    }
    selectWhat <- easydb:::.edb.sCol(edb = edb, sCol = sCol, 
        tableName = tableName, colQ = c("[", "]"))
    sCol <- selectWhat[["sCol"]]
    selectWhat <- selectWhat[["selectWhat"]]
    sRow <- easydb:::.edb.sRow(sRow = sRow, sRowOp = sRowOp, 
        charQ = "\"", colQ = c("[", "]"))
    distinct <- ifelse(distinct, "DISTINCT ", "")
    if (!is.null(orderBy)) {
        orderBy <- paste(sep = "", "\nORDER BY ", paste(collapse = ", ", 
            orderBy), "\n")
    }
    else {
        orderBy <- ""
    }
    statement <- paste(sep = "", "SELECT ", distinct, selectWhat, 
        "\n", "FROM [", tableName, "]\n", sRow, orderBy, ";\n")
    if (verbose) {
        cat("SQL statement:\n")
        cat(statement, sep = "\n")
    }
    msg <- sprintf(fmt = "Error detected in dbGetQuery() in edbRead.RSQLite_SQLite() (database: \%s; table: \%s). Database connection closed.\n", 
        edb[["dbName"]], tableName)
    tbl <- .edbOperation.RSQLite_SQLite(edb = edb, expr = expression({
        exprOut <- dbGetQuery(conn = dbCon, ...)
    }), maxCon = 1, errorMessage = msg, statement = statement, 
        ...)
    tbl <- easydb:::.formatCol(x = tbl, formatCol = formatCol)
    return(tbl)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
