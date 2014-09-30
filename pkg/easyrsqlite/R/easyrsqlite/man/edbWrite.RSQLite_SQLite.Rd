\name{edbWrite.RSQLite_SQLite}
\alias{edbWrite.RSQLite_SQLite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
edbWrite.RSQLite_SQLite(edb, tableName, data, mode = c("a", "u", "o")[1], pKey = NULL, getKey = NULL, formatCol = NULL, posixFormat = "", dateFormat = "", logOp = FALSE, logRandId = rnorm(1), logMsg = as.character(NA), logTableName = "edbLog", logCreateTableIfNotExist = TRUE, parano = TRUE, testFiles = TRUE, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{edb}{
%%     ~~Describe \code{edb} here~~
}
  \item{tableName}{
%%     ~~Describe \code{tableName} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (edb, tableName, data, mode = c("a", "u", "o")[1], pKey = NULL, 
    getKey = NULL, formatCol = NULL, posixFormat = "", dateFormat = "", 
    logOp = FALSE, logRandId = rnorm(1), logMsg = as.character(NA), 
    logTableName = "edbLog", logCreateTableIfNotExist = TRUE, 
    parano = TRUE, testFiles = TRUE, verbose = FALSE, ...) 
{
    if (testFiles) {
        easydb:::.edbFileExists(edb[["dbName"]])
    }
    last.warning.exist <- exists("last.warning", envir = baseenv())
    if (last.warning.exist) {
        old.warn <- get("last.warning", envir = baseenv())
        assign(x = "last.warning", value = list(), envir = baseenv())
    }
    else {
        old.warn <- list()
    }
    data <- easydb:::.formatCol(x = data, formatCol = formatCol)
    if (mode != "u") {
        if (mode == "a") {
            append <- TRUE
        }
        else {
            if (mode == "o") {
                append <- FALSE
            }
            else {
                stop("'mode' must be either 'u' or 'a' or 'o'.")
            }
        }
        if (is.null(getKey)) {
            if (parano & (mode != "o")) {
                colNamez <- edbColnames(edb = edb, tableName = tableName)
                testCol1 <- colnames(data) \%in\% colNamez
                if (!all(testCol1)) {
                  stop(paste(sep = "", "Some columns in input 'data' could not be found in the table '", 
                    tableName, "' (", paste(colnames(data)[!testCol1], 
                      collapse = ", "), ")."))
                }
                testCol2 <- colNamez \%in\% colnames(data)
                if (!all(testCol2)) {
                  stop(paste(sep = "", "Some columns in the table '", 
                    tableName, "' could not be found in input 'data' (", 
                    paste(colNamez[!testCol2], collapse = ", "), 
                    ")."))
                }
                data <- data[, colNamez]
            }
            msg <- sprintf(fmt = "Error detected in dbWriteTable() in edbWrite.RSQLite_SQLite() (database: \%s; table: \%s). Database connection closed.\n", 
                edb[["dbName"]], tableName)
            oldOptions <- getOption("warn")
            options(warn = max(c(1, oldOptions)))
            res <- .edbOperation.RSQLite_SQLite(edb = edb, expr = expression({
                exprOut <- dbWriteTable(conn = dbCon, ...)
            }), maxCon = 1, errorMessage = msg, name = tableName, 
                value = data, row.names = FALSE, overwrite = !append, 
                append = append, ...)
            options(warn = oldOptions)
        }
        else {
            data <- easydb:::.formatTable4Query(data = data, 
                del = "\"", posixFormat = posixFormat, dateFormat = dateFormat)
            dataCol <- colnames(data)
            oldOptions <- options("warn")[[1]]
            options(warn = 1)
            newId <- lapply(X = 1:nrow(data), FUN = function(X) {
                sqlUpdate <- paste(sep = "", "INSERT INTO [", 
                  tableName, "]\n", "(", paste("[", dataCol, 
                    "]", collapse = ",", sep = ""), ")\n", "VALUES(", 
                  paste(as.character(data[X, ]), collapse = ","), 
                  ");\n")
                if (verbose) {
                  cat(sqlUpdate)
                }
                sqlUpdate2 <- paste(sep = "", "SELECT ", getKey, 
                  " FROM [", tableName, "] WHERE ", getKey, " = last_insert_rowid();\n")
                if (verbose) {
                  cat(sqlUpdate2)
                }
                msg <- sprintf(fmt = "Error detected in .edbSendGetQuery.RSQLite_SQLite() in edbWrite.RSQLite_SQLite() (database: \%s; table: \%s; row: \%s). Database connection closed.\n", 
                  edb[["dbName"]], tableName, as.character(X))
                newId <- .edbOperation.RSQLite_SQLite(edb = edb, 
                  expr = expression({
                    exprOut <- .edbSendGetQuery.RSQLite_SQLite(conn = dbCon, 
                      ...)
                  }), maxCon = 1, errorMessage = msg, statement = c(sqlUpdate, 
                    sqlUpdate2), ...)
                msg <- sprintf(fmt = "Error detected in dbGetQuery() in edbWrite.RSQLite_SQLite() (database: \%s; table: \%s; row: \%s). Database connection closed.\n", 
                  edb[["dbName"]], tableName, as.character(X))
                return(newId)
            })
            newId <- as.numeric(unlist(newId))
        }
    }
    else {
        data <- easydb:::.formatTable4Query(data = data, del = "\"", 
            posixFormat = posixFormat, dateFormat = dateFormat)
        dataCol <- colnames(data)
        if (is.null(pKey)) {
            stop("When mode = 'u', pKey must be a non-null character string.")
        }
        if (pKey \%in\% colnames(data)) {
            const <- paste("WHERE ([", pKey, "] = ", data[, pKey], 
                ")", sep = "")
        }
        else {
            stop("When 'pKey' is not NULL, 'pKey' must be provided in 'data' (column names).")
        }
        data <- data[, dataCol, drop = FALSE]
        oldOptions <- options("warn")[[1]]
        options(warn = 1)
        res <- lapply(X = 1:nrow(data), FUN = function(X) {
            sqlUpdate <- paste(sep = "", "UPDATE [", tableName, 
                "]\n", "SET ", paste("[", dataCol, "]", " = ", 
                  data[X, ], collapse = ", ", sep = ""), "\n", 
                const[X], ";\n\n")
            if (verbose) {
                cat(sqlUpdate)
            }
            msg <- sprintf(fmt = "Error detected in dbGetQuery() in edbWrite.RSQLite_SQLite() (database: \%s; table: \%s; row: \%s). Database connection closed.\n", 
                edb[["dbName"]], tableName, as.character(X))
            res <- .edbOperation.RSQLite_SQLite(edb = edb, expr = expression({
                exprOut <- dbGetQuery(conn = dbCon, ...)
            }), maxCon = 1, errorMessage = msg, statement = sqlUpdate, 
                ...)
            return(res)
        })
        res <- unlist(res)
        options(warn = oldOptions)
    }
    last.warning.exist2 <- exists("last.warning", envir = baseenv())
    if (last.warning.exist2) {
        last.warning2 <- get("last.warning", envir = baseenv())
        if (length(last.warning2) != 0) {
            assign(x = "last.warning", value = c(old.warn, last.warning2), 
                envir = baseenv())
            stop("Warning(s) detected in SQLite transaction. type warnings() to see it/them.")
        }
    }
    else {
        if (last.warning.exist) {
            assign(x = "last.warning", value = old.warn, envir = baseenv())
        }
    }
    if (logOp) {
        tmp <- edbLog(edb = edb, tableName = tableName, fun = "edbWrite.RSQLite_SQLite", 
            date = date(), R.version = R.version.string, nodename = Sys.info()[["nodename"]], 
            edbVersion = NULL, mode = mode, getKey = getKey, 
            logRandId = logRandId, logMsg = logMsg, logTableName = logTableName, 
            logCreateTableIfNotExist = TRUE)
    }
    if (exists("newId")) {
        return(newId)
    }
    else {
        return(res)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
