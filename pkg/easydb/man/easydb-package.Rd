\name{easydb-package}
\alias{easydb-package}
\docType{package}
\title{Simplified and unified database interface for SQLite, MySQL and MS Access.}
\description{Simplified and unified database interface for SQLite, MySQL and MS Access. 'easydb' is a wrapper around RSQLite and RODBC. It provides unified functions for reading, writing, listing tables and listing column names in SQLite, MySQL and MS Access. It simplifies access to databases by internalising the connection to the database, and wrapping database operations into 'tryCatch' so that the database is properly closed if an operation fails. 'easydb' also provides a way to retrieve PRIMARY KEY values that were automatically attributed (in cases where AUTOINCREMENT is used in the table/field definition). It provides a simple mecanism to retrieve only part of the data in a table, according to some row constrains (translated internally into SQL queries). Nonetheless 'easydb' does not intend to replace all possible usage of RSQLite or RODBC, and is not optimised for speed.}
\details{
\tabular{ll}{Package: \tab easydb\cr
Version: \tab 0.3.1\cr
Date: \tab 2011-08-18\cr
Title: \tab Simplified and unified database interface for SQLite, MySQL and MS Access.\cr
Author: \tab Julien MOEYS <Julien.Moeys@mark.slu.se>\cr
Maintainer: \tab Julien MOEYS <Julien.Moeys@mark.slu.se>\cr
Depends: \tab R (>= 2.10.0)\cr
Suggests: \tab RODBC, RSQLite\cr
LazyLoad: \tab no\cr
License: \tab AGPL-3\cr
URL: \tab http://www.mark.slu.se, http://ckb.slu.se, http://julienmoeys.free.fr\cr}
}
\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}

\keyword{ package }


