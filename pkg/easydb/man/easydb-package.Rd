\name{easydb-package}
\alias{easydb-package}
\docType{package}
\title{Generic S3 classes for simple database interfaces.}
\description{Generic S3 classes for simple database interfaces. (presently RODBC and RSQLite). 'easydb' provide common S3 classes for reading, writing, listing and subsetting tables in RODBC and (R)SQLite databases. Functions in 'easydb' alone DO NOT provide any database interface, but they are used in other packages that provide inteface to various database systems (see suggests). 'easydb' allows to retrieve, write and subset database's tables in a similar way as data.frame's, using single square brackets db[,,,]. easydb internalise many database operations (otherwise tedious) such as rows and columns subsetting (no SQL needed) and connection and disconnection to / from databases.}
\details{
\tabular{ll}{Package: \tab easydb\cr
Version: \tab 0.7.0\cr
Date: \tab 2012-08-02\cr
Title: \tab Generic S3 classes for simple database interfaces.\cr
Author: \tab Julien MOEYS <Julien.Moeys@mark.slu.se>\cr
Maintainer: \tab Julien MOEYS <Julien.Moeys@mark.slu.se>\cr
Depends: \tab R (>= 2.15.1)\cr
Suggests: \tab easyrsqlite, easyrodbcexcel, easyrodbcmysql\cr
License: \tab AGPL-3\cr
URL: \tab http://easydb.r-forge.r-project.org/\cr}
}
\author{Julien MOEYS <Julien.Moeys@mark.slu.se>}

\keyword{ package }


