# library( "testthat" ); test_file( "C:/_R_PACKAGES/easydb/testthat/easydb_testthat.R" ) 
# https://github.com/hadley/devtools/wiki/Testing 

library( "easydb" ) 
library( "RODBC" ) 
library( "easyrodbcmysql" ) 
library( "testthat" ) 






context("1. Testing easydb for MySQL databases") 



# Load password and host name (only available on local
# computer) -- replace this by your own password and 
# host IP.
ref <- dget( file = 
    "C:/_R_PACKAGES/easydb/testthat_ref/testthat_ref.txt" ) 
names( ref ); class( ref ) 
# [1] "dbPwd"  "dbHost"
# [1] "list"



### Describe the database (NB: this is not a connection)
myDb <- edb( 
    dbType       = "RODBC_MySQL", 
    dbSourceName = "edb_test_rodbc", # or any name you like
    dbName       = "edb_test", 
    dbLogin      = "edbtestuser", 
    dbPwd        = ref[["dbPwd"]], 
    dbHost       = ref[["dbHost"]], 
    dbPort       = 3306 
)   #    



### Windows only:
if( Sys.info()[[ "sysname" ]] == "Windows" ){ 
    #
    context("1. 1. MySQL for MS Windows") 
    #
    test_that("Register the data source in ODBC", {
        res <- edbDataSource( myDb, verbose = TRUE ) 
        #
        expect_that( res, equals(0) )
    } ) #
    #
    # TEST edbNames()
    #
    test_that("List the table names in the database", {
        res <- edbNames( edb = myDb, onlyNames = FALSE ) 
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) )
    } ) #
    #
    test_that("List the table names in the database (2)", {
        res <- edbNames( edb = myDb ) 
        #
        expect_that( class(res), equals("character") ) 
        expect_that( length(res) > 1, equals(TRUE) )
    } ) #
    #
    # TEST edbColnames()
    #
    test_that("List the column names in a table", {
        res <- edbColnames( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
        #
        expect_that( class(res), equals("character") ) 
        expect_that( length(res) > 1, equals(TRUE) )
    } ) #
    #
    test_that("List the column names in a table (2)", {
        res <- edbColnames( 
            edb       = myDb, 
            tableName = "WRB_SOIL_GROUP", 
            onlyNames = FALSE # different too!
        )   #
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) )
    } ) #
    #
    # TEST edbRead()
    #
    test_that("Retrieve a table (data.frame style subsetting)", {
        res <- myDb[ "WRB_SOIL_GROUP" ] 
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) )
    } ) #
    #
    test_that("Retrieve a table with edbRead()", {
        res <- edbRead( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) )
    } ) #
    #
    test_that("Retrieve part of a table (with row constrains)", {
        res <- myDb[ "WRB_SOIL_GROUP", list("ABBREV" = c("AC","CR","PL")) ] 
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) )
    } ) #
    #
    test_that("Retrieve part of a table (with row constrains), with edbRead()", {
        res <- edbRead( 
            edb       = myDb, 
            tableName = "WRB_SOIL_GROUP", 
            sRow      = list("ABBREV" = c("AC","CR","PL")) 
        )   #
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) )
    } ) #
    #
    test_that("Retrieve part of a table (row constrains + select only some columns)", {
        res <- myDb[ 
            "WRB_SOIL_GROUP", 
            list("ABBREV" = c("AC","CR","PL")), 
            c("ID_WRB_SOIL_GROUP","NAME") 
        ]   #
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) ) 
        expect_that( ncol(res), equals(2) )
    } ) #
    #
    test_that("Retrieve part of a table (row constrains + select only some columns), with edbRead()", {
        res <- edbRead( 
            edb       = myDb, 
            tableName = "WRB_SOIL_GROUP", 
            sRow      = list("ABBREV" = c("AC","CR","PL")), 
            sCol      = c("ID_WRB_SOIL_GROUP","NAME") 
        )   #
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) ) 
        expect_that( ncol(res), equals(2) )
    } ) #
    #
    test_that("Use some SQL constrains", {
        res <- myDb[ 
            "WRB_SOIL_GROUP", 
            list( 
                "ABBREV" = c("AC","AB","AL","AN","AT"), 
                "SQL" = "NAME LIKE 'Al%'"
            )   
        ]   #
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) )
    } ) #
    #
    test_that("Multiple row constrains, alternative (constrain 1 OR constrain 2)", {
        res <- myDb[ 
            "WRB_SOIL_GROUP", 
            list( 
                "ABBREV" = c("AC","AB"), 
                "ID_WRB_SOIL_GROUP" = 25:30
            ),  
            sRowOp = "OR" 
        ]   #
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) )
    } ) #
    #
    test_that("Number of columns", {
        res <- edbNCol( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
        #
        expect_that( class(res), equals("integer") ) 
        expect_that( length(res), equals(1) )
    } ) #
    #
    test_that("Number of rows", {
        res <- edbNRow( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
        #
        expect_that( class(res), equals("integer") ) 
        expect_that( length(res), equals(1) )
    } ) #
    #
    test_that("Dimensions", {
        res <- edbDim( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
        #
        expect_that( class(res), equals("integer") ) 
        expect_that( length(res), equals(2) )
    } ) #
    #
    #  More ways to select columns:
    test_that("More ways to select columns: Indexes", {
        res <- myDb[ "WRB_SOIL_GROUP", sCol = 2:3 ] 
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) ) 
        expect_that( ncol(res), equals(2) ) 
    } ) #
    #
    test_that("More ways to select columns: Logical", {
        res <- myDb[ "WRB_SOIL_GROUP", sCol = c(FALSE,TRUE,TRUE) ] 
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( nrow(res) > 1, equals(TRUE) ) 
        expect_that( ncol(res), equals(2) ) 
    } ) #
    #
    # Function to convert POSIX integer "seconds from 1970-01-01" into 
    # R POSIXct date format.
    formatDT <- function( x, tz = "GMT" ){ 
        res <- ISOdatetime( year = 1970, month = 1, day = 1, 
            hour = 0, min = 0, sec = 0, tz = tz ) 
        res <- res + x 
        return( res ) } 
            
    
    # Function to convert integer "days from 1970-01-01" into 
    # R Date format.
    formatD <- function( x, tz = "GMT" ){ 
        res <- ISOdate( year = 1970, month = 1, day = 1, tz = tz ) 
        res <- res + (x * 24 * 60 * 60 ) 
        res <- as.Date( res ) 
        return( res ) } 
    
    # Now we can convert the columns on-the-fly
    test_that("On the fly columns transformation", {
        res <- myDb[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = formatDT, 
            "DAT_DAY" = formatD, "TEST_BOOL" = as.logical ) ] 
        #
        expect_that( class(res), equals("data.frame") ) 
        expect_that( class(res[,"DAT_TIM_SEC"]), equals(c("POSIXct","POSIXt")) ) 
        expect_that( class(res[,"DAT_DAY"]), equals("Date") ) 
        expect_that( class(res[,"TEST_BOOL"]), equals("logical") ) 
    } ) #
    #
    # SQL SELECT DISTINCT, equivalent of R unique().
    # To select unique / distinct values in a (group of) columns, 
    # set 'distinct' to TRUE:
    test_that("Test SQL Distinct (equivalent of R unique):", {
        res  <- myDb[ "PROFILE", sCol = "COMMENTS", distinct = TRUE ] 
        res2 <- myDb[ "PROFILE", sCol = "COMMENTS" ] 
        #
        expect_that( class(res), equals("data.frame") ) 
        #
        res  <- res[,1] 
        res2 <- unique( res2[,1] ) 
        #
        expect_that( identical(res,res2), equals(TRUE) ) 
    } ) #
    #
    # SQL ORDER BY, equivalent of R order(). 
    # To sort the table according to some columns (ascending or 
    # descending), use 'orderBy':
    test_that("Test SQL ORDER BY, equivalent of R order():", {
        res  <- myDb[ "WRB_SOIL_GROUP", orderBy = "ABBREV DESC" ] 
        res2 <- myDb[ "WRB_SOIL_GROUP" ] 
        #
        expect_that( class(res), equals("data.frame") ) 
        #
        res2 <- res2[order(res2[,"ABBREV"],decreasing = TRUE),] 
        #
        rownames(res)  <- NULL 
        rownames(res2) <- NULL 
        #
        expect_that( identical(res,res2), equals(TRUE) ) 
    } ) #
    #
    # TEST edbWrite() -------------------------------------------
    #
    # First retrieve the table profile
    profileTbl <- myDb[ "PROFILE" ] 
    #
    # Change the ID's (pseudo new profiles)
    profileTbl[, "ID_PROFILE" ] <- 3:4 
    #
    # Write the 'new' data in the database:
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE", 
        data      = profileTbl, 
        mode      = "a" # append
    )   #
    #
    test_that("Check edbWrite has worked (1)", {
        profileTbl2 <- myDb[ "PROFILE" ] 
        #
        expect_that( nrow(profileTbl2) == (nrow(profileTbl) * 2), equals(TRUE) )
    } ) #
    #
    # Alternative method:
    profileTbl[, "ID_PROFILE" ] <- 5:6 # Change IDs.
    myDb[ "PROFILE", mode = "a" ] <- profileTbl 
    #
    test_that("Check edbWrite has worked (2)", {
        profileTbl2 <- myDb[ "PROFILE" ] 
        #
        expect_that( nrow(profileTbl2) == (nrow(profileTbl) * 3), equals(TRUE) )
    } ) #
    #
    test_that("Check edbWrite does not work in some case", {
        expect_that( 
            { edbWrite( 
                edb       = myDb, 
                tableName = "PROFILE", 
                data      = profileTbl[, -1 ], 
                mode      = "a"  
            ) }, 
            throws_error() 
        )   #
    } ) #
    #
    test_that("Check edbWrite with getKey", {
        idBefore <- myDb[ "PROFILE", , sCol = "ID_PROFILE" ] 
        #
        newId <- edbWrite( 
            edb       = myDb, 
            tableName = "PROFILE", 
            data      = profileTbl[, -1 ], 
            mode      = "a", 
            #verbose   = TRUE,
            getKey    = "ID_PROFILE" 
        )   #
        #
        expect_that( length(newId), equals(2) ) 
        expect_that( all(newId %in% idBefore), equals(FALSE) ) 
    } ) #
    #
    test_that("Create a new table", { 
        tblBefore <- edbNames( edb = myDb ) 
        #
        edbWrite( 
            edb       = myDb, 
            tableName = "PROFILE2", 
            data      = profileTbl, 
            mode      = "o", 
        )   #
        #
        tblAfter <- edbNames( edb = myDb ) 
        #
        expect_that( "PROFILE2" %in% tblBefore, equals(FALSE) ) 
        expect_that( "PROFILE2" %in% tblAfter, equals(TRUE) ) 
    } ) #
    #
    test_that("Create a new table (2)", { 
        tblBefore <- edbNames( edb = myDb ) 
        #
        myDb[ "PROFILE3", mode = "o" ] <- profileTbl 
        #
        tblAfter <- edbNames( edb = myDb ) 
        #
        expect_that( "PROFILE3" %in% tblBefore, equals(FALSE) ) 
        expect_that( "PROFILE3" %in% tblAfter, equals(TRUE) ) 
    } ) #
    #
    test_that("Test edbDelete()", { 
        profBefore <- myDb[ "PROFILE" ] 
        #
        edbDelete( 
            edb       = myDb, 
            tableName = "PROFILE", 
            sRow      = list("SQL" = "ID_PROFILE > 2")
        )   #
        #
        profAfter <- myDb[ "PROFILE" ] 
        #
        expect_that( nrow(profBefore) > nrow(profAfter), equals(TRUE) ) 
    } ) #
    #
    test_that("Test edbDrop()", { 
        edbDrop( edb = myDb, tableName = "PROFILE2" ) 
        edbDrop( edb = myDb, tableName = "PROFILE3" ) 
        #
        tblAfter <- edbNames( edb = myDb, onlyNames = FALSE ) 
        #
        expect_that( any( c("PROFILE2","PROFILE3") %in% tblAfter ), equals(FALSE) ) 
    } ) #
    #
    # WORK ON PROGRESS
    #
    # It may be useful to transform some columns 'on-the-fly', before 
    # they are written to the database. In the example below we 
    # have some dates and times values, as well as some boolean stored 
    # as integers (seconds or days since 1970-01-01 or 0/1 values, 
    # respectively). We want to transform them when writing in the database.
    #
    
    #
    # It is possible to write an operation "log" every time edbWrite() 
    # is used (or edbDelete() or edbDrop()). The exact operation is 
    # not logged, but rather the function name, the table concerned, 
    # the version of R and easydb, the date, an eventual log message, 
    # etc. Set the argument 'logOp' to TRUE to log operations:
    #
    
    #
    test_that("Un-register the data source in ODBC", {
        res <- edbDataSource( myDb, trash = TRUE ) 
        #
        expect_that( res, equals(0) )
    } ) #
    #
}   #



