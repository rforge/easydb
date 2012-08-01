library( "easyrodbcmysql" ) 



testMySQL <- FALSE # Set to TRUE to run some tests
                   # after modifying the edb() part below 
                   # with your own database setting



### Windows only:
if( (Sys.info()[[ "sysname" ]] == "Windows") & testMySQL ){ 
    
    ### Describe the database (NB: this is not a connection)
    myDb <- edb( 
        dbType       = "RODBC_MySQL", 
        dbSourceName = "nameOfODBCSource", # or any name you like
        dbName       = "nameOfDatabase", 
        dbLogin      = "yourUserName", 
        dbPwd        = "yourPassword", 
        dbHost       = "127.0.0.1", 
        dbPort       = 3306 
    )   #    
    
    
    
    ### Register the data source in ODBC 
    edbDataSource( myDb, verbose = TRUE ) 
    
    
    
    ### Use the database:
    
    ## Write data in a table in the database
    
    # First retrieve the table profile
    profileTbl <- myDb[ "PROFILE" ] 
    
    # Change the ID's (pseudo new profiles)
    profileTbl[, "ID_PROFILE" ] <- 3:4 
    
    # Write the 'new' data in the database:
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE", 
        data      = profileTbl, 
        mode      = "a" # append
    )   
    
    # Alternative method:
    profileTbl[, "ID_PROFILE" ] <- 5:6 # Change IDs.
    myDb[ "PROFILE", mode = "a" ] <- profileTbl 
    
    myDb[ "PROFILE" ] # Look at the result
    
    # This would not work, because one column is missing:
    # (because the underlying function use dbWriteTable)
    try( 
        edbWrite( 
            edb       = myDb, 
            tableName = "PROFILE", 
            data      = profileTbl[, -1 ], 
            mode      = "a"  
        )   
    )   # Error
    
    # But as "ID_PROFILE" is a primary key, it can be omittted 
    # if 'getKey' is specified:
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE", 
        data      = profileTbl[, -1 ], 
        mode      = "a", 
        #verbose   = TRUE,
        getKey    = "ID_PROFILE" 
    )   #
    
    
    ## Create a new table:
    
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE2", 
        data      = profileTbl, 
        mode      = "o", 
    )   #
    
    # Alternative metod:
    myDb[ "PROFILE3", mode = "o" ] <- profileTbl 
    
    edbNames( myDb ) 
    
    
    ## Update some values:
    
    profileTbl[, "COMMENTS" ] <- "My comment" 
    
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE", 
        data      = profileTbl, 
        mode      = "u", # update
        pKey      = "ID_PROFILE" # Primary key
    )   
    
    myDb[ "PROFILE" ]
    
    # Alternative method:
    profileTbl[, "COMMENTS" ] <- "My other comment" 
    myDb[ "PROFILE", mode = "u", pKey = "ID_PROFILE" ] <- profileTbl 
    
    myDb[ "PROFILE" ]
    
    
    # Delete some rows
    edbDelete( 
        edb       = myDb, 
        tableName = "PROFILE", 
        sRow      = list("SQL" = "ID_PROFILE > 2")
    )   #
    
    
    
    # Drop tables
    edbDrop( edb = myDb, tableName = "PROFILE2" ) 
    edbDrop( edb = myDb, tableName = "PROFILE3" ) 
    
    
    
    # It may be useful to transform some columns 'on-the-fly', before 
    # they are written to the database. In the example below we 
    # have some dates and times values, as well as some boolean stored 
    # as integers (seconds or days since 1970-01-01 or 0/1 values, 
    # respectively). We want to transform them when writing in the database.
    
    myDb[ "MISCFORMAT" ]
    # NB: although Yes/No format, the last column is read as integer too...
    
    # So date variables have to be converted when written to the database.
    # The code below show how to do that.
    
    # Prepare a new record to be written:
    newRecord <- data.frame( 
        "ID_RECORD"   = 2, 
        "DAT_TIM_SEC" = as.POSIXct( "2011-12-15 12:00:00", tz = "GMT" ), 
        "DAT_DAY"     = as.Date( "2011-12-15" ), 
        "TEST_BOOL"   = FALSE, 
        "DAT_TIM"     = as.POSIXct( "2011-12-15 12:00:00", tz = "GMT" ), 
        "DAT"         = as.Date( "2011-12-15" ), 
        "TEST_BOOL2"  = FALSE  
    )   #
    newRecord 
    
    # Write the record:
    myDb[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = as.integer, 
        "DAT_DAY" = as.integer, "TEST_BOOL" = as.integer, 
        "TEST_BOOL2" = as.integer ) ] <- newRecord 
    
    # The records have been written as integers:
    myDb[ "MISCFORMAT" ]
    
    # But we can convert them on-the-fly
    
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
    
    myDb[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = formatDT, 
        "DAT_DAY" = formatD, "TEST_BOOL" = as.logical ) ] 
    
    # Misc test: just to make sure that this works too:
    edbWrite( 
        edb       = myDb, 
        tableName = "MISCFORMAT", 
        data      = newRecord[, -1 ], 
        mode      = "a", 
        verbose   = TRUE,
        getKey    = "ID_RECORD", 
        formatCol = list( "DAT_TIM_SEC" = as.integer, 
            "DAT_DAY" = as.integer, "TEST_BOOL" = as.integer, 
            "TEST_BOOL2" = as.integer )
    )   #    
    
    
    
    # Clean-up a bit:
    edbDelete( 
        edb       = myDb, 
        tableName = "MISCFORMAT", 
        sRow      = list("SQL" = "ID_RECORD > 1")
    )   #



    # It is possible to write an operation "log" every time edbWrite() 
    # is used (or edbDelete() or edbDrop()). The exact operation is 
    # not logged, but rather the function name, the table concerned, 
    # the version of R and easydb, the date, an eventual log message, 
    # etc. Set the argument 'logOp' to TRUE to log operations:

    # - Fetch some data
    profileTbl <- myDb[ "PROFILE", sRow = list( "ID_PROFILE" = 1 ) ] 
    
    # - Write it back, with a log
    myDb[ "PROFILE", mode = "u", pKey = "ID_PROFILE", logOp = TRUE, 
        logMsg = "Some log message" ] <- profileTbl 
    
    # Now check the log:
    myDb[ "edbLog" ]

    # - Clean up a bit
    edbDrop( edb = myDb, tableName = "edbLog" ) 



    ### Un-register the data source in ODBC (windows only)
    edbDataSource( myDb, trash = TRUE ) 
}   # 

