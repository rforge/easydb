library( "easydb" ) 
library( "RODBC" ) 



# System check. Ingnore this or set 'testODBC' to TRUE 
testODBC <- (class(try(odbcDataSources())) != "try-error") & # Check (R)ODBC 
            (Sys.info()[[ "sysname" ]] == "Windows") &       # Only MS Windows
            (.Machine$"sizeof.pointer" == 4)                 # Only 32-bits 



### Windows only:
if( testODBC ){ 
    ### Make a copy of MS Access example database:
    #   (a database of soil profile description) 
    file.copy( 
        from = system.file( "soils.mdb", package = "easydb" ), 
        to   = "soils.mdb" 
    )   
    
    # soils.db is now in your working directory.
    
    
    
    library( "RODBC" ) # Because soils.db is a SQLite database...
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb <- edb( dbType = "RODBC_Access", dbName = "soils.mdb" ) 
    
    
    
    ### Use the database:

    ## Read data in a table in the database
    
    # Retrieve a table (data.frame style subsetting):
    myDb[ "WRB_SOIL_GROUP" ]
    
    # Same operation, but with edbRead()
    edbRead( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    
    # Retrieve part of a table (with row constrains)
    myDb[ "WRB_SOIL_GROUP", list("ABBREV" = c("AC","CR","PL")) ] 
    
    # Same operation, but with edbRead()
    edbRead( 
        edb       = myDb, 
        tableName = "WRB_SOIL_GROUP", 
        sRow      = list("ABBREV" = c("AC","CR","PL")) 
    )   
    
    # Retrieve part of a table (row constrains + select only some columns)
    myDb[ 
        "WRB_SOIL_GROUP", 
        list("ABBREV" = c("AC","CR","PL")), 
        c("ID_WRB_SOIL_GROUP","NAME") 
    ]   
    
    # Same operation, but with edbRead()
    edbRead( 
        edb       = myDb, 
        tableName = "WRB_SOIL_GROUP", 
        sRow      = list("ABBREV" = c("AC","CR","PL")), 
        sCol      = c("ID_WRB_SOIL_GROUP","NAME") 
    )   
    
    # Use some SQL constrains
    myDb[ 
        "WRB_SOIL_GROUP", 
        list( 
            "ABBREV" = c("AC","AB","AL","AN","AT"), 
            "SQL" = "NAME LIKE 'Al%'"
        )   
    ]   
    
    # NB: the different elements in the list are separated by 
    # "AND" statement
    
    # Multiple row constrains, alternative (constrain 1 OR constrin 2) 
    myDb[ 
        "WRB_SOIL_GROUP", 
        list( 
            "ABBREV" = c("AC","AB"), 
            "ID_WRB_SOIL_GROUP" = 25:30
        ),  
        sRowOp = "OR" 
    ]   
    
    
    
    # Check the dimension of a table
    # - Number of columns:
    edbNCol( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    # - Number of rows:
    edbNRow( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    # - Number of rows, with constrains: 
    edbNRow( edb = myDb, tableName = "WRB_SOIL_GROUP", 
        sRow = list("ABBREV" = c("AC","AB")) ) 
    # - Dimensions:
    edbDim( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    
    
    
    # More ways to select columns:
    
    # - Indexes
    myDb[ "WRB_SOIL_GROUP", sCol = 2:3 ] 
    
    # - Logicals
    myDb[ "WRB_SOIL_GROUP", sCol = c(FALSE,TRUE,TRUE) ] 
    
    
    
    # It may be useful to transform some columns 'on-the-fly', after 
    # they have been read from the database. In the example below we 
    # have some dates and times values, as well as some boolean stored 
    # as integers (seconds or days since 1970-01-01 or 0/1 values, 
    # respectively). We want to transform them into dates or boolean.
    
    myDb[ "MISCFORMAT" ]
    # NB: although Yes/No format, the last column is read as integer too...
    
    # So date variables stored as integers have to be converted.
    # The code below show how to do that.
    
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
    myDb[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = formatDT, 
        "DAT_DAY" = formatD, "TEST_BOOL" = as.logical ) ] 
    
    
    
    # SQL SELECT DISTINCT, equivalent of R unique().
    # To select unique / distinct values in a (group of) columns, 
    # set 'distinct' to TRUE:
    myDb[ "PROFILE", sCol = "COMMENTS", distinct = TRUE ] 
    
    
    
    # SQL ORDER BY, equivalent of R order(). 
    # To sort the table according to some columns (ascending or 
    # descending), use 'orderBy':
    myDb[ "WRB_SOIL_GROUP", orderBy = "ABBREV DESC" ] 
    
    
    
    ### Clean-up
    file.remove( "soils.mdb" ) 
    
    
    
    ### Access 2007 ---------------------------------------------
    file.copy( 
        from = system.file( "soils.accdb", package = "easydb" ), 
        to   = "soils.accdb" 
    )   
    
    # soils.db is now in your working directory.
    
    
    
    library( "RODBC" ) # Because soils.db is a SQLite database...
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb2 <- edb( dbType = "RODBC_Access", dbName = "soils.accdb", 
        accessVersion = 2007 ) 
    
    
    
    ### Use the database:

    ## Read data in a table in the database
    
    # Retrieve a table (data.frame style subsetting):
    myDb2[ "WRB_SOIL_GROUP" ]
    
    # Same operation, but with edbRead()
    edbRead( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    
    # Retrieve part of a table (with row constrains)
    myDb2[ "WRB_SOIL_GROUP", list("ABBREV" = c("AC","CR","PL")) ] 
    
    # Same operation, but with edbRead()
    edbRead( 
        edb       = myDb2, 
        tableName = "WRB_SOIL_GROUP", 
        sRow      = list("ABBREV" = c("AC","CR","PL")) 
    )   
    
    # Retrieve part of a table (row constrains + select only some columns)
    myDb2[ 
        "WRB_SOIL_GROUP", 
        list("ABBREV" = c("AC","CR","PL")), 
        c("ID_WRB_SOIL_GROUP","NAME") 
    ]   
    
    # Same operation, but with edbRead()
    edbRead( 
        edb       = myDb2, 
        tableName = "WRB_SOIL_GROUP", 
        sRow      = list("ABBREV" = c("AC","CR","PL")), 
        sCol      = c("ID_WRB_SOIL_GROUP","NAME") 
    )   
    
    # Use some SQL constrains
    myDb2[ 
        "WRB_SOIL_GROUP", 
        list( 
            "ABBREV" = c("AC","AB","AL","AN","AT"), 
            "SQL" = "NAME LIKE 'Al%'"
        )   
    ]   
    
    # NB: the different elements in the list are separated by 
    # "AND" statement
    
    # Multiple row constrains, alternative (constrain 1 OR constrin 2) 
    myDb2[ 
        "WRB_SOIL_GROUP", 
        list( 
            "ABBREV" = c("AC","AB"), 
            "ID_WRB_SOIL_GROUP" = 25:30
        ),  
        sRowOp = "OR" 
    ]   
    
    
    
    # Check the dimension of a table
    # - Number of columns:
    edbNCol( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    # - Number of rows:
    edbNCol( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    # - Dimensions:
    edbDim( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    
    
    
    # More ways to select columns:
    
    # - Indexes
    myDb2[ "WRB_SOIL_GROUP", sCol = 2:3 ] 
    
    # - Logicals
    myDb2[ "WRB_SOIL_GROUP", sCol = c(FALSE,TRUE,TRUE) ] 
    
    
    
    # It may be useful to transform some columns 'on-the-fly', after 
    # they have been read from the database. In the example below we 
    # have some dates and times values, as well as some boolean stored 
    # as integers (seconds or days since 1970-01-01 or 0/1 values, 
    # respectively). We want to transform them into dates or boolean.
    
    myDb2[ "MISCFORMAT" ]
    # NB: although Yes/No format, the last column is read as integer too...
    
    # So date variables stored as integers have to be converted.
    # The code below show how to do that.
    
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
    myDb2[ "MISCFORMAT", formatCol = list( "DAT_TIM_SEC" = formatDT, 
        "DAT_DAY" = formatD, "TEST_BOOL" = as.logical ) ] 
    
    
    
    # SQL SELECT DISTINCT, equivalent of R unique().
    # To select unique / distinct values in a (group of) columns, 
    # set 'distinct' to TRUE:
    myDb2[ "PROFILE", sCol = "COMMENTS", distinct = TRUE ] 
    
    
    
    # SQL ORDER BY, equivalent of R order(). 
    # To sort the table according to some columns (ascending or 
    # descending), use 'orderBy':
    myDb2[ "WRB_SOIL_GROUP", orderBy = "ABBREV DESC" ] 
    
    
    
    ### Clean-up
    file.remove( "soils.accdb" ) 
}   #
