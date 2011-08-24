library( "easydb" ) 



testMySQL <- FALSE # Set to TRUE to run some tests
                   # after modifying the edb() part below 
                   # with your own database setting



### Windows only:
if( (Sys.info()[[ "sysname" ]] == "Windows") & testMySQL ){ 
    
    
    
    library( "RODBC" ) 
    
    
    
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
    
    ## 3.3 Create a new table:
    
    edbWrite( 
        edb       = myDb, 
        tableName = "PROFILE2", 
        data      = profileTbl, 
        mode      = "o", 
    )   #
    
    # Alternative metod:
    myDb[ "PROFILE3", mode = "o" ] <- profileTbl 
    
    edbNames( myDb ) 
    
    
    
    # Delete some rows
    edbDelete( 
        edb       = myDb, 
        tableName = "PROFILE", 
        sRow      = list("SQL" = "ID_PROFILE > 2")
    )   #
    
    
    
    # Drop tables
    edbDrop( edb = myDb, tableName = "PROFILE2" ) 
    edbDrop( edb = myDb, tableName = "PROFILE3" ) 
    
    
    
    ### Un-register the data source in ODBC (windows only)
    edbDataSource( myDb, trash = TRUE ) 
}   # 

