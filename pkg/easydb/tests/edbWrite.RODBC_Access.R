library( "easydb" ) 



### Windows only:
if( Sys.info()[[ "sysname" ]] == "Windows" ){ 
    ### Make a copy of SQLite example database:
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
        mode      = "o"  
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
    
    
    ## Delete some rows
    edbDelete( 
        edb       = myDb, 
        tableName = "PROFILE", 
        sRow      = list("SQL" = "ID_PROFILE > 2")
    )   #
    
    
    
    ## Drop tables
    edbDrop( edb = myDb, tableName = "PROFILE2" ) 
    edbDrop( edb = myDb, tableName = "PROFILE3" ) 
    
    
    
    ### Clean-up
    file.remove( "soils.mdb" ) 
}   #
