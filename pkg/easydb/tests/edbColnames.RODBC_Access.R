library( "easydb" ) 
library( "RODBC" ) 



# System check. Ingnore this or set 'testODBC' to TRUE 
testODBC <- (class(try(odbcDataSources())) != "try-error") & 
            (Sys.info()[[ "sysname" ]] == "Windows")



### Windows only:
if( testODBC ){ 
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
    
    # List the column names in a table:
    edbColnames( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    
    
    
    ### Clean-up
    file.remove( "soils.mdb" ) 
    
    
    
    ### Access 2008 ---------------------------------------------
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
    
    # List the column names in a table:
    edbColnames( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    
    
    
    ### Clean-up
    file.remove( "soils.accdb" ) 
}   #
