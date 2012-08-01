library( "easyrodbcaccess" ) 



# System check. Ingnore this or set 'testODBC' to TRUE 
testODBC <- (class(try(odbcDataSources())) != "try-error") & # Check (R)ODBC 
            (Sys.info()[[ "sysname" ]] == "Windows") &       # Only MS Windows
            (.Machine$"sizeof.pointer" == 4)                 # Only 32-bits 



### Windows only:
if( testODBC ){ 
    ### Make a copy of MS Access example database:
    #   (a database of soil profile description) 
    file.copy( 
        from = system.file( "soils.mdb", package = "easyrodbcaccess" ), 
        to   = "soils.mdb" 
    )   
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb <- edb( dbType = "RODBC_Access", dbName = "soils.mdb" ) 
    
    
    
    ### Use the database:
    
    # List the column names in a table:
    edbColnames( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    
    
    
    ### Clean-up
    file.remove( "soils.mdb" ) 
    
    
    
    ### Access 2007 ---------------------------------------------
    file.copy( 
        from = system.file( "soils.accdb", package = "easyrodbcaccess" ), 
        to   = "soils.accdb" 
    )   
    
    
    
    ### Describe the database (NB: this is not a connection)
    myDb2 <- edb( dbType = "RODBC_Access", dbName = "soils.accdb", 
        accessVersion = 2007 ) 
    
    
    
    ### Use the database:
    
    # List the column names in a table:
    edbColnames( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    
    
    
    ### Clean-up
    file.remove( "soils.accdb" ) 
}   #
