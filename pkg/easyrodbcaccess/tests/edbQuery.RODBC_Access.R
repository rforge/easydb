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

    ## Read data in a table in the database
    
    # Retrieve a table (data.frame style subsetting):
    myDb[ "WRB_SOIL_GROUP" ]
    
    # Same operation, but with edbRead()
    edbRead( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    
    # Retrieve part of a table (with row constrains)
    myDb[ "WRB_SOIL_GROUP", list("ABBREV" = c("AC","CR","PL")), 
        verbose = TRUE ] 
    
    # Same operation, but with edbQuery() 
    edbQuery( edb = myDb, statement = 
        "SELECT * FROM [WRB_SOIL_GROUP] WHERE [ABBREV] IN ('AC','CR','PL')" ) 
    
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

    ## Read data in a table in the database
    
    # Retrieve a table (data.frame style subsetting):
    myDb2[ "WRB_SOIL_GROUP" ]
    
    # Same operation, but with edbRead()
    edbRead( edb = myDb2, tableName = "WRB_SOIL_GROUP" ) 
    
    # Retrieve part of a table (with row constrains)
    myDb2[ "WRB_SOIL_GROUP", list("ABBREV" = c("AC","CR","PL")), 
        verbose = TRUE ] 
    
    # Same operation, but with edbQuery() 
    edbQuery( edb = myDb2, statement = 
        "SELECT * FROM [WRB_SOIL_GROUP] WHERE [ABBREV] IN ('AC','CR','PL')" ) 
    
    ### Clean-up
    file.remove( "soils.accdb" ) 
}   #
