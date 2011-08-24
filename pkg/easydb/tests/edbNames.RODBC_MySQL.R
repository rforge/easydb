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
    
    # List the table names in the database:
    edbNames( edb = myDb ) 
    
    # with more details
    edbNames( edb = myDb, onlyNames = FALSE ) 
    
    
    
    ### Un-register the data source in ODBC (windows only)
    edbDataSource( myDb, trash = TRUE ) 
}   #


