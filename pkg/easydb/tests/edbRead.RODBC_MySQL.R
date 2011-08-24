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
    
    ## Read data in a table in the database
    
    # Retrieve a table (data.frame style subsetting):
    myDb[ "WRB_SOIL_GROUP" ] 
    
    # Same operation, but with edbRead()
    edbRead( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    
    # Retrieve part of a table (with row constrains)
    myDb[ "WRB_SOIL_GROUP", list("NAME" = c("AC","CR","PL")) ] 
    
    # Same operation, but with edbRead()
    edbRead( 
        edb       = myDb, 
        tableName = "WRB_SOIL_GROUP", 
        sRow      = list("NAME" = c("AC","CR","PL")) 
    )   
    
    # Retrieve part of a table (row constrains + select only some columns)
    myDb[ 
        "WRB_SOIL_GROUP", 
        list("NAME" = c("AC","CR","PL")), 
        c("ID_WRB_SOIL_GROUP","NAME") 
    ]   
    
    # Same operation, but with edbRead()
    edbRead( 
        edb       = myDb, 
        tableName = "WRB_SOIL_GROUP", 
        sRow      = list("NAME" = c("AC","CR","PL")), 
        sCol      = c("ID_WRB_SOIL_GROUP","NAME") 
    )   
    
    # Use some SQL constrains
    myDb[ 
        "WRB_SOIL_GROUP", 
        list( 
            "NAME" = c("AC","AB","AL","AN","AT"), 
            "SQL" = "ABBREV LIKE 'Al%'"
        )   
    ]   
    
    # NB: the different elements in the list are separated by 
    # "AND" statement
    
    # Multiple row constrains, alternative (constrain 1 OR constrin 2) 
    myDb[ 
        "WRB_SOIL_GROUP", 
        list( 
            "NAME" = c("AC","AB"), 
            "ID_WRB_SOIL_GROUP" = 25:30
        ),  
        sRowOp = "OR" 
    ]   
    
    
    
    # Check the dimension of a table
    # - Number of columns:
    edbNCol( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    # - Number of rows:
    edbNRow( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    # - Dimensions:
    edbDim( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
    
    
    
    ### Un-register the data source in ODBC (windows only)
    edbDataSource( myDb, trash = TRUE ) 
}   #

