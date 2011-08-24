library( "easydb" ) 



### Make a copy of SQLite example database:
#   (a database of soil profile description) 
file.copy( 
    from = system.file( "soils.db", package = "easydb" ), 
    to   = "soils.db" 
)   

# soils.db is now in your working directory.



library( "RSQLite" ) # Because soils.db is a SQLite database...



### Describe the database (NB: this is not a connection)
myDb <- edb( dbType = "RSQLite_SQLite", dbName = "soils.db" ) 



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
edbNCol( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
# - Dimensions:
edbDim( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 



### Clean-up
file.remove( "soils.db" ) 

