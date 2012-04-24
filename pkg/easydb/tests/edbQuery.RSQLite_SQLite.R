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
myDb[ "WRB_SOIL_GROUP", list("ABBREV" = c("AC","CR","PL")), 
    verbose = TRUE ] 

# Same operation, but with edbQuery() 
edbQuery( edb = myDb, statement = 
    "SELECT * FROM [WRB_SOIL_GROUP] WHERE [ABBREV] IN ('AC','CR','PL')" ) 

### Clean-up
file.remove( "soils.db" ) 

