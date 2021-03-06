library( "easydb" ) 



### 0. Make a copy of SQLite example database:
#      (a database of soil profile description) 
file.copy( 
    from = system.file( "soils.db", package = "easyrsqlite" ), 
    to   = "soils.db" 
)   

# soils.db is now in your working directory.



### 2. Describe the database (NB: this is not a connection)
myDb <- edb( dbType = "RSQLite_SQLite", dbName = "soils.db" ) 



## You have to install "easyrsqlite" before running this example
library( "easyrsqlite" ) # Because soils.db is a SQLite database...



### 3. Use the database:

# List the tables:
edbNames( myDb ) 



## 3.1 Read data in a table in the database

# Retrieve a table (data.frame style subsetting):
myDb[ "WRB_SOIL_GROUP" ]

# Same operation, but with edbRead()
edbRead( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 

# List the column names in a table:
edbColnames( edb = myDb, tableName = "WRB_SOIL_GROUP" ) 
 
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



## 3.2 Write data in a table in the database

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
profileTbl[, "ID_PROFILE" ] <- 5:6 
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



### Clean-up
file.remove( "soils.db" ) 
