require( "DBI" ) 
require( "RSQLite" ) 


# Initiate the database
m <- dbDriver("SQLite")
tfile <- tempfile()
con <- dbConnect(m, dbname = tfile)


# Create a table with AUTOINCREMENT Identifier
myQuery <- 
 'CREATE TABLE PERSON( ID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, FIRSTNAME TEXT NOT NULL, LASTNAME TEXT NOT NULL )'
dbSendQuery( conn = con, statement = myQuery )

dbListTables(con)


# Send some INSERT queries to add new data, without ID
myQuery2 <- 
c( 'INSERT INTO [PERSON] (FIRSTNAME,LASTNAME) VALUES("ALBERT","DUPONT")', 
   'INSERT INTO [PERSON] (FIRSTNAME,LASTNAME) VALUES("JAN","ANDERSSON")' ) 
# dbBeginTransaction(con)
# dbSendQuery( conn = con, statement = myQuery2[1] ) 
# res1 <- dbSendQuery( conn = con, statement = "SELECT ID FROM [PERSON] WHERE ID = last_insert_rowid()" ) 
# dbClearResult( res1 ) 
# dbSendQuery( conn = con, statement = myQuery2[2] ) 
# res2 <- dbSendQuery( conn = con, statement = "SELECT ID FROM [PERSON] WHERE ID = last_insert_rowid()" ) 
# dbClearResult( res2 ) 
# res3 <- dbCommit(con)

system.time( { 
    dbBeginTransaction(con)
    res <- unlist( lapply( 
        X   = 1:1000, 
        FUN = function(X){ 
            dbSendQuery( conn = con, statement = myQuery2[1] ) 
            res <- dbGetQuery( conn = con, statement = "SELECT ID FROM [PERSON] WHERE ID = last_insert_rowid()" ) 
            return( as.numeric( res ) )
        }   #
    ) ) #
    dbCommit(con) 
} ) #
res 

system.time( { 
    res <- unlist( lapply( 
        X   = 1:1000, 
        FUN = function(X){ 
            dbSendQuery( conn = con, statement = myQuery2[1] ) 
            res <- dbGetQuery( conn = con, statement = "SELECT ID FROM [PERSON] WHERE ID = last_insert_rowid()" ) 
            return( as.numeric( res ) )
        }   #
    ) ) #
} ) #
res 



# Read the table
tbl <- dbReadTable( conn = con, name = "PERSON" )
dim( tbl ) 

# clean up 
dbDisconnect(con)
file.remove(tfile) 

