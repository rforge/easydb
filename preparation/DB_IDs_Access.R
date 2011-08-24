require( "RODBC" ) 

channel <- odbcConnectAccess(
    access.file = "C:/Documents and Settings/julienm/My Documents/_WORKS/_R_TESTS/DB_IDs/DB_IDs.mdb" ) 


myQuery <- 
 'CREATE TABLE [PERSON] ( [ID] Counter NOT NULL, [FIRSTNAME] varchar(50) NOT NULL, [LASTNAME] varchar(50) NOT NULL, PRIMARY KEY ([ID]) )' # ID PRIMARY KEY AUTOINCREMENT, 

sqlQuery( channel, query = myQuery )

sqlTables( channel )[,"TABLE_NAME"]



myQuery2 <- 
c( "INSERT INTO `PERSON` (FIRSTNAME,LASTNAME) VALUES ('ALBERT','DUPONT')", 
   "INSERT INTO `PERSON` (FIRSTNAME,LASTNAME) VALUES ('JAN','ANDERSSON')" ) 



system.time( { 
    res <- unlist( lapply( 
        X   = 1:1000, 
        FUN = function(X){ 
            sqlQuery( channel, query = myQuery2[1] ) # not sqlQuery
            odbcQuery( channel, query = "SELECT @@IDENTITY" ) 
            res <- sqlGetResults( channel ) 
            return( as.numeric( res ) )
        }   #
    ) ) #
} ) #
res 

odbcSetAutoCommit( channel, autoCommit = FALSE )
system.time( { 
    res <- unlist( lapply( 
        X   = 1:1000, 
        FUN = function(X){ 
            sqlQuery( channel, query = myQuery2[1] ) # not sqlQuery
            odbcQuery( channel, query = "SELECT @@IDENTITY" ) 
            res <- sqlGetResults( channel ) 
            return( as.numeric( res ) )
        }   #
    ) ) #
} ) #
res 
odbcEndTran( channel, commit = TRUE ) 
odbcSetAutoCommit( channel, autoCommit = TRUE )


close(channel)
