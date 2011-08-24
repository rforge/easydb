require( "RODBC" ) 

channel <- odbcConnect( dsn = "MACRO_SE_DB_001", uid = "macroseuser", pwd = "qa8Ufd1g5Gy8Wn" ) 

myQuery <- 
 'CREATE TABLE PERSON( ID INTEGER NOT NULL PRIMARY KEY AUTO_INCREMENT, FIRSTNAME TEXT NOT NULL, LASTNAME TEXT NOT NULL )'

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
            odbcQuery( channel, query = "SELECT `ID` FROM `PERSON` WHERE `ID` = last_insert_id()" ) 
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
            odbcQuery( channel, query = "SELECT `ID` FROM `PERSON` WHERE `ID` = last_insert_id()" ) 
            res <- sqlGetResults( channel ) 
            return( as.numeric( res ) )
        }   #
    ) ) #
} ) #
res 
odbcEndTran( channel, commit = TRUE ) 
odbcSetAutoCommit( channel, autoCommit = TRUE )

