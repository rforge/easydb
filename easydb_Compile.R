# source( "C:/_R_PACKAGES/easydb/easydb_Compile.R", echo = TRUE, verbose = FALSE ) 
rm(list=ls(all=TRUE)) 
pkgName     <- "easydb" 
rootDir     <- "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/easydb" 
pkgDir      <- file.path( rootDir, "pkg" )  
pkgVersion  <- "0.7.3" 
pkgDepends  <- NULL 
pkgSuggests <- NULL # c( "easyrsqlite", "easyrodbcexcel", "easyrodbcmysql", "easyrodbcaccess" ) 
RVersion    <- NULL 
r.path       <- ""  #  Use curent R version

# Version history
#       0.2.x = MySQL finshed; 
#       0.3.x = Access finished; 
#       0.4.x = POSIX, Date and Boolean data handling
#       0.5.x = Build-in operation-log system
#       0.6.x = Excel finished;
#       0.7.x = Separated base-easydb from easyrodbcexcel and 
#               easyrsqlite
#       0.7.3 = new 64 bit computer



# Source some utility functions
source( file.path( rootDir, "packageUtilities.R" ) ) 
library( "inlinedocs" )



# Change the description file:
pkgDescription( 
    pkgName     = pkgName, 
    pkgDir      = pkgDir, 
    pkgVersion  = pkgVersion, 
    pkgDepends  = pkgDepends, 
    pkgSuggests = pkgSuggests, 
    RVersion    = RVersion  
)   #



package.skeleton.dx( 
    pkgdir      = file.path( pkgDir, pkgName ), 
    namespace   = TRUE  
)   #



pkgRemove( pkgName = pkgName ) 

