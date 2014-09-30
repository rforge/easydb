
rm(list=ls(all=TRUE)) 
pkgName     <- "easyrsqlite" 
rootDir     <- "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/easydb" 
pkgDir      <- file.path( rootDir, "pkg" )  
pkgVersion  <- "0.7.4" 
pkgDepends  <- c("RSQLite","easydb") 
pkgSuggests <- NULL # c( "easyrsqlite", "easyrodbcexcel", "easyrodbcmysql", "easyrodbcaccess" ) 
RVersion    <- NULL 



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

