
rm(list=ls(all=TRUE)) 
pkgName     <- "easyrodbcaccess" 
rootDir     <- sprintf( "%s/easydb", Sys.getenv("rPackagesDir") )
pkgDir      <- file.path( rootDir, "pkg" )  
pkgVersion  <- "0.7.8" 
pkgDepends  <- c( "RODBC", "easydb" ) 
pkgImports  <- c( "stats", "utils" ) 
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
    pkgImports  = pkgImports, 
    pkgSuggests = pkgSuggests, 
    RVersion    = RVersion  
)   #



package.skeleton.dx( 
    pkgdir      = file.path( pkgDir, pkgName ), 
    namespace   = TRUE  
)   #



pkgRemove( pkgName = pkgName ) 

