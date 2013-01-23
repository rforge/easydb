# source( "C:/_R_PACKAGES/easydb/easydb_Compile.R", echo = TRUE, verbose = FALSE ) 
rm(list=ls(all=TRUE)) 
pkgName     <- "easyrodbcmysql" 
rootDir     <- "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/easydb" 
pkgDir      <- file.path( rootDir, "pkg" )  
pkgVersion  <- "0.7.3" 
pkgDepends  <- c("RODBC","easydb")  
pkgSuggests <- NULL # c( "easyrsqlite", "easyrodbcexcel", "easyrodbcmysql", "easyrodbcaccess" ) 
RVersion    <- NULL 
r.path       <- ""  #  Use curent R version



# Version history
#       0.7.0 = easyrodbcexcel extracted from easydb;



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

