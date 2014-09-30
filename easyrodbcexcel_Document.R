
rm(list=ls(all=TRUE)) 
pkgName     <- "easyrodbcexcel" 
rootDir     <- "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/easydb" 
pkgDir      <- file.path( rootDir, "pkg" )  
pkgVersion  <- "0.7.6" 
pkgDepends  <- c("RODBC","easydb")  
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



# source( "C:/_R_PACKAGES/easydb/easydb_Compile.R", echo = TRUE, verbose = FALSE ) 
rm(list=ls(all=TRUE)) 
pkg.dir.win  <- "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/easydb/pkg" 
# pkg.dir.lin  <- "/media/JMOEYS_8G2/_R_PACKAGES/easydb/pkg" 
pkg.name     <- "easyrodbcexcel" 
pkg.version  <- "0.7.4" 
pkg.depends  <- c("RODBC","easydb") 
pkg.suggests <- NULL 
RVersion     <- "R (>= 2.15.1)" 
# r.path       <- "C:/Program Files/_SCIENCE/R_PROJECT_2-4-1/bin" 
r.path       <- ""  #  Use curent R version

# Version history
#       0.7.0 = easyrodbcexcel extracted from easydb;



require( "rcmdwrapper" ) # See rcmdwrapper_1.1.zip


if( .Platform[[ "OS.type" ]] == "windows" ){ 
    pkg.dir <- pkg.dir.win 
}   #
if( .Platform[[ "OS.type" ]] == "unix" ){ 
    pkg.dir <- pkg.dir.lin 
}   #



# Change the description file:
pkg.description( 
    pkg.name     = pkg.name, 
    pkg.dir      = pkg.dir, 
    pkg.version  = pkg.version, 
    pkg.depends  = pkg.depends, 
    pkg.suggests = pkg.suggests, 
    RVersion     = RVersion  
)   #



package.skeleton.dx( 
    pkgdir      = file.path( pkg.dir, pkg.name ), 
    namespace   = TRUE  
)   #



pkg.remove.wrapper( pkg.name = pkg.name ) 

