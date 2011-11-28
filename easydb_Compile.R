# source( "C:/_R_PACKAGES/easydb/easydb_Compile.R", echo = TRUE, verbose = FALSE ) 
rm(list=ls(all=TRUE)) 
pkg.dir.win  <- "C:/_R_PACKAGES/easydb/pkg" 
# pkg.dir.lin  <- "/media/JMOEYS_8G2/_R_PACKAGES/easydb/pkg" 
pkg.name     <- "easydb" 
pkg.version  <- "0.4.0" # 0.2 = MySQL finshed; 0.3 = Access finished; 0.4 = POSIX, Date and Boolean data handling
pkg.depends  <- NULL 
pkg.suggests <- c("RODBC","RSQLite") 
RVersion     <- "R (>= 2.10.0)" 
# r.path       <- "C:/Program Files/_SCIENCE/R_PROJECT_2-4-1/bin" 
r.path       <- ""  #  Use curent R version




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

