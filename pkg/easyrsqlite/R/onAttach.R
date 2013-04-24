
.onAttach <- function(# Print a welcome message
### Print a welcome message.

 libname, 
### See help(".onLoad")

 pkgname
### See help(".onLoad")

){  #
    msg <- sprintf( 
        "'%s' package loaded. For the help menu, type help(pack='%s')", 
        pkgname, pkgname ) 
    
    packageStartupMessage( msg ) 
### Does not return anything.
}   
