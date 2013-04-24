# .onLoad <- function(# Print a welcome message
# ### Print a welcome message.

 # libname, 
# ### See help(".onLoad")

 # pkgname
# ### See help(".onLoad")

# ){  #
    # msg <- sprintf( 
        # "'%s' package loaded. Type help(package='%s') for examples and help.", 
        # pkgname, pkgname ) 
    
    # msg2 <- sprintf( 
        # "    Notice that '%s' has been significantly refactored. Please have a look at 'easyrsqlite', 'easyrodbcaccess', 'easyrodbcexcel' or 'easyrodbcmysql'.", 
        # pkgname, pkgname ) 
    
    # packageStartupMessage( msg, msg2 ) 
# ### Does not return anything.
# }   #



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
    
    # msg2 <- sprintf( 
        # "    Notice that '%s' has been significantly refactored. Please have a look at 'easyrsqlite', 'easyrodbcaccess', 'easyrodbcexcel' or 'easyrodbcmysql'.", 
        # pkgname, pkgname ) 
    
    packageStartupMessage( msg ) # , msg2
### Does not return anything.
}   #
