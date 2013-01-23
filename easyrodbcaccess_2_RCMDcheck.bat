set pkgname=easyrodbcaccess
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 
 
R CMD check --no-examples %pkgname%

pause

