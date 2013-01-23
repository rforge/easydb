set pkgname=easyrodbcmysql
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD check --no-tests %pkgname%

pause

