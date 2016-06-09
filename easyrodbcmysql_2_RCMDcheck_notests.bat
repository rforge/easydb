set pkgname=easyrodbcmysql
set projectname=easydb
set version=0.7.8

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD check --no-examples %pkgname%_%version%.tar.gz

pause

