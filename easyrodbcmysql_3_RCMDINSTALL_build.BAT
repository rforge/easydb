set pkgname=easyrodbcmysql
set projectname=easydb
set version=0.7.8

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD INSTALL --build --both-arch --compact-docs --byte-compile %pkgname%_%version%.tar.gz

pause
