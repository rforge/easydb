set pkgname=easyrodbcexcel
set projectname=easydb
set version=0.7.7

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD check --no-examples %pkgname%_%version%.tar.gz

@REM --as-cran

pause
