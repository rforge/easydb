set pkgname=easyrodbcexcel
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD check --no-examples --no-tests %pkgname%

@REM --as-cran

pause
