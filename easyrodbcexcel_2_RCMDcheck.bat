set pkgname=easyrodbcexcel
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD check --no-examples %pkgname%

@REM --as-cran

pause
