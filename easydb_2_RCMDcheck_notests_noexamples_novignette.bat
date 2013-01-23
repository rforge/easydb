set pkgname=easydb
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD check --no-tests --no-examples --no-vignettes %pkgname%

@REM --as-cran

pause
