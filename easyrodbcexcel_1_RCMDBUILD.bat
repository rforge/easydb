set pkgname=easyrodbcaccess
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD build --compact-vignettes="gs" easyrodbcexcel 

pause
