set pkgname=easyrodbcexcel
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD build --compact-vignettes="gs" %pkgname% 

pause
