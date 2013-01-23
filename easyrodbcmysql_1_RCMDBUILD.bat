set pkgname=easyrodbcmysql
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD build --compact-vignettes="gs" --md5 %pkgname% 

pause
