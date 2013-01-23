set pkgname=easydb
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD build --no-vignettes --md5 %pkgname% 

pause
