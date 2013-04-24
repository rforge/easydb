set pkgname=easyrodbcexcel
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD INSTALL --build --compact-docs --byte-compile %pkgname%
pause
