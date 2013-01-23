set pkgname=easydb
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD INSTALL --build --both-arch --compact-docs --byte-compile %pkgname%

pause
