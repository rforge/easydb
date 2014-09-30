set pkgname=easyrsqlite
set projectname=easydb
set version=0.7.4

cd /D "%rPackagesDir%\%projectname%\pkg" 

R CMD check --no-examples %pkgname%_%version%.tar.gz

pause
