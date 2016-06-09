set project=easydb
set pkgname=easyrodbcaccess
set version=0.7.8

cd /D "%rPackagesDir%\%project%\pkg" 

R CMD Rd2pdf %pkgname% --output=%pkgname%.%version%.pdf

pause
