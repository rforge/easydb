set project=easydb
set pkgname=easyrodbcmysql
set version=0.7.7

cd /D "%rPackagesDir%\%project%\pkg" 

R CMD Rd2pdf %pkgname% --output=%pkgname%.%version%.pdf

pause
