set pkgname=easydb
set version=0.7.8

cd /D "%rPackagesDir%\%pkgname%\pkg" 

R CMD Rd2pdf %pkgname% --output=%pkgname%.%version%.pdf

pause
