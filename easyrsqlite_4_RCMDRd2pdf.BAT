set project=easydb
set pkgname=easyrsqlite
set version=0.7.5

cd /D "%rPackagesDir%\%project%\pkg" 

R CMD Rd2pdf %pkgname% --output=%pkgname%.%version%.pdf

pause
