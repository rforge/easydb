set pkgname=easyrodbcexcel
set projectname=easydb

cd /D "%rPackagesDir%\%projectname%\pkg" 

svnversion > %pkgname%\inst\SVN_VERSION

R CMD build --compact-vignettes="gs" %pkgname% 

pause
