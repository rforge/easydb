set pkgname=easydb
set projectname=easydb
set rootdir="_R_PACKAGES"
c:
cd \
cd "%rootdir%\%projectname%\pkg" 
R CMD check --no-tests --no-examples --no-vignettes %pkgname%
@REM --as-cran
pause
