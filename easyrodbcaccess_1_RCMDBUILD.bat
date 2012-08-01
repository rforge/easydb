set pkgname=easyrodbcaccess
set projectname=easydb
set rootdir="_R_PACKAGES"
c:
cd \
cd "%rootdir%\%projectname%\pkg" 
R CMD build --compact-vignettes="gs" %pkgname% 
pause
