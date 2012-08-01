set pkgname=easyrodbcmysql
set projectname=easydb
set rootdir="_R_PACKAGES"
c:
cd \
cd "%rootdir%\%projectname%\pkg" 
R CMD INSTALL --build --compact-docs --byte-compile %pkgname%
pause
