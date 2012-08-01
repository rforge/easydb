set pkgname=easyrsqlite
set projectname=easydb
set rootdir="_R_PACKAGES"
c:
cd \
cd "%rootdir%\%projectname%\pkg" 
R CMD check --no-examples %pkgname%
pause

