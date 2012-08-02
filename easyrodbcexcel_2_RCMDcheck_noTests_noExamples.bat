c:
cd \
cd "_R_PACKAGES\easydb\pkg" 
R CMD check --no-examples --no-tests easyrodbcexcel
@REM --as-cran
pause
