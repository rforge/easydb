c:
cd \
cd "_R_PACKAGES\easydb\pkg" 
R CMD check --no-tests --no-vignettes --no-examples easydb
REM --no-examples
pause
