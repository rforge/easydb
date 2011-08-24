c:
cd \
cd "_R_PACKAGES\easydb\pkg" 
R CMD check --no-vignettes --no-examples easydb
REM --no-examples
pause
