c:
cd \
cd "_R_PACKAGES\easydb\pkg" 
R CMD check --no-tests --no-examples easydb
REM --no-examples --no-vignettes
pause
