c:
cd \
cd "_R_PACKAGES\easydb\pkg" 
R CMD check --no-tests --no-examples --as-cran --no-vignettes easydb
pause
