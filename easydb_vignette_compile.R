rm(list=ls(all=TRUE)) 

setwd( "C:/_R_PACKAGES/easydb/pkg/easydb/inst/doc" ) 

Stangle( "easydb_vignette.Rnw" ) 

Sweave( "easydb_vignette.Rnw" ) 

msg <- tools::texi2dvi( 
    file        = "easydb_vignette.tex", 
    pdf         = TRUE, 
    clean       = TRUE, 
    texinputs   = getwd() 
)   # 

