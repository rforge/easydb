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

file.remove( list.files( getwd(), "\\.tex$", full.names = TRUE ) )
file.remove( list.files( getwd(), "\\.bib.bak$", full.names = TRUE ) )
file.remove( list.files( getwd(), "\\.R$", full.names = TRUE ) )

library("tools")
res <- compactPDF( paths = getwd(), gs_quality = "ebook" ) # paste(sep="",file.name.root,".pdf") 
res 
