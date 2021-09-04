#!/usr/bin/env Rscript

# Set Working directory to git root

library(tidyverse)

if (rstudioapi::isAvailable()){
    
    # When called in RStudio
    SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
    
} else {
    
    #  When called from command line 
    SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
    SD <- unlist(str_split(SD,'/'))
    
}

WD <- paste(SD[1:(length(SD)-1)],collapse='/')
setwd(WD)

png("png/Kurven_09.png",width=1000,height=1000)

xlim <- c(0,1)
ylim <- c(0,1)

col <- c("red","green")

plot( NA
      , xlab = "Schutz vor Infektion"
      , ylab = "Schutz vor Tod (CFR geimpft)"
      , xlim = xlim
      , ylim = ylim
      , main = "Schutz - CFR geimpfte"
      , yaxt = 'n'
      )

    par (new = TRUE)
    
CFRa <- 0.1
CFRu <- 0.2176
Q <- 0.7005

    curve( 1 - ( ( (1-x*Q)*CFRa - (1-Q)*(CFRu) )/ ( (1-x)*Q ) )/CFRu
        , col = col[1]
        , xlab=""
        , ylab=""
        , xlim = xlim
        , ylim = ylim
        , axes = FALSE
        , lwd = 5
        , cex.lab = 3
        , cex.axis = 2
    )
    axis( side=2)

dev.off()
