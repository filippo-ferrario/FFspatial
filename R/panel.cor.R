# ===================================================================
# Title		:	Correlation Pair Plot panel
# Author	:	Filippo Ferrario
# Date		:	2020-07-22
# Version	:	0.1
# Aim		:	function to be produce panels in pair plots to investigate correlations
# 				modified from Zurr 2009 book.
# ===================================================================



#####################################
#  variable correlation for plot
#####################################


#' Correlation Pair Plot panel
#' 
#' function to be produce panels in pair plots to investigate correlations
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com}
#'  
#' @details 
#' Modified from Zurr 2009 book.
#' 
#' @export


 panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor,cor.treshold=NULL, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r.val <- cor(x, y,...)
    r <- abs(cor(x, y,...))
    txt <- format(c(r.val, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    if (is.null(cor.treshold)){
    text(0.5, 0.5, txt, cex = cex.cor * r, col=1)
    } else { if (r<=cor.treshold){    
              text(0.5, 0.5, txt, cex = cex.cor * r, col=1)
              } else {text(0.5, 0.5, txt, cex = cex.cor * r, col=2)}
    } 
}

