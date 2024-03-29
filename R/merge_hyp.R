# ===================================================================
# Title     :   Function merge.hyp
# Author    :   Filippo Ferrario
# Date      :   2020-04-29
# Version   :   0.1
# Aim       :   Function to merge 2 hyperframes 
#               modified from merge.data.frame.
#               currently only implemented to perform merge with all=FALSE
#               appending a suffix and renaming of variables is not implemented                
# ===================================================================


#' Function to merge 2 hyperframes 
#' 
#' @param x an hyperframe (sensu spatstat)
#' @param y an hyperframe (sensu spatstat)
#' @param by,by.x,by.y specifications of the columns used for merging. See [merge]
#' @param suffixes as in [merge]
#' @param no.dups as in [merge]
#' @param incomparables as in [merge]
#' @param ... as in [merge]
#' 
#' @details
#' Modified from merge.data.frame.
#' Currently only implemented to perform merge with all=FALSE
#' Appending a suffix and renaming of variables is not implemented                
#'
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @examples 
#' 
#' @seealso
#' [hyperframe], [merge]
#' 
#' 
#' @export 




# merge.data.frame from base package

 
merge_hyp<-function (x, y, by = intersect(names(x), names(y)), by.x = by, 
    by.y = by, sort = TRUE, 
    suffixes = c(".x", ".y"), no.dups = TRUE, incomparables = NULL, 
    ...) 
{
 all = FALSE
 all.x= all
 all.y = all

    fix.by <- function(by, df) {
        if (is.null(by)) 
            by <- numeric()
        by <- as.vector(by)
        nc <- ncol(df)
        if (is.character(by)) {
            poss <- c("row.names", names(df))
            if (any(bad <- !charmatch(by, poss, 0L))) 
                stop(ngettext(sum(bad), "'by' must specify a uniquely valid column", 
                  "'by' must specify uniquely valid columns"), 
                  domain = NA)
            by <- match(by, poss) - 1L
        }
        else if (is.numeric(by)) {
            if (any(by < 0L) || any(by > nc)) 
                stop("'by' must match numbers of columns")
        }
        else if (is.logical(by)) {
            if (length(by) != nc) 
                stop("'by' must match number of columns")
            by <- seq_along(by)[by]
        }
        else stop("'by' must specify one or more columns as numbers, names or logical")
        if (any(bad <- is.na(by))) 
            stop(ngettext(sum(bad), "'by' must specify a uniquely valid column", 
                "'by' must specify uniquely valid columns"), 
                domain = NA)
        unique(by)
    }
    
{  # create a copy of the hyperframe to be used to create the output
    x.hyp <- x
    y.hyp <- y
}

    nx <- nrow(x <- as.data.frame(x))
    ny <- nrow(y <- as.data.frame(y))

    if (nx >= 2^31 || ny >= 2^31) 
        stop("long vectors are not supported")
    by.x <- fix.by(by.x, x)
    by.y <- fix.by(by.y, y)
    if ((l.b <- length(by.x)) != length(by.y)) 
        stop("'by.x' and 'by.y' specify different numbers of columns")

    # caso 1	
    # if (l.b == 0L) {
    #     nm <- nm.x <- names(x)
    #     nm.y <- names(y)
    #     has.common.nms <- any(cnm <- nm.x %in% nm.y)
    #     if (has.common.nms) {
    #         names(x)[cnm] <- paste0(nm.x[cnm], suffixes[1L])
    #         cnm <- nm.y %in% nm
    #         names(y)[cnm] <- paste0(nm.y[cnm], suffixes[2L])
    #     }
    #     if (nx == 0L || ny == 0L) {
    #         res <- cbind(x[FALSE, ], y[FALSE, ])
    #     }
    #     else {
    #         ij <- expand.grid(seq_len(nx), seq_len(ny))
    #         res <- cbind(x[ij[, 1L], , drop = FALSE], y[ij[, 
    #             2L], , drop = FALSE])
    #     }
    # }    else {
    #     if (any(by.x == 0L)) {
    #         x <- cbind(Row.names = I(row.names(x)), x)
    #         by.x <- by.x + 1L
    #     }
    #     if (any(by.y == 0L)) {
    #         y <- cbind(Row.names = I(row.names(y)), y)
    #         by.y <- by.y + 1L
    #     }
        row.names(x) <- NULL
        row.names(y) <- NULL
        if (l.b == 1L) {
            bx <- x[, by.x]
            if (is.factor(bx)) 
                bx <- as.character(bx)
            by <- y[, by.y]
            if (is.factor(by)) 
                by <- as.character(by)
        }                else {
            if (!is.null(incomparables)) 
                stop("'incomparables' is supported only for merging on a single column")
            bx <- x[, by.x, drop = FALSE]
            by <- y[, by.y, drop = FALSE]
            names(bx) <- names(by) <- paste0("V", seq_len(ncol(bx)))
            bz <- do.call("paste", c(rbind(bx, by), sep = "\r"))
            bx <- bz[seq_len(nx)]
            by <- bz[nx + seq_len(ny)]
        }
        comm <- match(bx, by, 0L)

        bxy <- bx[comm > 0L]
        xinds <- match(bx, bxy, 0L, incomparables)
        yinds <- match(by, bxy, 0L, incomparables)
        if (nx > 0L && ny > 0L) {
                    m <- .Internal(merge(xinds, yinds, all.x, all.y))
                }        else { m <- list(xi = integer(), yi = integer(), x.alone = seq_len(nx), 
                    y.alone = seq_len(ny))}
{# define names to be used in the output
        # nm <- nm.x <- names(x)[-by.x]
        # nm.by <- names(x)[by.x]
        # nm.y <- names(y)[-by.y]
}
        
        ncx <- ncol(x)
        if (all.x) 
            all.x <- (nxx <- length(m$x.alone)) > 0L
        if (all.y) 
            all.y <- (nyy <- length(m$y.alone)) > 0L
        lxy <- length(m$xi)
{# still operates on names        
        # has.common.nms <- any(cnm <- nm.x %in% nm.y)
        # if (has.common.nms && nzchar(suffixes[1L])) 
        #     nm.x[cnm] <- paste0(nm.x[cnm], suffixes[1L])
}       
{# original 
        # x <- x[c(m$xi, if (all.x) m$x.alone), c(by.x, seq_len(ncx)[-by.x]), 
        #     drop = FALSE]
        # names(x) <- c(nm.by, nm.x)
}

        X <- x.hyp[c(m$xi, if (all.x) m$x.alone),, drop=FALSE]
        #names(X)<- paste(names(X),'.x')


        # if (all.y) {
        #     ya <- y[m$y.alone, by.y, drop = FALSE]
        #     names(ya) <- nm.by
        #     xa <- x[rep.int(NA_integer_, nyy), nm.x, drop = FALSE]
        #     names(xa) <- nm.x
        #     x <- rbind(x, cbind(ya, xa))
        # }
{# original name
                # if (has.common.nms && nzchar(suffixes[2L])) {
                #     cnm <- nm.y %in% nm
                #     nm.y[cnm] <- paste0(nm.y[cnm], suffixes[2L])
                # }
        # y <- y[c(m$yi, if (all.x) rep.int(1L, nxx), if (all.y) m$y.alone),-by.y, drop = FALSE]
                
        }       
        Y <- y.hyp[c(m$yi, if (all.x) rep.int(1L, nxx), if (all.y) m$y.alone), ,drop = FALSE]
{# this part should handl the "Left join" 
        # if (all.x) {
        #     zap <- (lxy + 1L):(lxy + nxx)
        #     for (i in seq_along(y)) {
        #         if (is.matrix(y[[1]])) 
        #           {y[[1]][zap, ] <- NA}   else is.na(y[[i]]) <- zap
        #     }
        # }
}	
{# naming
        # if (has.common.nms) 
        #     names(y) <- nm.y
        # if (no.dups && any((mi <- match(nm.by, names(y), 0L)) > 
        #     0L) && nzchar(suffixes[2L])) 
        #     names(y)[mi] <- paste0(names(y)[mi], suffixes[2L])
        # nm <- c(names(x), names(y))
        # if (any(d <- duplicated(nm))) 
        #     if (sum(d) > 1L) {
        #                     warning("column names ", paste(sQuote(nm[d]), 
        #                       collapse = ", "), " are duplicated in the result", 
        #                       domain = NA) 
        #                 } else {warning("column name ", sQuote(nm[d]), 
        #                     " is duplicated in the result", domain = NA)}
}
        res <- cbind.hyperframe(X, Y)
        if (sort) 
            res <- res[if (all.x || all.y) {
                x <- x[, seq_len(l.b), drop = FALSE]
                attributes(x) <- NULL
                do.call("order", x)
            }  else sort.list(bx[m$xi]), , drop = FALSE]
    # } # this close the "else" of the "if (l.b == 0L)""
    # attr(res, "row.names") <- .set_row_names(nrow(res))
    row.names(res)<- 1:nrow(res)
    col2del<-which(duplicated(  gsub(names(res), pattern='\\.1', replacement='') ) )
    res<-res[,-col2del, drop=FALSE]
    res
}



