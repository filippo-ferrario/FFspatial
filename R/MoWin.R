# ===============================================================================
# Name   	: Variable squared moving window 
# Author 	: Filippo Ferrario
# Date   	: 31-08-2022 [dd-mm-yyyy]
# Version	: 
# URL		: 
# Aim    	: Prepare a squared moving window matrix of variable size to be used in other functions (e.g., raster::focal, landscapemetrics::window_lsm)
# ===============================================================================


#' Variable squared moving window 
#' 
#' Prepare a squared moving window matrix of variable size to be used in other functions (e.g., raster::focal, landscapemetrics::window_lsm)
#' 
#' @param dist width of the square
#' @param side.cell width of the cell/pixel of the moving window
#' 
#' 
#' @export




MoWin<-function(dist = NULL, side.cell = NULL){

    if (is.null(dist) | !is.numeric(dist)) stop("specify dist as numeric value")
    if (is.null(side.cell)| !is.numeric(side.cell)) stop("specify side.cell as numeric value")

			ncell <- as.integer(as.character(dist/side.cell))
		    rad <- (ncell - 1)%%2
		    if (rad == 1) {
		        warning(paste0("Moving window side lenght adjusted to ", 
            (ncell <- ncell + 1) * side.cell), call. = FALSE)
    		}
    	matrix(1, ncell, ncell)
}