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
#' Prepare a squared or circular moving window matrix of variable size to be used in other functions (e.g., raster::focal, landscapemetrics::window_lsm)
#' 
#' @param dist width of the square or radius of the circle
#' @param side.cell width of the cell/pixel of the moving window
#' @param type type of moving windows. At the moment just 'square' or 'circle'. 
#' 
#' @details 
#' 
#' The size of the matrix is always squared for both types. For the circular one, the cells outside the circle are Filled with NA, while in the squared window cells have just values of 1.  
#' The number of columns and rows is alway odd to allow having a focal cell. Thus both the side and the raidus of the windows are adjusted (in excess) if needed.
#' 
#' @export




MoWin<-function(dist = NULL, side.cell = NULL, type=c('square','circle')){
    if (is.null(dist) | !is.numeric(dist)) stop("specify dist as numeric value")
    if (is.null(side.cell)| !is.numeric(side.cell)) stop("specify side.cell as numeric value")
    if (!type %in% c('square','circle')) stop("type must be either 'square' or 'circle'")
	
	# rectangular (i.e., squared) case
	if (type=='square')	{
				ncell <- as.integer(as.character(dist/side.cell))
			    rad <- (ncell - 1)%%2
			    if (rad == 1) {
			        warning(paste0("Moving window side lenght adjusted to ", 
	            (ncell <- ncell + 1) * side.cell), call. = FALSE)
	    		}
	    	out<-matrix(1, ncell, ncell)
	    }

	# circular case
	if (type=='circle')	{
				ncell <- 2*as.integer(as.character(dist/side.cell))
			    rad <- (ncell - 1)%%2
			    if (rad == 1) {
			        ncell <- ncell + 1 
			        radius <- ncell/2
			        warning(paste0("Moving window radius adjusted to ",ncell* side.cell/2))
			   }
	    	tempMat<-matrix(1:ncell^2, ncell, ncell)

			# create a data frame of the coordinates:

			g = expand.grid(1:nrow(tempMat), 1:nrow(tempMat))
			# compute distance-to-centre:
	    	radius<-radius+0.5
			g$d2 = sqrt ((g$Var1-radius)^2 + (g$Var2-radius)^2)
			# compare with circle radius:
			g$inside = g$d2<=(ncell-0.5)/2

			out<-tempMat
			out[g$inside]<-1
			out[!g$inside]<-NA
			# out

	    }
	    out

}







# # bench
# # =======
# MoWin(3,0.5, type='square')
# 7*0.5
# MoWin(3,1)

# MoWin(1,0.1)
# MoWin(4,0.2)
# 0.2*9

# MoWin(dist=2,side.cell=1, type='square')

# dist=1
# side.cell=0.5
# type='circle'
# MoWin(dist=3,side.cell=0.5, type='circle')

# MoWin(dist=1,side.cell=1, type='circle')


# MoWin(dist=2,side.cell=0.1, type='circle')
