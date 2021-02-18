# ==============================================================================
# Title		:	Focal stats to spatstats 
# Author	:	Filippo Ferrario
# Date		:	2021-02-12
# Version	:	0.1
# Aim		:	function to apply the raster::focal funtion to spatstat objects
# ==============================================================================

#' Focal stats to spatstats 
#' 
#' This function to apply the raster::focal funtion to spatstat objects
#' 
#' @param X a ppp or a owin object
#' @param W_from a window (object of class "owin") or data acceptable to [spatstat::as.owin]. Needed to give a spatial domain if X is an owin. Optional if X is a ppp.
#' @param dist the size of the side of the squared moving window used to calculate the focal statistic.
#' @param side.cell the size of the (squared) pixel side to be used to subdivide the moving window.
#' @param ... agruments to be passed to pixellate (except "eps";e.g., DivideByPixelArea) and [raster::focal] (in particular `fun`, `na.rm`, `pad` and `padValue`).
#' 
#' 
#' @details
#' 
#' If X is an owin object, the "W_from" argument is used to assign the Window of observation which otherwise will be lacking. 
#' In this case the window should likely be assigned from a ppp under analysis.
#' #' 
#' ***workflow***
#' ----------------
#'  
#' If X is an `owin` object:
#' 1) pixellate the owin object with the "W_from" window used to assign the spatial domain.
#' 	  The pixallate function is used with DivideByPixelArea=T. 
#' 	  As such, the vaule assigned to each pixel is the percent of the pixel coverd by the polygon being pixellated.
#' 	  if "W_from" has an irregual shape the values outside "W_from" are given a value 0.
#' 2) assign the spatial domain of the "W_from" to exclude the pixels outside "W_from".
#' 	  This is achieved passing the pixellate output to as.im(,W=W_from) which set the pixels outside "W_from" as NA.
#' 3) convert image to a rasterLayer to be able to use raster::focal
#' 
#' If X is an `ppp` object:
#' the density map is calculated using pixellate.ppp and transformed in a raster to be able to use the function focal.
#' If the argument "W_from" is specified then the Window of X is changed to correspond to that of "W_from". Oterwise X retain its original Window.
#' Setting "W_from" when X is a `ppp` object is useful to set the Window of the density variable equal to the that of another ppp (e.g., that used a response variable)
#' when X has been created with a different Window. NOTE: only the spatial domain of the density variable is modified and not the window of the `ppp` used as X.
#' 
#' Common steps:
#' 4) apply focal to raster (the matrix need to have numebr of cells by row and column odd)
#' 5) extract the data matrix and return the output as an `im` object
#' 
#' 
#' 
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @examples 
#' 
#' @seealso
#' [spatstat::pixellate.owin], [spatstat::pixellate.ppp], [raster::focal]
#' 
#' @return
#' an `im` object
#' 
#' @export


spstFocal<-function(X, W_from=NULL, dist=NULL, side.cell=NULL, ...){
			# require(raster)
			if(is.null(dist)) stop('specify dist')
			if(is.null(side.cell)) stop('specify side.cell')
			if(is.owin(X) & is.null(W_from)) stop('"W_from" must be specified when X is an owin') 	
            if (!is.null(W_from)) {WW<- as.owin(W_from)} else {WW<-W_from}
			# chech moving window dimension
			ncell<- as.integer(as.character(dist/side.cell)) # as.integer(as.character()) is needed to avoid floating point error when dist < 1
			rad<-  (ncell-1) %% 2 
 			if (  rad == 1 ) {
				# message(paste0('Moving window side lenght adjusted to ',(ncell<- ncell+1)*side.cell))
				warning(paste0('Moving window side lenght adjusted to ',(ncell<- ncell+1)*side.cell), call.=FALSE)
				# ncell<- ncell+1
				}
			# prepare raster for OWIN objects	
        	if (is.owin(X)){
        				# 1) pixellate the owin object with the Transect window.
        				p2<-pixellate.owin(X, W=WW, ..., eps=c(side.cell,side.cell))
        				# 2) assign the spatial domain of the transect
        				int<-as.im(p2,W=WW)
        				# 3) convert image to a rasterLayer
        				ras<-raster::raster(int)
        		}
        	# prepare raster for PPP objects
        	if (is.ppp(X))	{
        		# check existence of W_from and, if found, assign it as Window to X otherwise set WW as the Window of X
        		if (!is.null(WW)) {Window(X)<-WW} else {WW<-Window(X)}
        		int<-pixellate.ppp(X,eps=c(side.cell,side.cell),...) 
        		# 3) convert image to a rasterLayer
				ras<-raster::raster(int)
        		}	
			# apply focal to raster (the matrix need to have numebr of cells by row and column odd)
			mean_ras<-raster::focal(ras, w=matrix(1,ncell,ncell),...) #  "..." specifing should allow ",fun=mean, na.rm=T, pad=T, padValue=NA"
			# extract the data matrix 
			m<- raster::as.matrix(mean_ras)
			# fix spatial indexing
			m<- spatstat::transmat(m, from='European', to ='spatstat')
			# convert back to image object
			mean_im<-as.im(m, WW)
			im2<-as.im(mean_im,WW)
			im2
}


# ############# TEST #################

# library(spatstat)

# # =============================
# # load data
# # =============================

# load('./data/data_for_spatstat-green_urchin.R')
# load('./data/data_for_spatstat-rocks.R')
# load('./data/data_for_spatstat-sand_dollars.R')

# # =============================
# # Study
# # =============================

# ur<-urc_ppp['T2-11_20+T2-11_20']
# WW<-Window(ur[[1]])
# rk<-rock_win['T2-11_20+T2-11_20']
# sd<-sand_dols['T2-11_20+T2-11_20']


# # check on use of density.ppp
# # ------------------------------

# plot(sd[[1]]); plot(Window(sd[[1]]),add=T, border=2)
# d1<-density.ppp(sd[[1]], eps=0.1)
# sd2<-sd[[1]]
# Window(sd2)<-as.owin(ur[[1]])
# plot(sd2); plot(Window(sd2),add=T, border=2)
# d2<-density.ppp(sd2, eps=0.1)

# par(mfrow=c(2,1))
# plot(d1,useRaster=F)
# plot(d2,useRaster=F)

# # # bench
# # # --------
# X=ur[[1]]
# W_from=ur[[1]]
# dist=2
# side.cell=0.1
# fun=mean
# na.rm=T
# pad=T
# padValue=NA

# # case 1 poligons
# resW2<-spstFocal(X=rk[[1]],W_from=ur[[1]], dist=0.3, side.cell=0.1, fun=mean, na.rm=T, pad=T, padValue=NA)
# plot(resW2,useRaster=F)

# xy<-locator()
# plot(resW2, xlim=c(min(xy$x),max(xy$x)), ylim=c(min(xy$y),max(xy$y)),main='')
# plot(Window(ur[[1]]), add=T)
# plot(rk[[1]], add=T, border=2)

# # case 2 points without changing window.
# resP<-spstFocal(X=ur[[1]], dist=0.3, side.cell=0.1,DivideByPixelArea=F, preserve=T, fun=mean, na.rm=T, pad=T, padValue=NA)
# plot(resP,useRaster=F)
# plot(ur[[1]],add=T, pch=1, col='white')


# # case 3 with X having a Window different from the ppp used as the response variable
# # NO CHANGE OF X WINDOW
# plot(sd[[1]]); plot(Window(sd[[1]]),add=T, border=2)
# resSD<-spstFocal(X=sd[[1]], dist=0.3, side.cell=0.1,DivideByPixelArea=F, preserve=T, fun=mean, na.rm=T, pad=T, padValue=NA, sigma=0.1)
# plot(resSD,useRaster=F)
# plot(Window(sd[[1]]), border=5,add=T)
# plot(Window(ur[[1]]), border=4,add=T)
# plot(sd[[1]],add=T, pch=16, col=rgb(0,0,0,0.05))



# # case 4 with X having a Window different from the ppp used as the response variable
# # WITH CHANGE OF X WINDOW
# # plot(density.ppp(sd[[1]],eps=0.1),add=F,useRaster=F); plot(sd[[1]],add=T); plot(Window(sd[[1]]),add=T, border=5)
# # SDwu<- sd[[1]]; Window(SDwu)<-Window(ur[[1]]); plot(Window(SDwu))
# # densSDwu<-density.ppp(SDwu,eps=0.1, sigma=0.5)
# # plot(densSDwu,add=F,useRaster=F,main='Dens - sigma=0.5 - eps=0.1'); plot(Window(sd[[1]]),add=T, border=5)# ; plot(SDwu,add=T)


# resSD4<-spstFocal(X=sd[[1]],W_from=ur[[1]], dist=0.3, side.cell=0.1,DivideByPixelArea=F, preserve=T, fun=mean, na.rm=T, pad=T, padValue=NA)
# plot(resSD4,useRaster=F, main='Focal - sigma=0.1 - dist=0.5')
# plot(Window(sd[[1]]), border=5,add=T)
# plot(Window(ur[[1]]), border=4,add=T)
# plot(sd[[1]],add=T, pch=16, col=rgb(0,0,0,0.05))



# # zoom
# xy<-locator()
# plot(resSD4, xlim=c(min(xy$x),max(xy$x)), ylim=c(min(xy$y),max(xy$y)),main='')
# plot(Window(sd[[1]]), border=5,add=T)
# plot(Window(ur[[1]]), border=4,add=T)
# plot(sd[[1]],add=T, pch=16, col=rgb(0,0,0,0.1))
