# ===============================================================================
# Name   	: Map metric to Raster using buffers
# Author 	: Filippo Ferrario
# Date   	: 26-08-2022 [dd-mm-yyyy]
# Version	: 0.1
# URL		: 
# Aim    	: Map metric to Raster using buffers. The function use the approach of
# ===============================================================================

#' Map metric to Raster using buffers
#' 
#' Apply a function to calculate a summary metric at buffers created along a regular grid of sampling points whithin the polygon of an observed area.
#' 
#' @param obs_polygon polygon delimiting the area observed (e.g., a transect)
#' @param side.cell the size of the (squared) pixel side to be used to subdivide the observation window (i.e., the polygon defining the extent of the raster to be produced).
#' @param radius the radius of the buffer to be used.
#' @param FUN the funtion to be mapped on the obs_polygon.
#' @param ... arguments to be passed to FUN.
#' 
#' 
#' @details   
#' 
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @examples 
#' 
#' @seealso
#' [PRindex_body], [sf::st_point], [sf::st_buffer]
#' 
#' 
#' @export 

 
map2raster<-function(obs_polygon, side.cell=0.1, radius=1, FUN,...) {

	# check parameters
	if( sum(c('sf','sfc') %in% class(obs_polygon))==0) stop('obs_polygon need to be of class "sfc" or "sf" with a geometry')
	obs_poly<-st_geometry(obs_polygon) # extract geometry 
	if (! c('sfc_POLYGON') %in% class(obs_poly)) stop('obs_polygon need to be of class "sfc_POLYGON"')
	if (length(obs_poly)!=1) stop('To use this function for more than 1 obs_polygon please use an apply synthax')
	# if (!is.function(FUN)) stop("argument FUN is not a function!")
	
	# List of Optional arguments
	listArgs<-list(...)
	

	# define points for which calculate the index
	regrd<-st_make_grid(obs_poly, cellsize=side.cell, square=TRUE)
	pixID<-st_intersects(regrd,obs_poly, sparse=FALSE)
	regrd<-regrd[pixID]
	# retain only pixels in the obs_polygon
	cx<-st_centroid(regrd)
	# create buffers per each point
	buffer<-st_buffer(cx, dist=radius)
	# add buffer and cx to list of optional arguments to be passed to FUN
	listArgs$cx<-cx
	listArgs$buffer<-buffer
	# METRIC BLOCK
	# ------------------
		res_vect<-do.call(FUN, args=listArgs)  # do.call allows to interpret the listArgs with amended arguments
	# ---------------------------
	# Now extract the coordinates from the point geometry and combine them with the values.
        dt2<-cbind(st_coordinates(cx),res_vect)
		# create a raster from XYZ
		ras<-raster::rasterFromXYZ(dt2, res=c(side.cell,side.cell))
		# add CRS
		raster::crs(ras)<-raster::crs(obs_poly)
		ras

}



# # -----------
# # Bench
# # -----------
# # data in sf format
# trssf<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='transects')
# sbs_vect<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='segmentation_refined')
# rcksf<-sf::st_cast(sbs_vect[sbs_vect$class_corrected=='rock',], 'POLYGON')


# newOBS<-st_buffer(st_centroid(trssf[1,]), dist=2)


# # system.time(
# res<-map2raster(obs_polygon=newOBS, side.cell=0.5, radius=1, FUN=PatchRelv, zero_distance=0.5,poly=rcksf)
# # )


# system.time(
# mt2<-PRindex_raster(obs_polygon=newOBS, side.cell=0.5, poly=rcksf, buffer=1, zero_distance=0.01) 
# )

