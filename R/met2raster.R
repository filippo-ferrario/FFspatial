# ===============================================================================
# Name   	: Metric to Raster
# Author 	: Filippo Ferrario
# Date   	: 17-06-2022 [dd-mm-yyyy]
# Version	: 0.1
# URL		: 
# Aim    	: 
# ===============================================================================

#' 
#' 
#' @param obs_polygon 
#' @param ... the distance value that will be used to calculate the Area:distance ratio when `cx` falls in a patch. All distances < than `zero_distance` will be set to this.
#' @param side.cell the size of the (squared) pixel side to be used to subdivide the observation window (i.e., the polygon defining the extent of the raster to be produced).
#' @param make_spatstat_im logical (default=TRUE). Should the output be a [spatstat.geom::im] object for `spatstat` rather than a [raster::raster]? 
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

 
met2raster<-function(obs_polygon, side.cell=0.1, FUN,make_spatstat_im=TRUE,...) {
		# check parameters
		if( sum(c('sf','sfc') %in% class(obs_polygon))==0) stop('obs_polygon need to be of class "sfc" or "sf" with a geometry')
		obs_poly<-st_geometry(obs_polygon) # extract geometry 
		if (! c('sfc_POLYGON') %in% class(obs_poly)) stop('obs_polygon need to be of class "sfc_POLYGON"')
		if (length(obs_poly)!=1) stop('To use this function for more than 1 obs_polygon please use an apply synthax')
		if (!is.function(FUN)) stop("argument FUN is not a function!")
		
		# define points for which calculate the index
		regrd<-st_make_grid(obs_poly, cellsize=side.cell, square=TRUE)
		pixID<-st_intersects(regrd,obs_poly, sparse=FALSE)
		regrd<-regrd[pixID]
		# retain only pixels in the obs_polygon
		sam_pts<-st_centroid(regrd)
		
		# calculate pixel values using the metric function provided
		pixvals<-sapply(sam_pts, function(cx) { 
									# browser()
									cx<-st_sfc(cx)
									st_crs(cx)<-st_crs(sam_pts)
									FUN(cx,...)
									} )
		# Now extract the coordinates from the point geometry and combine them with the values.
        dt2<-cbind(st_coordinates(sam_pts),pixvals)
		# create a raster from XYZ
		ras<-raster::rasterFromXYZ(dt2, res=c(side.cell,side.cell))
		ras
}


# -----------
# Bench
# -----------
# data in sf format
trssf<-sf::st_read('DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='transects')
sbs_vect<-sf::st_read('DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='segmentation_refined')
rcksf<-sf::st_cast(sbs_vect[sbs_vect$class_corrected=='rock',], 'POLYGON')


# data in spatstat format
load('DFO-Godbout-2020-localSDM/R_output/R_objects/windows-rocks.Rdata')
load('DFO-Godbout-2020-localSDM/R_output/R_objects/windows-transects.Rdata')



# test 

# r1<-met2raster(obs_polygon=trssf[1,], side.cell=0.1, metric=PRindex,make_spatstat_im=TRUE,poly=st_geometry(rcksf), buffer=5, zero_distance=0.01) 

new<-st_buffer(st_centroid(trssf[1,]), dist=2)
system.time(
r1<-met2raster(obs_polygon=new, side.cell=0.1, PRindex,make_spatstat_im=TRUE,poly=rcksf, buffer=1, zero_distance=0.01) 
)

 # #   user  system elapsed 
 # # 217.00    4.03  226.13 
 # #   user  system elapsed 
 # # 286.80    5.67  305.14
 #   user  system elapsed 
 # 235.86    3.88  240.43 

 mean( c(226.13 , 305.14,240.43 ))

system.time(
r2<-met2raster(obs_polygon=trssf[1,], side.cell=0.5, PRindex,make_spatstat_im=TRUE,poly=rcksf, buffer=1, zero_distance=0.01) 
)

#   user  system elapsed 
# 571.76   11.31  599.86
# 494.75    8.64  525.50 


p1<-st_point(c(595604.9,5461802))
p2<-st_point(c(595604.0,5461802))

id<-r2[]<50
r3<-r2; r3[!id]<-NA ; r3[r3[]==0]<--99
plot(st_geometry(trssf[1,]),xlim=(c(5,-5)+595592.4), ylim=(c(5,-5)+5461802))
raster::plot(r2, useRaster=F, add=T)
plot(st_geometry(rcksf), add=T, border=1, col=0)
# plot(new, add=T, col=0, border='red')
# plot(c(p1,p2),add=T, col=c('blue','red'), pch=16)
# plot(c(st_buffer(p1, dist=1),st_buffer(p2, dist=1)), add=T, col=0, border=c('blue','red'))

