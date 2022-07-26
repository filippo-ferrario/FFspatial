



PRindex_raster<-function(obs_polygon, side.cell=0.1, poly, buffer=5, zero_distance=0.01){
	# check parameters
	if( sum(c('sf','sfc') %in% class(obs_polygon))==0) stop('obs_polygon need to be of class "sfc" or "sf" with a geometry')
	obs_poly<-st_geometry(obs_polygon) # extract geometry 
	if (! c('sfc_POLYGON') %in% class(obs_poly)) stop('obs_polygon need to be of class "sfc_POLYGON"')
	if (length(obs_poly)!=1) stop('To use this function for more than 1 obs_polygon please use an apply synthax')
	# if (!is.function(FUN)) stop("argument FUN is not a function!")
	
	# define points for which calculate the index
	regrd<-st_make_grid(obs_poly, cellsize=side.cell, square=TRUE)
	pixID<-st_intersects(regrd,obs_poly, sparse=FALSE)
	regrd<-regrd[pixID]
	# retain only pixels in the obs_polygon
	sam_pts<-st_centroid(regrd)
	# create buffers per each point
	buffs<-st_buffer(sam_pts, dist=buffer)
	# MERTRIC BLOCK
	# ------------------
		# intersect each buffer with polygons
		int_list<-mapply( function(bf, cx) { 
									# browser()
									bf<-st_sfc(bf)
									st_crs(bf)<-st_crs(buffs)
									poly<-st_intersection(poly,bf)
									poly
									}, 
						 buffs,sam_pts, SIMPLIFY=FALSE)
		# calculate area of each intersected polygon in each buffer
		area_list<-lapply(int_list, function(x) {
								st_area(x)
							}
						)
		# calculate distanse to each intersected polygon in each buffer
		dist_list<-mapply(function(poly, cx) {
								# browser()
							cx<-st_sfc(cx)
							st_crs(cx)<-st_crs(sam_pts)
							
							bdlt<-st_distance(cx,poly)
							if(class(bdlt)=='units') {units(zero_distance)<-units(bdlt)$numerator}  # This is needed in case when poly has units, otherwise the replacement with zero_distance fails
							bdlt[bdlt<zero_distance]<-zero_distance # arbitrairily choose as zero_distance. all distances which are less than zero_distance are set to this value.
							bdlt
							}, 
							int_list,sam_pts, SIMPLIFY=FALSE)
		# divide area by distance in lists
		res_vect<- mapply(function(area, bdlt) sum(area/bdlt) , 
							area_list, dist_list, SIMPLIFY=T)
		# in case NA are produced because of lack of polygons in a buffer, set the indx to zero
		res_vect[is.na(res_vect)]<-0
	# ---------------------------
	# Now extract the coordinates from the point geometry and combine them with the values.
        dt2<-cbind(st_coordinates(sam_pts),res_vect)
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


new<-st_buffer(st_centroid(trssf[1,]), dist=2)
obs_polygon=new
side.cell=0.5
poly=rcksf
buffer=1 
zero_distance=0.01

#  test
new<-st_buffer(st_centroid(trssf[1,]), dist=2)
system.time(
mt1<-PRindex_raster(obs_polygon=new, side.cell=0.1,poly=rcksf, buffer=1, zero_distance=0.01) 
)

 #   user  system elapsed 
 # 200.70    3.74  207.44 
 #   user  system elapsed 
 # 214.27    4.42  219.95
 #   user  system elapsed 
 # 279.72    4.83  285.36 
mean(c(207.44,219.95,285.36 ))



system.time(
mt2<-PRindex_raster(obs_polygon=trssf[1,], side.cell=0.5, poly=rcksf, buffer=1, zero_distance=0.01) 
)
# user  system elapsed 
 # 454.00    8.88  467.51 
 # 441.18    7.70  456.66 

p1<-st_point(c(595604.9,5461802))
p2<-st_point(c(595604.0,5461802))

id<-mt2[]<50
mt3<-mt2;mt3[!id]<-NA ;mt3[mt3[]==0]<--99
plot(st_geometry(trssf[1,]),xlim=(c(5,-5)+595592.4), ylim=(c(5,-5)+5461802))
raster::plot(mt2, useRaster=F, add=T)
plot(st_geometry(rcksf), add=T, border=1, col=0)
# plot(new, add=T, c
								