# ===============================================================================
# Name   	: Perimeter to Area ratio in buffer
# Author 	: Filippo Ferrario
# Date   	: 14-11-2022 [dd-mm-yyyy]
# Version	: 
# URL		: 
# Aim    	: 
# ===============================================================================





#' Mean perimeter to area ratio of patches within a buffer.
#' 
#' 
#' 
#' @param poly a collection of polygons representing the patches to be use to calculate the index
#' @param buffer a vector (or dataframe) of polygons representing the buffers. tipically the result of sf::st_buffer.
#' @param ... unused.
# 
#' @details  
#' 
#' The ratio is calculated as:
#' \deqn{ \frac{Perimeter}{Area}}  
#' it is not scale independent, meaning that increasing the patch size while not changing the patch form will change the ratio.
#' 
#' The `...` argument allows the functioning within [map2raster], but it does not supply used arguments.
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @examples 
#' 
#' @seealso
#' [sf::st_point], [sf::st_buffer], [FFspatial::PRindex], [FFspatial::map2raster]
#' 
#' 
#' @export 


peri_area<-function(poly,buffer,...){

		# compute the area of each patch
		area<-st_area(poly)
		# compute the perimeter of each patch
		peri<-lwgeom::st_perimeter(poly)
		# compute ratio and attach it to poly dataframe
		poly$periarea<-peri/area
		
		# discard polygons with area = 0 (e.g., possible segmentation/digitizing issues resulting in lines or other area-less features)
		noAreaID<-which(as.numeric(area)==0)
		if (length(noAreaID)>0) poly<-poly[-noAreaID,]

		# intersect each buffer with polygons
		int_list<-lapply(buffer, function(bf) { 
									# browser()
									bf<-st_sfc(bf)
									st_crs(bf)<-st_crs(buffer)
									poly<-suppressWarnings(st_intersection(poly,bf))
									poly
									})
		# compute the mean ratio within each buffer
		res_vect<- sapply(int_list, function(x) mean(x$periarea) )
		res_vect
}		








# # # -----------
# # # Bench
# # -----------
# # data in sf format
# trssf<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='transects')
# sbs_vect<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='segmentation_refined')
# rcksf<-sf::st_cast(sbs_vect[sbs_vect$class_corrected=='rock',], 'POLYGON')

# cx<-st_geometry(st_centroid(trssf[1,]))
# new<-st_buffer(cx, dist=2)
# bfr<-st_geometry(st_buffer(cx, dist=1))
# st_crs(bfr)<-st_crs(trssf)
# poly=rcksf
# buffer=bfr

# # Check behaviour 
# # -------------------
# area<-st_area(poly)
# # compute the perimeter of each patch
# peri<-lwgeom::st_perimeter(poly)
# # compute ratio and attach it to poly dataframe
# poly$periarea<-peri/area

# par(mfrow=c(1,2))
# # plot(st_geometry(trssf[1,]))
# plot(new, add=F)
# plot(poly,add=T)
# plot(cx, add=T)
# plot(bfr, add=T)
# int_list<-lapply(buffer, function(bf) { 
# 									# browser()
# 									bf<-st_sfc(bf)
# 									st_crs(bf)<-st_crs(buffer)
# 									poly<-suppressWarnings(st_intersection(poly,bf))
# 									poly
# 									})
# for (i in 1:nrow(int_list[[1]]))
# {
# 	plot(st_geometry(int_list[[1]][i,]), add=T, col=i)
# }
# int_list[[1]]

# # case buffer intersection results in fragmentation of a patch 
# # locator()
# cx2<-st_sfc( st_point(c( 595602.9,5461802)) )
# st_crs(cx2)<-st_crs(trssf)
# bfr2<-st_geometry(st_buffer(cx2, dist=1))
# plot(bfr2, border='green',add=T)
# cx<-cx2
# buffer=bfr2

# plot(bfr2, border='green',add=F)
# # plot(new, add=F)
# plot(poly,add=T)
# plot(cx, add=T)
# int_list2<-lapply(buffer, function(bf) { 
# 									# browser()
# 									bf<-st_sfc(bf)
# 									st_crs(bf)<-st_crs(buffer)
# 									poly<-suppressWarnings(st_intersection(poly,bf))
# 									poly
# 									})
				
# for (i in 1:nrow(int_list2[[1]]))
# {
# 	plot(st_geometry(int_list2[[1]][i,]), add=T, col=i)
# }


# int_list
# int_list2
# mean(int_list[[1]]$periarea)
# mean(int_list2[[1]]$periarea)

# peri_area(poly,bfr, cx)
# peri_area(poly,bfr2, cx2)

# # Use with map2raster
# # -----------------------------
# newOBS<-st_buffer(st_centroid(trssf[1,]), dist=2)
# test<-map2raster(obs_polygon=newOBS, side.cell=0.5, radius=1, FUN=peri_area,poly=rcksf)

# plot(st_geometry(newOBS))
# plot(rcksf, add=T)
# raster::plot(test, alpha=0.5, add=T)



# # # infinite value for Mixed sediment trasect ph02 
# # # -----------------------------

# mixsf<-sf::st_cast(sbs_vect[sbs_vect$class_corrected=='mixed_sediment' & sbs_vect$transect=='ph02',], 'POLYGON')

# plot(st_geometry(mixsf),  xlim=c(595382.1 ,595385.9), ylim=c(5461792,5461796 ))


# cx<-st_point(c(595383.7, 5461794))
# bfr<-st_geometry(st_buffer(cx, dist=0.15))

# plot(cx,pch='+', add=T)
# plot(bfr, add=T)


# poly=mixsf
# buffer=bfr
# st_crs(buffer)<-st_crs(poly)
# peri_area(poly,buffer)


# new<-st_sfc(st_buffer(cx, dist=2))
# st_crs(new)<-st_crs(poly)
# plot(new)
# plot(st_make_grid(new),cellsize=0.5, add=T )

# res<-map2raster(obs_polygon=new, side.cell=0.5, radius=0.15, FUN=peri_area,poly=mixsf)
# raster::plot(res,add=T, useRaster=F)