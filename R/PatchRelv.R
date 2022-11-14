# ===============================================================================
# Name   	: Patch Relevance index
# Author 	: Filippo Ferrario
# Date   	: 26-08-2022 [dd-mm-yyyy]
# Version	: 0.1
# URL		: 
# Aim    	: For use in map2raster
# ===============================================================================

#' Patch Relevance index calculated within a buffer.
#' 
#' 
#' 
#' @param cx  st_point center(s) of the buffer. Provided by `map2raster` if `PatchRelv` is used in it.
#' @param poly a collection of polygons representing the patches to be use to calculate the index
#' @param buffer a vector (or dataframe) of polygons representing the buffers. tipically the result of sf::st_buffer.
#' @param zero_distance the distance value that will be used to calculate the Area:distance ratio when `cx` falls in a patch. All distances < than `zero_distance` will be set to this.
#' 
#' @details   
#' 
#' The `Patch Relevance` is calculated as:
#' \deqn{PR = \sum{n=1}^{n} \frac{A}{D}}
#' where _A_ is the area of the _i_ polygon in a buffer of a given radius, and _D_ is the distance of that polygon to the center of the buffer.
#' 
#' [FFspatial::PRindex] is an alternative version, probably easier to use for tests.
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @examples 
#' 
#' @seealso
#' [PRindex_body], [sf::st_point], [sf::st_buffer], [FFspatial::PRindex]
#' 
#' 
#' @export 


PatchRelv<-function(poly,buffer, cx, zero_distance=0.01){

# intersect each buffer with polygons
		int_list<-lapply(buffer, function(bf) { 
									# browser()
									bf<-st_sfc(bf)
									st_crs(bf)<-st_crs(buffer)
									poly<-suppressWarnings(st_intersection(poly,bf))
									poly
									})
		# calculate area of each intersected polygon in each buffer
		area_list<-lapply(int_list, function(x) {
								st_area(x)
							}
						)
		# calculate distanse to each intersected polygon in each buffer
		dist_list<-mapply(function(poly, cx) {
								# browser()
							cx<-st_sfc(cx)
							st_crs(cx)<-st_crs(buffer)
							
							bdlt<-st_distance(cx,poly)
							if(class(bdlt)=='units') {units(zero_distance)<-units(bdlt)$numerator}  # This is needed in case when poly has units, otherwise the replacement with zero_distance fails
							bdlt[bdlt<zero_distance]<-zero_distance # arbitrairily choose as zero_distance. all distances which are less than zero_distance are set to this value.
							bdlt
							}, 
							int_list,cx, SIMPLIFY=FALSE)
		# divide area by distance in lists
		res_vect<- mapply(function(area, bdlt) sum(area/bdlt) , 
							area_list, dist_list, SIMPLIFY=T)
		# in case NA are produced because of lack of polygons in a buffer, set the indx to zero
		res_vect[is.na(res_vect)]<-0

		res_vect

}

# # -----------
# # Bench
# # -----------
# # data in sf format
# trssf<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='transects')
# sbs_vect<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='segmentation_refined')
# rcksf<-sf::st_cast(sbs_vect[sbs_vect$class_corrected=='rock',], 'POLYGON')

# cx<-st_geometry(st_centroid(trssf[1,]))
# new<-st_buffer(cx, dist=2)
# bfr<-st_geometry(st_buffer(cx, dist=1))

# poly=rcksf
# buffer=bfr
# zero_distance=0.01 

# PatchRelv(poly=rcksf, buffer=bfr, cx, zero_distance=0.01 )



# PRindex_raster(obs_polygon=new, side.cell=0.5,poly=rcksf, buffer=1, zero_distance=0.01) 
