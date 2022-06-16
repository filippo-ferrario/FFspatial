# ===============================================================================
# Name   	: Patch Relevance index
# Author 	: Filippo Ferrario
# Date   	: 16-06-2022 [dd-mm-yyyy]
# Version	: 0.1
# URL		: 
# Aim    	: 
# ===============================================================================

#' Patch Relevance index calculated within a buffer.
#' 
#' @param cx an st_point center of the buffer
#' @param poly a collection of polygons representing the patches to be use to calculate the index
#' @param buffer the radius of the buffer to use around `cx`.
#' @param zero_distance the distance value that will be used to calculate the Area:distance ratio when `cx` falls in a patch. All distances < than `zero_distance` will be set to this.
#' 
#' @details   
#' 
#' The `PRindex` is calculated as:
#' \deqn PR = \sum_{n=1}^{n} \frac{A}{D}
#' where _A_ is the area of the _i_ polygon in a buffer of a given radius, and _D_ is the distance of that polygon to the center of the buffer.
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

 
# based only on distance
PRindex<-function(cx, poly, buffer=5, zero_distance=0.01){
	buff<-st_buffer(cx, dist=buffer)
	poly<-st_intersection(poly,buff)
	area<-st_area(poly)
	bdlt<-st_distance(cx,poly)
	bdlt[bdlt<zero_distance]<-zero_distance # arbitrairily choose as zero_distance. all distances which are less than zero_distance are set to this value.
	# bdlt[bdlt>0 & bdlt<1]<-1
	res<-sum(area/bdlt)
	res
}
