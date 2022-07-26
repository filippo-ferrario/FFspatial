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
#' @param one_body_lenght the lenght to be used to standardize the distance: e.g. the mean length or the radius of the body for a given species.
#' 
#' @details   
#' 
#' The `PRindex_body` is calculated as:
#' \deqn{PR = \sum_{n=1}^{n} \frac{A}{\frac{D}{mean~~body~~lenght}}}
#' 
#' where _A_ is the area of the _i_ polygon in a buffer of a given radius, and _D_ is the distance of that polygon to the center of the buffer.
#' 
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @examples 
#' 
#' @seealso
#' [PRindex], [sf::st_point], [sf::st_buffer]
#' 
#' 
#' @export 


PRindex_body<-function(cx, poly, one_body_length=0.03, buffer=5){
	# inID<-which(st_contains(cx,poly, sparse=FALSE)==T)
	buff<-st_buffer(cx, dist=buffer)
	poly<-st_intersection(poly,buff)
	if (is.empty(poly)){
		res<-0
	} else {
	    area<-st_area(poly)
		bdlt<-st_distance(cx,poly,sparse=FALSE)/one_body_length
		units(bdlt)<-NULL # remove units to allow replacement, since we don't consider body_length as a unit 
		bdlt[bdlt>0 & bdlt<1]<-1
		bdlt[bdlt==0]<-0.5
		res<-sum(area/bdlt)
	}
	res
}

