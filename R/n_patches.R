# ===============================================================================
# Name   	: Number of patch in buffer
# Author 	: Filippo Ferrario
# Date   	: 26-07-2022 [dd-mm-yyyy]
# Version	: 
# URL		: 
# Aim    	: 
# ===============================================================================


#' Number of patch in buffer
#' 
#' 
#' 
#' @inheritParam PatchRelv
#' 
#' @details   
#' 
#' 
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @examples 
#' 
#' @seealso
#' [map2raster], [PatchRelv], [sf::st_point], [sf::st_buffer]
#' 
#' 
#' @export 







PatchNum<-function(poly,buffer, cx ){

# intersect each buffer with polygons
		int_list<-lapply(buffer, function(bf) { 
									# browser()
									bf<-sf::st_sfc(bf)
									sf::st_crs(bf)<-sf::st_crs(buffer)

									poly<-suppressWarnings(sf::st_intersection(poly,bf))
									poly<-st_cast(poly, to='MULTIPOLYGON')
									poly<-suppressWarnings(st_cast(poly, to='POLYGON'))
									poly
									})
		
		# 
		res_vect<-sapply(int_list, function(x) nrow(x))

		res_vect

}


# -----------
# Bench
# -----------
# # # data in sf format
# trssf<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='transects')
# sbs_vect<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='segmentation_refined')
# rcksf<-sf::st_cast(sbs_vect[sbs_vect$class_corrected=='rock',], 'POLYGON')

# cx<-st_geometry(st_centroid(trssf[1,]))
# new<-st_buffer(cx, dist=2)
# bfr<-st_geometry(st_buffer(cx, dist=1))


# # PatchNum(poly=rcksf, buffer=bfr, cx)

# # plot(new)
# # plot(rcksf,add=T)
# # plot(new,add=T)
# # plot(bfr,add=T)

# PatchNum(poly=sbs_vect, buffer=bfr, cx)

# # plot(new)
# plot(bfr,add=F)
# plot(sbs_vect,add=T)
# plot(bfr,add=T)



# map2raster(obs_polygon=new, side.cell=0.5, radius=1, FUN=PatchNum,  poly=rcksf)


