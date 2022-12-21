# ===============================================================================
# Name   	: Number of patch in buffer
# Author 	: Filippo Ferrario
# Date   	: 26-07-2022 [dd-mm-yyyy]
# Version	: 
# URL		: 
# Aim    	: 1. Number of patch in buffer
# 			  5. Patch diversity in buffer
# 			  2. % cover substrate type in buffer
# 			  3. Mean patch size in buffer
# 			  4. mean perimeter:area ratio per patch in buffer
# ===============================================================================


#' Number of patch in buffer
#' 
#' Number of patch within
#' 
#' @inheritParams PatchRelv
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







patch_stats<-function(poly,buffer, cx , field){

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
		
		 
		stats<-sapply(int_list, function(x) {
								# browser()
								fct<-as.factor(as.data.frame(x)[,field])
								spl<-split(x, f=fct)
								n_type<- sapply(spl, function(y) nrow(y))
								area_type<- sapply(spl, function(y) {
									areas<-st_area(y)
									mean(areas)
									})
								peri_type<- sapply(spl, function(y) { # browser()
									peri<-st_cast(y, to='MULTILINESTRING')
									peri<-st_length(peri)
									mean(peri)
									})
								peri_area<-mapply(x=peri_type,y=area_type, function(x,y){x/y})
								area_buff<-mean(st_area(buffer))
								cover_type<- sapply(spl, function(y) {
									areas<-st_area(y)
									sum(areas/area_buff)*100
									})

								N<-sum(n_type)
								Rich<-length(n_type)
								shan<- -sum(n_type/N*log(n_type/N))
								H<-shan/log(Rich)	

								out<-list(n_type=n_type,avg_area_type=area_type,avg_peri_type=peri_type,cover_type=cover_type, N=N,Richness=Rich, Shannon=shan, Evenness=H)
									})
		stats
}

								
								





# # -----------
# # Bench
# # -----------
# # # data in sf format
# trssf<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='transects')
# sbs_vect<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='segmentation_refined')
# rcksf<-sf::st_cast(sbs_vect[sbs_vect$class_corrected=='rock',], 'POLYGON')

# cx<-st_geometry(st_centroid(trssf[1,]))
# new<-st_buffer(cx, dist=2)
# bfr<-st_geometry(st_buffer(cx, dist=1))

# poly<-sbs_vect
# buffer<-bfr
# field= 'class_corrected'


# # patch_stats(poly=rcksf, buffer=bfr, cx)

# # plot(new)
# # plot(rcksf,add=T)
# # plot(new,add=T)
# # plot(bfr,add=T)

# patch_stats(poly=sbs_vect, buffer=bfr, cx, field='class_corrected')

# # plot(new)
# plot(bfr,add=F)
# plot(sbs_vect,add=T)
# plot(bfr,add=T)



# # map2raster(obs_polygon=new, side.cell=0.5, radius=1, FUN=PatchNum,  poly=rcksf)


