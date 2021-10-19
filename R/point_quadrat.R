#===============================================================================
# Name   : Substrate classification
# Author : Filippo Ferrario
# Date   : 13/10/2021 
# Version: 0.1
# Aim    : prepare point and quadrats shapefile to sample the substrate shapefile resulting from segmentation.
#		   The idea is to characterize the size class distribution of rocks in polygons identified as matrix and those identified as rocks. 
#           Desired output:
#           - shapefile with sampling quadrats
#           - shapefile of points
#           - shapefile of classified polygons per class per quadrat
#===============================================================================





#======================
# scripts
#======================

#' @param dsn path where to save output
#' @param file_tag otpional. A tag to be attached at the name of files to dfferentiate them from others from the same function.
#' @param shp polygon shapefile that need to be sampled.
#' @param site_poly optional. Polygon delimiting the area of the site to be taken into account. If not provided the shapefile provided in 'shp' will be consitered as site.
#' @param n_quad number of replicate quadrats 
#' @param n_pts number of points per quadrats
#' @param seed seed for random generation if needed.
#' @param quadrat_size size of the replicare quadrat in meters.
#' @param class_lab not used 
#' @param class_val not used.
#' 
#' @details 
#' Attention about the behaviour of the function if the shp shapefile is referring to multiple sites. In this case it is better to provide the parameter "site_poly" 

point_quadrat<-function(dsn=NULL,file_tag=NULL,shp=NULL, site_poly=NULL, n_quad=3, n_pts=6, seed=1294, quadrat_size=1 , class_lab='class',class_val=c( "Bloc ou galet","Sédiment mixte grossier") ) 
{

    # require(sf)
    # require(tidyverse)
    # require(sfheadres)
    # set seed if needed
	# make 1 unique polygon of from the shp
	if(!is.null(site_poly)) {site<-site_poly } 
		else {site<-st_union(shp) %>% sfheaders::sf_remove_holes(.)}
		
    # make grid
    grd<-st_make_grid(site, cellsize=quadrat_size)
    # make sure the reference system is the same
    st_crs(grd)<-st_crs(site)

    # seleziono solo quadrati interamente contenuti nel transetto
    grd<-st_as_sf(grd)
    site<-st_as_sf(site)
    int<- st_intersection(grd,site) %>%
           mutate(area=as.numeric(st_area(.)) ) %>%
           filter(area==quadrat_size^2) %>%
           mutate(repl=1:nrow(.))
    
    if(is.numeric(seed)) set.seed(seed)
    IDs<-sample(1:nrow(int), size=n_quad, replace=F )
    quad_samp<- int[IDs,]
    # { this part for selecting classes to consider. need development
    # class_col<-which(names(shp)==class_lab)
        # ID_poly<- as.data.frame(shp) %>% 
        #             .[,class_col] %in% class_val
    
        # int_quad<- st_intersection(shp[ID_poly,],quad_samp) %>% 
    # }
    int_quad<- st_intersection(shp,quad_samp) %>% 
                st_cast(., 'MULTIPOLYGON')

                 set.seed(988)
    pt_shp<-lapply(1:n_quad, function(x){
                      pt_samp<- int_quad[ int_quad$repl== IDs[x],] %>%
                                st_sample(st_geometry(.), size=n_pts, by_polygon=T) %>%  
                                st_as_sf(.)   
                    }) %>% bind_rows(.)
    st_write(quad_samp, dsn=paste0(dsn,'/',file_tag,'-pq-rep_quadrats.shp'))
    st_write(pt_shp, dsn=paste0(dsn,'/',file_tag,'-pq-points_quadrats.shp'))
    st_write(int_quad, dsn=paste0(dsn,'/',file_tag,'-pq-poly_quadrats.shp'))


}




# #=====================
# # bench data
# #=====================
# library(sf)

# shp<-sf::st_read('C:/Users/Utilisateur/Documents/Lavoro/DFO/GODBOUT_mosaics/data/validated shp/val_ph03_epsg32619.shp')
# plot(sf::st_geometry(shp), col=as.factor(shp$class))
# str(shp)


# plot(st_geometry(int_quad[int_quad$repl==909,]), col=as.factor(int_quad[int_quad$repl==909,]$class), border=2 )
# plot(st_geometry(pt_shp), add=T, col='white', pch=20)

