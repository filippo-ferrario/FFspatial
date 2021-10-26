#===============================================================================
# Name   : Substrate classification
# Author : Filippo Ferrario
# Date   : 13/10/2021 
# Version: 0.2
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
#' @param n_pts number of points per category per quadrats
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
    require(tidyverse)
    # require(sfheadres)

    # make all classes lower case to ensure consistency
    class_lab<-tolower(class_lab)
    class_val<-tolower(class_val)
    names(shp)<-tolower(names(shp))
    shp_df<-as.data.frame(shp)
    cl_vec<-tolower(shp_df[,names(shp_df)==class_lab])
    shp[,names(shp)==class_lab]<-cl_vec


	# make 1 unique polygon of from the shp
	if(!is.null(site_poly)) {site<-site_poly 
            } else {site<-sf::st_union(shp) %>% sfheaders::sf_remove_holes(.)}
		
    # make grid
    grd<-sf::st_make_grid(site, cellsize=quadrat_size)
    # make sure the reference system is the same
    sf::st_crs(grd)<-sf::st_crs(site)

    # seleziono solo quadrati interamente contenuti nel transetto
    grd<-sf::st_as_sf(grd)
    site<-sf::st_as_sf(site)
    int<- sf::st_intersection(grd,site) %>%
           mutate(quad_area=as.numeric(sf::st_area(.)) ) %>%
           filter(quad_area==quadrat_size^2) %>%
           mutate(repl=1:nrow(.))
    
    # set seed if needed
    if(is.numeric(seed)) set.seed(seed)
    IDs<-sample(1:nrow(int), size=n_quad, replace=F )
    quad_samp<- int[IDs,]
    int_quad<- sf::st_intersection(shp,quad_samp) %>% 
                sf::st_cast(., 'MULTIPOLYGON') %>%
                mutate(poly_area=as.numeric(sf::st_area(.)))%>%
                filter(.data[[class_lab]] %in% class_val )

    pt_shp<-NULL
    for (i in 1:length(class_val)){            
        pt_temp<-lapply(1:n_quad, function(x){
                                    ww<-int_quad %>% #[ int_quad$repl== IDs[x],] %>%
                                        filter(.data[[class_lab]]==class_val[i], int_quad$repl== IDs[x] )
                                    if (nrow(ww)>0) {
                                                 sf::st_sample(sf::st_geometry(ww), size=n_pts, by_polygon=F) %>%  
                                                 sf::st_as_sf(.) }  
                                    }
                        ) %>% 
                 bind_rows(.)
        pt_shp<-rbind(pt_shp,pt_temp)


    }


    sf::st_write(quad_samp, dsn=paste0(dsn,'/',file_tag,'-pq-rep_quadrats.shp'), append=FALSE) # append=FALSE cause overwriting
    sf::st_write(pt_shp, dsn=paste0(dsn,'/',file_tag,'-pq-points_quadrats.shp'), append=FALSE) # append=FALSE cause overwriting
    sf::st_write(int_quad, dsn=paste0(dsn,'/',file_tag,'-pq-poly_quadrats.shp'), append=FALSE) # append=FALSE cause overwriting


}




# # #=====================
# # # bench data
# # #=====================
# # library(sf)
# dsn='E:/PPO_godbout/GIS/shapefiles/pt_quad'
# file_tag='temp'
# site_poly=NULL 
# n_quad=3
# n_pts=6
# seed=1294
# quadrat_size=1 
# class_lab='class'
# class_val=c( "Bloc ou galet","Sédiment mixte grossier") 

# shp<-sf::st_read('C:/Users/Utilisateur/Documents/Lavoro/DFO/GODBOUT_mosaics/data/validated shp/val_ph03_epsg32619.shp')
# # plot(sf::st_geometry(shp), col=as.factor(shp$class))
# # str(shp)


# # plot(st_geometry(int_quad[int_quad$repl==909,]), col=as.factor(int_quad[int_quad$repl==909,]$class), border=2 )
# # plot(st_geometry(pt_shp), add=T, col='white', pch=20)

