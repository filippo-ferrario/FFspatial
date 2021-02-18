# ==============================================================================
# Title		:	Focal stats to spatstats list
# Author	:	Filippo Ferrario
# Date		:	2021-02-14
# Version	:	0.1
# Aim		:	Apply spstFocal to the elements of the list "to_focal" in which not all the element may have polygonal windows (or points) to be converted in data.
# ==============================================================================




#' Apply spstFocal to the elements of the list "to_focal" with potential indtroduction of real zeros.
#' 
#' This function is to be used to create a variable (usually a predictor)  using [spstFocal] for each replicate plot to be analyzed.
#' The intended use case:
#' - a set of ppp is to be used as response varable
#' - for each ppp, the analyst wants to produce a response variable using a focal statistic based on another set of spatial objects (e.g., owin, ppp) describing a given feature. 
#' 	 However it is possible that that feature is not observed (i.e., a real zero and not to be considered missing data) in all the plots where the response ppp is available. 
#' 	 In this case the function produce an image whose pixels have all 0 value.
#' 
#' 
#' @param to_focal a list of spatstat objects (e.g., owin or ppp) to which apply the focal function. 
#' @param W_from  optional if to_focal is a list of ppp. A list windows (objects of class "owin") or data acceptable to [spatstat::as.owin], where to look for the Window of observation to assign to to_focal elements.
#' @param full_list optional. A character vector or a list of spatstat objects (e.g., owin or ppp) defining the complete set of replicates for which a variable need to be computed using [spstFocal]. To be specified if "full_list" is a subset of W_from to avoid superflouos calculations.
#' @param changeW logical. If TRUE (the default) and "W_from" is specified, the Window of the elements from "W_from" is used and will replace that of elements in "to_focal" (this in case X is a ppp). If FALSE the window of "to_focal" elements is kept.
#' @param addZeros logical. If TRUE (the default) and "W_from" is specified, the output will include elements with all pixels having value 0 when an element is in "W_from" but not in "to_focal"
#' @param ... agruments to be passed to [spstFocal] and [raster::focal] (In particular `fun`, `na.rm`, `pad` and `padValue`).
#' 
#' @details
#' 
#' List "to_focal" may be a subset of or longer list than "full_list" or "W_from". Only element from "to_focal" also present in either "W_from" or "full_list" will be considered.
#' For those element of "full_list" not in "to_focal", the function creates "0": the absence elements of list "full_list" from the "to_focal" list is not to be considered as missing values but rather as 0.
#' For those element the function can create an im object in which the pixels value is set to zero.
#' 
#' The 'full_list' may be a subset of "W_from" in case not all the elements in "W_from" need to be used for the analysis. 
#' Lists are compared by names so technically the order should not be important.
#' 
#' If some elements in "to_focal" are not contained in "full_list" (either not present or misspelled) a warning is given.
#' If "full_list" is specified, all its elements (either characters or names of a solist) must be included in "W_from" otherwise an error is thrown. 
#' 
#' @notes
#' 
#' ***ATTENTION***
#' Verify to check the value of the argument DivdideByPixelArea passed to [spatstat::pixellate] via [spstFocal].
#' Default is FALSE!
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @examples 
#' 
#' @seealso
#' [spstFocal], [spatstat::pixellate]
#' 
#' @return
#' an object of class "imlist"  "solist"  "anylist" "listof"  "list" 
#' 
#' @export 


focal2list<-function (to_focal=NULL, W_from=NULL, full_list=W_from, changeW=TRUE, addZeros=TRUE,...){
		
		# check arguments
		if (is.null(to_focal) | !spatstat::is.solist(to_focal)) stop('"to_focal" must be a solist')
		if (!is.null(W_from) & !spatstat::is.solist(W_from)) stop('"W_from" must be a solist or NULL')
		if (!is.null(full_list) & !spatstat::is.solist(full_list) & ! is.character(full_list)) stop('full_list must be either a solist or a characher vector OR NULL')
		if (!is.logical(changeW)) stop('"changeW" must be logical')
		if (!is.logical(addZeros)) stop('"addZeros" must be logical')
		if ((changeW | addZeros ) & is.null(W_from) ) stop('Need "W_from" to change window or add zeros' )
		
		# coherce full_list to character
		if (spatstat::is.solist(full_list)) full_list<-names(full_list)	
		
		# check names
		# If W_from is specified 
		if (spatstat::is.solist(W_from)){	
				if (sum(full_list %in% names(W_from))<length(full_list) ) stop('Bad correspondence between names of "full_list" and "W_from"')
		        if (sum(names(to_focal) %in% full_list)<length(names(to_focal))) warning('Some names of to_focal are not in full_list')
			}
		
		# if W_from is not specified then it means function needs to be applied to all elements of to_focal without changing Window of observation
		if (is.null(W_from)) {
			W_from<-to_focal
			if (is.null(full_list)) {
				full_list<-names(W_from) 
			} else {
				if (sum(! full_list %in% names(to_focal)) >=1) {
					warning('since "W_from is not specified, elements in "full_list" that are not in "to_focal" will be ignored')
					full_list<-full_list[full_list %in% names(to_focal)]
				}
			}
		}
		
		# add Zeros BUT Keep the original window (i.e., use W_from to just assign a window to new Zero elements)
		if (changeW==F & addZeros==T){
			# make W_from a new list composed of to_focal elements and those or W_form not in to_focal
			W_from_new<- c(to_focal,W_from[!names(W_from) %in% names(to_focal)])
			#subset W_from_new to match full_list (mainly in case full list is character or a subset of W_from)
			W_from_new<-W_from_new[names(W_from_new) %in% full_list]
			W_from<-W_from_new
		}	
		
		# do not addZero
		if (addZeros==FALSE) full_list<-full_list[full_list %in% names(to_focal)]
		#core function
		qq<-lapply(full_list , FUN=function(K) { 
			    Y<-W_from[names(W_from) %in% K]
				if (sum(names(to_focal) == K) ==1){
						x<-to_focal[K]
						res<-spstFocal(X=x[[1]], W_from=Y[[1]]  ,... )  # , dist=0.5, side.cell=0.1, pad=T, padValue=NA, fun=mean,na.rm=T
				}
				if  (sum(names(to_focal) == K) ==0){
					res<-spatstat::as.im(spatstat::Window(Y[[1]]), value=0)		
				}
				res		

			})

		names(qq)<-full_list
		qq<-spatstat::as.solist(qq)
		qq
}

# bench 
# --------

# ############ TEST #################

# library(spatstat)

# # =============================
# # load data
# # =============================

# load('./data/data_for_spatstat-green_urchin.R')
# load('./data/data_for_spatstat-rocks.R')
# load('./data/data_for_spatstat-sand_dollars.R')

# # =============================
# # Study
# # =============================

# rk<-rock_win[1:4]
# nam<-names(rk)
# ur<-urc_ppp
# sd<-sand_dols

# to_focal=bushy 
# to_focal=kelp
# full_list=pref1
# W_from=urc_ppp
# dist=0.5 
# side.cell=0.1
# pad=T 
# padValue=NA
# fun=mean
# na.rm=T

# # case 1  rk fully contained in ur and no missing data, rk has an extra element which is not used (generate warning)
# res1<-focal2list(to_focal=rk, W_from=ur[names(ur) %in% nam], dist=0.5,side.cell=0.1, DivideByPixelArea=T,fun=mean,na.rm=T, pad=T, padValue=NA)
# res1<-focal2list(to_focal=rk, W_from=ur[names(ur) %in% nam],full_list=c('EISS02+6','EISS02+8'), dist=0.5,side.cell=0.1, DivideByPixelArea=T,fun=mean,na.rm=T, pad=T, padValue=NA)
# 	par(mfrow=c(1,3))
# lapply(names(res1), function(x) {
# 		plot(res1[x][[1]], useRaster=F)
# 		plot(rk[x][[1]], add=T, border='white')
# })

# par(mfrow=c(1,1))
# plot(res1[1][[1]],useRaster=F)
# plot(rk[names(res1[1])][[1]], add=T, border='white')


# # zoom
# xy<-locator()
# plot(res1[1][[1]], xlim=c(min(xy$x),max(xy$x)), ylim=c(min(xy$y),max(xy$y)),main='', useRaster=F)
# plot(rk[names(res1[1])][[1]], add=T, border='white')

# # case 2: rk fully contained in ur and presence of missing data (i.e. ur has more elements than rk), rk has an extra element which is not used (generate warning)
# res2<-focal2list(to_focal=rk, W_from=ur, dist=0.5,side.cell=0.1, fun=mean,na.rm=T, pad=T, padValue=NA)
# plot(res2, useRaster=F)
# res2b<-focal2list(to_focal=rk, W_from=ur, changeW=F, dist=0.5,side.cell=0.1, fun=mean,na.rm=T, pad=T, padValue=NA)
# windows();plot(res2b, useRaster=F)

# res2c<-focal2list(to_focal=rk, W_from=ur, addZeros=F, dist=0.5,side.cell=0.1, fun=mean,na.rm=T, pad=T, padValue=NA)
# plot(res2c, useRaster=F)

# # case 3 full_list as chatacter 
# res3<-focal2list(to_focal=rk, W_from=ur, full_list=c('EISS02+6','EISS19+BC') ,  dist=0.5,side.cell=0.1, fun=mean,na.rm=T, pad=T, padValue=NA)
# plot(res3, useRaster=F)


# # case 4 point pattern full_list as chatacter 
# res4<-focal2list(to_focal=sd, W_from=ur, full_list=c('EISS02+6','EISS05+3') ,  dist=0.5,side.cell=0.1,DivideByPixelArea=F, preserve=T, fun=mean,na.rm=T, pad=T, padValue=NA)
# plot(res4, useRaster=F)

# # case 5 point pattern full_list as chatacter WITHOUT W_from (a.k.a., no window change, no add zeros)
# res5<-focal2list(to_focal=sd, changeW=F, addZeros=F,dist=0.5,side.cell=0.1,DivideByPixelArea=F, preserve=T, fun=mean,na.rm=T, pad=T, padValue=NA)
# plot(res5, useRaster=F)


# # case 6 point pattern no window Change But AddZeros
# res6<-focal2list(to_focal=sd[1:5],W_from=ur[1:5], changeW=F, addZeros=T,dist=0.5,side.cell=0.1,DivideByPixelArea=F, preserve=T, fun=mean,na.rm=T, pad=T, padValue=NA)
# plot(res6, useRaster=F)


# # case 7 point pattern Window Change AND AddZeros
# res7<-focal2list(to_focal=sd[1:5],W_from=ur[1:5], changeW=T, addZeros=T,dist=0.5,side.cell=0.1,DivideByPixelArea=F, preserve=T, fun=mean,na.rm=T, pad=T, padValue=NA)
# plot(res7, useRaster=F)


# # case 8 point pattern Window Change AND NOT AddZeros
# res8<-focal2list(to_focal=sd[1:5],W_from=ur[1:5], changeW=T, addZeros=F,dist=0.5,side.cell=0.1,DivideByPixelArea=F, preserve=T, fun=mean,na.rm=T, pad=T, padValue=NA)
# plot(res8, useRaster=F)
