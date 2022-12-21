# ==============================================================================
# Title		:	density.ppp to a list
# Author	:	Filippo Ferrario
# Date		:	2021-02-14
# Version	:	0.1
# Aim		:	Apply density.ppp to element of the list "to_density".
# ==============================================================================



#' Apply density.ppp to element of a list of ppp
#' 
#' This function is to be used to create a variable (usually a predictor) using [density.ppp] for each replicate plot to be analyzed.
#' The intended use cases are:
#' - a set of ppp is to be used as response varable
#' - for each ppp, the analyst wants to produce a variable describing a given feature using [density.ppp], in replicate plots where another set of spatial objects (e.g., owin, ppp) has been observed. 
#' 	 However it is possible that the predictor ppp is not observed (i.e., a real zero and not to be considered missing data) in all the plots where the response ppp is available. 
#' 	 In this case the function produce an image whose pixels have all 0 value
#' 
#' @param to_density a solist of ppp for which density needs to be computed
#' @param W_from a list windows (objects of class "owin") or data acceptable to [as.owin], where to look for the Window of observation to assign to to_density elements
#' @param full_list optional. A character vector or a list of spatstat objects (e.g., owin or ppp) defining the complete set of replicates for which a variable need to be computed. To be specified if "full_list" is a subset of W_from to avoid superflouos calculations.
#' @param changeW logical (default =FALSE). If TRUE  and "W_from" is specified, the Window of the elements from "W_from" is used and will replace that of elements in "to_density" (this in case X is a ppp). If FALSE the window of "to_density" elements is kept.
#' @param addZeros logical. If TRUE (the default) and "W_from" is specified, the output will include elements with all pixels having value 0 when an element is in "W_from" but not in "to_density"
#' @param ... to pass extra arguments to [density.ppp]
#' 
#' @details
#' List "to_density" may be a subset of or longer list than "full_list" or "W_from". 
#' Only element from "to_density" also present in either "W_from" or "full_list" will be considered.
#' For those element of "full_list" not in "to_density", the function creates "0": the absence elements of list "full_list" from the "to_density" list is not to be considered as missing values but rather as 0.
#' For those element the function can create an im object in which the pixels value is set to zero.
#' 
#' The 'full_list' may be a subset of "W_from" in case not all the elements in "W_from" need to be used for the analysis. 
#' Lists are compared by names so technically the order should not be important.
#' 
#' If some elements in "to_density" are not contained in "full_list" (either not present or misspelled) a warning is given.
#' If "full_list" is specified, all its elements (either characters or names of a solist) must be included in "W_from" otherwise an error is thrown. 
#' 
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @examples 
#' 
#' @seealso
#' [density.ppp], [densityfun]
#' 
#' @return
#' an object of class 
#' 
#' @export 

density2list<-function (to_density=NULL,  W_from=NULL, full_list=W_from, changeW=FALSE, addZeros=TRUE, ...){
 		# check arguments
		if (is.null(to_density) | !'solist' %in% class(to_density)) stop('"to_density" must be a solist')
		if (!is.null(W_from) & !'solist' %in% class(W_from)) stop('"W_from" must be a solist or NULL')
		if (!is.null(full_list) & !'solist' %in% class(full_list) & ! is.character(full_list)) stop('full_list must be either a solist or a characher vector OR NULL')
		if (!is.logical(changeW)) stop('"changeW" must be logical')
		if (!is.logical(addZeros)) stop('"addZeros" must be logical')
		if ((changeW | addZeros ) & is.null(W_from) ) stop('Need "W_from" to change window or add zeros' )
		
		# coherce full_list to character
		if ('solist' %in% class(full_list)) full_list<-names(full_list)	
		
		# check names
		# If W_from is specified 
		if ('solist' %in% class(W_from)){	
				if (sum(full_list %in% names(W_from))<length(full_list) ) stop('Bad correspondence between names of "full_list" and "W_from"')
		        if (sum(names(to_density) %in% full_list)<length(names(to_density))) warning('Some names of to_density are not in full_list')
			}
		
		# if W_from is not specified then it means the function needs to be applied to all elements of to_density without changing Window of observation
		if (is.null(W_from)) {
			W_from<-to_density
			if (is.null(full_list)) {
				full_list<-names(W_from) 
			} else {
				if (sum(! full_list %in% names(to_density)) >=1) {
					warning('since "W_from is not specified, elements in "full_list" that are not in "to_density" will be ignored')
					full_list<-full_list[full_list %in% names(to_density)]
				}
			}
		}
	
		# add Zeros BUT Keep the original window (i.e., use W_from to just assign a window to new Zero elements)
		if (changeW==F & addZeros==T){
			# make W_from a new list composed of to_density elements and those or W_form not in to_density
			W_from_new<- c(to_density,W_from[!names(W_from) %in% names(to_density)])
			#subset W_from_new to match full_list (mainly in case full list is character or a subset of W_from)
			W_from_new<-W_from_new[names(W_from_new) %in% full_list]
			W_from<-W_from_new
		}	
		
		# do not addZero
		if (addZeros==FALSE) full_list<-full_list[full_list %in% names(to_density)]


		qq<-lapply(full_list , FUN=function(K) { 
				# browser()S
			    Y<-W_from[names(W_from) %in% K]
				if (sum(names(to_density) == K) ==1){
						x<-to_density[K]
						# If changeW if TRUE then change W of ppp before density because otherwise density.ppp ignore W
						if (changeW) {Window(x[[1]])<-as.owin(Y[[1]])}
						# res<-densityfun(X=x[[1]], W=Y[[1]],... )  #... eps, sigma, diggle, edge
						res<-density.ppp(x=x[[1]], ... )  #... eps, sigma, diggle, edge
				}
				if  (sum(names(to_density) == K) ==0){
					temp_ppp<-ppp(x=NULL,y=NULL,window=as.owin(Y[[1]]))
					# res<-densityfun(X=temp_ppp, W=temp_ppp,... )  #... eps, sigma, diggle, edge
					res<-density.ppp(x=temp_ppp, ... )  #... eps, sigma, diggle, edge	
				}
				res		

			})

		names(qq)<-full_list
		qq<-as.solist(qq)
		qq
}


# bench 
# --------

# ############ TEST #################

# library(spatstat)

# # =============================
# # load data
# # =============================
# setwd('./Lavoro/LAVAL/benthos_spatial')

# load('./data/data_for_spatstat-green_urchin.R')
# load('./data/data_for_spatstat-rocks.R')
# load('./data/data_for_spatstat-sand_dollars.R')

# # =============================
# # Study
# # =============================

# rk<-rock_win
# ur<-urc_ppp
# sd<-sand_dols[1:4]
# nam<-names(sd)

# ############################
# # bench test change WINDOW in density.ppp : Apparently setting W whithin density.ppp does not work
# dsd<-density.ppp(sd$'EISS02+6',sigma=1,eps=0.1)
# dsdW<-density.ppp(sd$'EISS02+6',sigma=1,eps=0.1, W=Window(ur$'EISS02+6'))
# pr<-sd$'EISS02+6'
# Window(pr)<-as.owin(ur$'EISS02+6')
# dpr<-density(pr,sigma=1,eps=0.1)

# par(mfrow=c(1,3))
# plot(dsd,useRaster=F)
# 	plot(Window(sd$'EISS02+6'),add=T, border=5, lwd=2)
# 	plot(Window(ur$'EISS02+6'),add=T, border=1, lwd=2)
# plot(dsdW,useRaster=F)
# 	plot(Window(sd$'EISS02+6'),add=T, border=5, lwd=2)
# 	plot(Window(ur$'EISS02+6'),add=T, border=1, lwd=2)
# plot(dpr,useRaster=F)
# 	plot(Window(sd$'EISS02+6'),add=T, border='white', lwd=2)
# 	plot(Window(ur$'EISS02+6'),add=T, border=1, lwd=2)
# ############################

# names(sd)
# names(ur[1:5])

# # case 4 point pattern full_list as chatacter  keep window and add zero
# res4<-density2list(to_density=sd, W_from=ur[1:5] , sigma=0.1, eps=0.1)
# plot(res4, useRaster=F)
# res4a<-density2list(to_density=sd, W_from=ur, full_list=c('EISS02+6','EISS05+3') , sigma=0.1, eps=0.1)
# plot(res4a, useRaster=F)
# # case keep window DO NOT ADD ZERO
# res4b<-density2list(to_density=sd, W_from=ur ,addZeros=F, sigma=0.1, eps=0.1)
# plot(res4b, useRaster=F)

# # case 5 point pattern full_list as chatacter WITHOUT W_from (a.k.a., no window change, no add zeros)
# res5<-density2list(to_density=sd, changeW=F, addZeros=F,sigma=0.1, eps=0.1)
# plot(res5, useRaster=F)

# # case 6 point pattern no window Change But AddZeros
# res6<-density2list(to_density=sd,W_from=ur[1:5], changeW=F, addZeros=T,sigma=0.1, eps=0.1)
# plot(res6, useRaster=F)

# # case 7 point pattern Window Change AND AddZeros
# res7<-density2list(to_density=sd,W_from=ur[1:5], changeW=T, addZeros=T,sigma=0.1, eps=0.1)
# plot(res7, useRaster=F)


# # case 8 point pattern Window Change AND NOT AddZeros
# res8<-density2list(to_density=sd,W_from=ur[1:5], changeW=T, addZeros=F,sigma=0.1, eps=0.1)
# plot(res8, useRaster=F)
