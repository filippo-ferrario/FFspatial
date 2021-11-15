# ===================================================================
# Title		:	Windows to list of Planar point patters
# Author	:	Filippo Ferrario
# Date		:	2020-03-04
# Version	:	0.1
# Aim		:	function that assign a window from a list to the correct ppp in another list
#				It should work either when the windows list is as long or longer than the ppp list, or vice versa.	
# ===================================================================

#' Function that assign a window from a list to the correct ppp in another list
#'
#' The function match the names of the objects in the two lists to make sure that the correct Window is assigned to the corresponding ppp. 
#'
#' @param X list (solist) of ppp objects to which change the Window.
#' @param W list (solist) of Window objects 
#'
#' @details	
#'
#' It should work either when the windows list is as long or longer than the ppp list, or vice versa.	
#'
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @examples 
#' 
#' @seealso
#' [spatstat.geom::solist], [spatstat.geom::Window]
#' 
#' 
#' @export 


win2ppp<-function(X,W){

	if (length(W)>=length(X)) {
			indx<-names(W) %in% names(X)
			W<-W[indx] 
		} else { 
			indx<-names(X) %in% names(W)
			X<-X[indx] 
		}
# check that the order of the windows in the list is the same of the ppp
	chk<-sum(names(W)==names(X))	
	if  (chk!= length(X)) stop ('The lists do not follow the same order rule') 
	if  (chk== length(X)) print('The two lists follow the same order: matching windows to ppp will be correct!') 
# assign the correct window to each urchin point pattern
	for (i in 1:length(X)) spatstat.geom::Window(X[[i]])<-W[[i]] 
	X
}
