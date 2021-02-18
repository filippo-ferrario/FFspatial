# ===================================================================
# Title		:	Cluster Boostrap sampling
# Author	:	Filippo Ferrario
# Date		:	2020-07-22
# Version	:	0.1
# Aim		:	function to be produce resampled dataset from original dataset when clustering (i.e. groups) 
# 				are present. 
# 				based on paper and its supplementaty material:
# 				Bouwmeester, W., K. G. M. Moons, T. H. Kappen, W. A. van Klei, J. W. R. Twisk, M. J. C. Eijkemans, and Y. Vergouwe. 2013. 
# 				Internal Validation of Risk Models in Clustered Data: A Comparison of Bootstrap Schemes. 
# 				American Journal of Epidemiology 177:1209–1217.
# 				Originally coded for the Habitat Loss Offsetting (HLO) Project with Ladd Johnson.
# ===================================================================



##########################################################
# Resampling the dataset for Cluster bootstrap.
###########################################################

#' Cluster Boostrap sampling
#' 
#' This function resample a dataset where observation are grouped by one of its variables. Internally used by [spt_glmmPQL_bts_val]
#' 
#' @param dataset is the dataset to be resampled
#' @param column_obs is the column of the dataset containing the obsevations. must be in the format dataset$observation
#' @param column_group is the column of the dataset containing the groups (cluster). must be in the format dataset$group
#' 
#' 
#' @details
#' The function is a modified version of the cluster bootstrap scheme coded by Bouwmeester et al 2012
#' and available on-line at  http://aje.oxfordjournals.org/content/suppl/2013/05/09/kws396.DC1/kws396_Web_Material.pdf
#' 
#' This modified version is only producing the resampled dataset.
#' 
#' @references
#' Bouwmeester, W., K. G. M. Moons, T. H. Kappen, W. A. van Klei, J. W. R. Twisk, M. J. C. Eijkemans, and Y. Vergouwe. 2013. Internal Validation of Risk Models in Clustered Data: A Comparison of Bootstrap Schemes. American Journal of Epidemiology 177:1209–1217.
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @seealso 
#' [spt_glmmPQL_bts_val]
#' 
#' @export

boot.sample<-function(dataset,column_obs,column_group)
{ 
	# A study sample generated from a source population. 
	study_sample<- dataset
	# The number of patients in the study sample (= the number of patients in the bootstrap sample). 
	# N_samples<- length(study_sample[,1]) 
	# Construct a vector with the number of observation per group in the study sample. 
	tmp <- aggregate(column_obs, list(group = column_group), FUN=length) 
	N_samples_group <- as.vector(tmp[,2]) 
	# group IDs (e.g. 1-100). 
	names(N_samples_group) <- tmp[,1] 


	# Draw group IDs with replacement (i.e. bootstrapping). 
	bootstrapped_group_IDs <- sample(names(N_samples_group), length(names(N_samples_group)), replace = TRUE) 

	# Include in the bootstrap sample all samples which belong to the bootstrapped group. Make a local vector (loc.N_samples_group) which includes the number of samples per group in the bootstrap sample. 
	loc.N_samples_group <- N_samples_group[bootstrapped_group_IDs] 

	# Make a data frame which will include the data of the bootstrapped groups, where the number of rows is adjusted to the number of samples in the bootstrap sample. 
	bootstrap_sample <-as.data.frame(matrix( nrow=0, ncol=ncol(study_sample)))  
	colnames(bootstrap_sample) <- colnames(study_sample) 

	# Collect the data which belong to the bootstrapped samples. 
	for (i in 1:length(bootstrapped_group_IDs)) { 
	bootstrap_sample<-rbind(bootstrap_sample,study_sample[column_group==names(loc.N_samples_group)[i],])
	} 
	return(bootstrap_sample)

}

