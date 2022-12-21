# ===================================================================
# Title   : Internal Validation with bootstrap for Spatial point process
# Author  : Filippo Ferrario
# Date    : 2020-12-03
# Version : 0.2
# Aim     : Function to fit models in a set of GLMM PQL models to both whole dataset and replicate bootstapped datasets.
#           The function extract the model parameters, RMSE and R2 (marginal and conditionals) of each model runs, 
#           and calculate the RMSE optimism at each run.
#           Extracted info are stored in a dataframe.
#           
#           hackglmmPQL is set to mimic the fitting done for MIXED models by mppm           
#           
#           Based on :
#            Bouwmeester, W., K. G. M. Moons, T. H. Kappen, W. A. van Klei, J. W. R. Twisk, M. J. C. Eijkemans, and Y. Vergouwe. 2013. Internal Validation of Risk Models in Clustered Data: A Comparison of Bootstrap Schemes. American Journal of Epidemiology 177:1209–1217.  
#            Harrell, F. E. 2015. Regression Modeling Strategies. Springer International Publishing, Cham. [Chapter 5]     
# ===================================================================



##########################################################
# Resampling the dataset for Cluster bootstrap.
###########################################################


# # load boot.sample function (for cluster bootstrap)
# source('~/Lavoro/R_functions/MISC-data_analysis/cluster_bootstrap_sampling-v0.1.R')

#' Internal Validation with bootstrap for Spatial point process
#' 
#' Function to fit models in a set to both whole dataset and replicate bootstapped datasets using GLMM PQL.
#' The function extract the model parameters, RMSE and R^2^ (marginal and conditionals) of each model runs, and calculate the RMSE optimism at each run.
#' 
#' @param mod_fmla character vectror with formulas of the models to be fit. It must be a named vector regardless of its length.
#' @param random_fmla string with random formula of the models to be fit
#' @param Y_col character name of the column with response variables
#' @param B number of bootstrap samples to be taken. If 0 no bootstrap is performed and the model fitted on the whole dataset returned.
#' @param dataset full dataset
#' @param column_group character name of the dataset column with grouping.
#' @param RAN.pred logical. Specify if using or not the random factor to obtain predictions. Defaults to FALSE. see Details and [nlme::predict.lme]
#' @param reltol as in mppm.
#' @param ... used to pass arguments to [MuMIn::r.squaredGLMM] (e.g. pj2014)
#' 
#' @details
#' The function fits the models in the model set `mod_fmla` to the origial dataframe and to each of the boostrapped samples.
#' This function should be used to produce the dataframe from which calculate the average optimism and correct the model fitted on the original dataset.
#' 
#' Cluster Boostrap sampling is performed using [boot.sample]. 
#' 
#' Models are fitted using [hackglmmPQL] with family set to quasipoisson(link = log) to allow using [MuMIn::r.squaredGLMM].     
#' 
#' 
#' @seealso
#' [boot.sample], [MuMIn::r.squaredGLMM], [mppm]
#' 
#' @return
#' A dataframe where each line is a parameter of one particular model in the model set, and the averaged one (TO BE IMPLEMENTED), for each iteration
#' The column 'type' state if a row refer to the original dataframe or a bootstrap sample.
#' 
#' The `RAN.pred` argument determines on which predictions the RMSE is calculated (i.e. including or not the Random factor). It does not affect the R^2^ values.
#' 
#' TO BE IMPLEMENTED: At each iteration the models in the model set are also averaged and the parameter of the shrinked model saved.
#' 
#' For using Random factor when obtaining predictions please see Bouwmeester et al 2013.
#' 
#' @references
#' Bouwmeester, W., K. G. M. Moons, T. H. Kappen, W. A. van Klei, J. W. R. Twisk, M. J. C. Eijkemans, and Y. Vergouwe. 2013. Internal Validation of Risk Models in Clustered Data: A Comparison of Bootstrap Schemes. American Journal of Epidemiology 177:1209–1217.  
#' Harrell, F. E. 2015. Regression Modeling Strategies. Springer International Publishing, Cham. (Chapter 5)
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @export  


spt_glmmPQL_bts_val<-function( mod_fmla=NULL, random_fmla=NULL, Y_col=NULL, B=200,dataset=NULL,column_group=NULL, RAN.pred=FALSE, reltol=0.001, ...)

{
  # checks
  if (!is.character(column_group)) stop('column_group must be the name of the dataset column with grouping.')
  if (is.null(names(mod_fmla))) stop('mod_fmla must be a NAMED vector. see help. ')

  # Function to fit models with hackglmmPQL
  myPQL<-function(fmla=NULL, df_to_fit=NULL)
  	{ 
      hackglmmPQL(fixed = as.formula(fmla), random =as.formula(random_fmla), 
  		     	  	  family = quasipoisson(link = log),
  					  data=df_to_fit, weights = .mpl.W * caseweight, 
  					  control = attr(fixed, "ctrl"), reltol=reltol)
  	}

  # function to extract info form list of models to a dataframe
  model_to_dataframe<- function(model_list=NULL, frame_template=NULL, obs_dataset=NULL , Y_col=NULL, Bootstrap=TRUE,  RAN.pred, ...) 
  {
    res.temp<-frame_template
    for (i in 1: length(model_list)){
                  # select model and extract info from it
                  mod<-model_list[i][[1]]
                  smry<-summary(mod)
                  betas<- coef(smry)[,1]
                  betas_name<-names(betas)
                  # performance training
                  lev.pr<-ifelse(RAN.pred==FALSE,0,1)
                  pr.train<- stats::predict(mod, level=lev.pr )  # set to 0 To match re.form=NA in Bouwmeester et al 2013. see ?predict.lme : Level values increase from outermost to innermost grouping, with level zero corresponding to the population predictions. 
                  fit.train<-exp(pr.train) # to check that is correct : sum ((fit.train) != predict(mod, re.form=NA, type='response' ) ) 
                  Y.train<-mod$data[,Y_col]
                  rmse.train<- sqrt( mean( (fit.train - Y.train)^2 ))
                  r2.train<-MuMIn::r.squaredGLMM(mod, envir=e, ...)
                  # prepare info to be stored
                  if(is.null(betas_name)) betas_name<-'(Intercept)'
                  aic<-function(m=mod) { # written from code in mppm, logLik and AIC.mppm fixef.mppm
                           deviants <- -2 * logLik(m)
                           W <- with(obs_dataset, .mpl.W * caseweight)
                           SUBSET <- obs_dataset$.mpl.SUBSET
                           Z <- (obs_dataset$.mpl.Y != 0)
                           maxlogpl<- -(deviants/2 + sum(log(W[Z & SUBSET])) + sum(Z & SUBSET))
                           ll <- logLik(maxlogpl)
                           pen <- length(coef(m))
                           
                           (-2 * as.numeric(ll) + 2 * pen)

                          }
                  aic_v<-aic(mod)
                  param<-c(betas_name,'rmse','r2_mar','r2_cond','AIC') 
                  vals<-c(betas,rmse.train,
                                r2.train[row.names(r2.train)=='delta', 'R2m'], # see ?MuMIn::r.squaredGLMM : "The delta method can be used with for all distributions and link functions")
                                r2.train[row.names(r2.train)=='delta', 'R2c'],
                                AIC=aic_v) 
                  # performance test
                  if (Bootstrap==TRUE){
                                    pr.test<-stats::predict(mod, newdata=obs_dataset, level=0) # set to 0 To match re.form=NA in Bouwmeester et al 2013. see ?predict.lme : Level values increase from outermost to innermost grouping, with level zero corresponding to the population predictions. 
                                    fit.test<-exp(pr.test)
                                    Y.test<-obs_dataset[,Y_col]
                                    rmse.test<- sqrt( mean( (fit.test - Y.test)^2 ))
                                    optimism<- rmse.train-rmse.test
                                    # prepare info to be stored
                                    param<-c(param,'rmse_test','rmse_optimism')
                                    vals<-c(vals,rmse.test,optimism) 
                                  }
                  # Store information in dataframe

                  tmp2<-data.frame(
                    model=names(model_list[i]),
                    formula=as.character(formula(mod))[3] ,
                    type= ifelse(Bootstrap==TRUE,'bootstrap','observed'),
                    parameter=param,
                    value=vals  )
                  res.temp<-rbind(res.temp,tmp2)
      }
  return( res.temp)    
  }
  # Check that the formulas is a named vector
  if (is.null(names(mod_fmla))) stop('vector of formula must be named!')
  # initialize dataframe where to store results
  res<-as.data.frame(matrix(nrow=0,ncol=5),row.names=NULL)
  # res<-data.frame() 
  names(res)<-c('model','formula','type','parameter', 'value')
  # create new environmnet to store df_to_ hoping it works for r.sqaredGLMM
  e<-new.env()
  # fit models to full dataset
  df_to_fit<-dataset
  e$df_to_fit<-df_to_fit
  M0<-lapply(mod_fmla, FUN=function(x) myPQL(fmla=x, df_to_fit) )
  # browser()
  # extract info for model fitted on full dataset and store values
  infoM0<-model_to_dataframe(model_list=M0, frame_template=res, obs_dataset=dataset, Y_col=Y_col, Bootstrap=FALSE, RAN.pred,...)

  if (B==0){ 
     return(infoM0)
   } else {
     # initialize temporary dataframe for storing bootstraped results
     res.boot<-res
     # Loop for bootstrap
     for (b in 1:B){
      tryCatch({
                 # draw a sample dataset
                 boot.dat<-boot.sample(dataset,dataset[,Y_col],dataset[,column_group])
                 # fit model to bootstrapped sample
                 df_to_fit<- boot.dat
                 e$df_to_fit<-df_to_fit
                 M1<-lapply(mod_fmla, FUN=function(x) myPQL(fmla=x, df_to_fit) )
                 # extract info from boostrapped model and store them in a dataframe
                 info<-model_to_dataframe(model_list=M1, frame_template=res, obs_dataset=dataset, Y_col=Y_col, Bootstrap=TRUE, RAN.pred=FALSE,...)  # set to RAN.pred= FALSE To match re.form=NA in Bouwmeester et al 2013. see ?predict.lme : Level values increase from outermost to innermost grouping, with level zero corresponding to the population predictions.  
                 warning('when bootstrapping RAN.pred is set to FALSE to mimic "re.form=NA" following Bouwmeester et al 2013:\n !!! predictions for both train and Test RMSE do not consider random  factor !!! ')
                 res.boot<-rbind(res.boot, info)
               }, error=function(e){})
     }    
   
     out<-rbind(infoM0,res.boot)
     return(out)
   }
}

# Changes:
# 2020-10-19 Added tryCatch in the for loop of the bootstrap to allow model fitting failures
# 2021-12-02 replaced MASS::glmmPQL with hackglmmPQL and added retol as function argument
# 2021-12-03 Added AIC formula