# ===================================================================
# Title   : Internal Validation with bootstrap.
# Author  : Filippo Ferrario
# Date    : 2020-07-24
# Version : 0.1
# Aim     : Function to fit models in a set of GLM models to both whole dataset and replicate bootstapped datasets.
#           The function extract the model parameters, RMSE and R^2^ of each model runs, 
#           and calculate the RMSE optimism at each run.
#           Extracted info are stored in a dataframe.
#           BOOTSTRAP: normal boostrap
#
#           GLM is set to mimic the fitting done for Fixed models by mppm           
#           
#           Based on :
#            Bouwmeester, W., K. G. M. Moons, T. H. Kappen, W. A. van Klei, J. W. R. Twisk, M. J. C. Eijkemans, and Y. Vergouwe. 2013. Internal Validation of Risk Models in Clustered Data: A Comparison of Bootstrap Schemes. American Journal of Epidemiology 177:1209–1217.  
#            Harrell, F. E. 2015. Regression Modeling Strategies. Springer International Publishing, Cham. (Chapter 5)     
# ===================================================================



##########################################################
# Resampling the dataset for Cluster bootstrap.
###########################################################


#' Internal Validation with bootstrap
#' 
#' Function to fit a set of GLM models to both whole dataset and replicate bootstapped datasets.
#' The function extract the model parameters, RMSE and R^2^ of each model runs, and calculate the RMSE optimism at each run.
#' 
#' @param mod_fmla character vectror with formulas of the models to be fit. It must be a named vector regardless of its length.
#' @param Y_col character name of the column with response variables
#' @param B number of bootstrap samples to be taken. If 0 no bootstrap is performed and the model fitted on the whole dataset returned.
#' @param dataset full dataset
#' @param ... not used
#' 
#' @details
#' The function fits the models in the model set `mod_fmla` to the origial dataframe and to each of the boostrapped samples.
#' Bootstrap sampling is performed internally.
#' [TO BE IMPLEMENTED] At each iteration the models in the model set are also averaged and the parameter of the shrinked model saved.
#' 
#' The R^2^ is calculated using [MuMIn::r.squaredGLMM]
#'  
#' @return
#' A dataframe from which calculate the average optimism to be used to correct the model fitted on the original dataset.
#' Each line is a parameter of one particular model in the model set, and the averaged one[TO BE IMPLEMENTED], for each iteration
#' The column 'orig' state if a row refer to the original dataframe or a bootstrap sample.
#'
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @export 
#' 
#' 

spt_glm_bts_val<-function( mod_fmla=NULL, Y_col=NULL, B=200,dataset=NULL,...)

{
  # checks
  if (is.null(names(mod_fmla))) stop('mod_fmla must be a NAMED vector. see help. ')

  # Function to fit models with GLM
  myGLM<-function(fmla=NULL, df_to_fit=NULL)
  	{ 
       glm(formula = as.formula(fmla),
                data= df_to_fit, family = quasipoisson(link = log), weights = .mpl.W * caseweight) 
  	}

  # function to extract info form list of models to a dataframe
  model_to_dataframe<- function(model_list=NULL, frame_template=NULL, obs_dataset=NULL , Y_col=NULL, Bootstrap=TRUE) 
  {
    res.temp<-frame_template
    for (i in 1: length(model_list)){
                  # select model and extract info from it
                  mod<-model_list[i][[1]]
                  smry<-summary(mod)
                  betas<- coef(smry)[,1]
                  betas_name<-names(betas)
                  # performance training
                  pr.train<- predict(mod )  # To match re.form=NA in Bouwmeester et al 2013. see ?predict.lme : Level values increase from outermost to innermost grouping, with level zero corresponding to the population predictions. 
                  fit.train<-exp(pr.train) # to check that is correct : sum ((fit.train) != predict(mod, re.form=NA, type='response' ) ) 
                  Y.train<-mod$data[,Y_col]
                  rmse.train<- sqrt( mean( (fit.train - Y.train)^2 ))
                  r2.train<-MuMIn::r.squaredGLMM(mod, envir=e)
                  # prepare info to be stores
                  if(is.null(betas_name)) betas_name<-'(Intercept)'
                  aic<-function(m=mod) { # written from code in mppm, logLik and AIC.mppm
                           deviants <- deviance(m)
                           W <- with(obs_dataset, .mpl.W * caseweight)
                           SUBSET <- obs_dataset$.mpl.SUBSET
                           Z <- (obs_dataset$.mpl.Y != 0)
                           maxlogpl<- -(deviants/2 + sum(log(W[Z & SUBSET])) + sum(Z & SUBSET))
                           ll <- maxlogpl
                           pen <- length(coef(m))
                           (-2 * as.numeric(ll) + 2 * pen)
                          }
                  aic_v=aic(mod) 
                  param<-c(betas_name,'rmse','r2_mar','AIC') 
                  vals<-c(betas,rmse.train,
                                r2.train[row.names(r2.train)=='delta', 'R2m'],  # see ?MuMIn::r.squaredGLMM : "The delta method can be used with for all distributions and link functions")                        
                                AIC=aic_v)
                  # performance test
                  if (Bootstrap==TRUE){
                                    pr.test<-predict(mod, newdata=obs_dataset)
                                    fit.test<-exp(pr.test)
                                    Y.test<-obs_dataset[,Y_col]
                                    rmse.test<- sqrt( mean( (fit.test - Y.test)^2 ))
                                    optimism<- rmse.train-rmse.test
                                    # prepare info to be stores
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

  # initialize dataframe where to store results
  res<-as.data.frame(matrix(nrow=0,ncol=5),row.names=NULL)
  names(res)<-c('model','formula','type','parameter', 'value')
  # create new environmnet to store df_to_ hoping it works for r.sqaredGLMM
  e<-new.env()
  # fit models to full dataset
  df_to_fit<-dataset
  e$df_to_fit<-df_to_fit
  M0<-lapply(mod_fmla, FUN=function(x) myGLM(fmla=x, df_to_fit) )
  # browser()
  # extract info for model fitted on full dataset and store values
  infoM0<-model_to_dataframe(model_list=M0, frame_template=res, obs_dataset=dataset, Y_col=Y_col, Bootstrap=FALSE) 

  if (B==0){ 
     return(infoM0)
    } else{
    
      # initialize temporary dataframe for storing bootstraped results
      res.boot<-res
      # Loop for bootstrap
        sample_size<-nrow(dataset)
      for (b in 1:B){
           tryCatch({
                # draw a sample dataset
                sample_id<-sample(x=c(1:sample_size), size=sample_size, replace=TRUE )
                boot.dat<- dataset[sample_id,]
                # fit model to bootstrapped sample
                df_to_fit<- boot.dat
                e$df_to_fit<-df_to_fit
                M1<-lapply(mod_fmla, FUN=function(x) myGLM(fmla=x, df_to_fit) )
                # extract info from boostrapped model and store them in a dataframe
                info<-model_to_dataframe(model_list=M1, frame_template=res, obs_dataset=dataset, Y_col=Y_col, Bootstrap=TRUE) 
                res.boot<-rbind(res.boot, info)
             }, error=function(e){})    
      }    
    
      out<-rbind(infoM0,res.boot)
      return(out)
    }
}

# Changes:
# 2021-12-03 Added AIC formula