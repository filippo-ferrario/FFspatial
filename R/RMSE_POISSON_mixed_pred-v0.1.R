# ===================================================================
# Title   : Root Mean squared error for both fixed and mixed models FITTED WITH POISSON FAMILY
# Author  : Filippo Ferrario
# Date    : 2020-07-31
# Version : 0.1
# Aim     : calculate the rmse with the option of calculating prediction with or without RANDOM effect
# ===================================================================



##########################################################
# Calculate RMSE allowing prediction on fixed or random factor
###########################################################

#' Root Mean squared error for Poisson glm(m)
#' 
#' Root Mean squared error for both fixed and mixed models FITTED WITH POISSON FAMILY
#' 
#' @param model a Poisson glm(m) model
#' @param Y_col name of the column containing the response variable.
#' @param ... used to pass arguments to predict.
#' 
#' @details 
#' `...` can be used to specify "level" . 0= To match re.form=NA in Bouwmeester et al 2013. 
#' See `?predict.lme` : Level values increase from outermost to innermost grouping, with level zero corresponding to the population predictions. 
#' 
#' 
#' @author Filippo Ferrario, \email{filippo.f3rrario@gmail.com} 
#' 
#' @seealso
#' [nlme::predict.lme]
#' 
#' @export

rmsePOI<-function(model, Y_col=NULL,...){
          pr.train<- predict(model, ... )  # specify "level" . 0= To match re.form=NA in Bouwmeester et al 2013. see ?predict.lme : Level values increase from outermost to innermost grouping, with level zero corresponding to the population predictions. 
          fit.train<-exp(pr.train) # to check that is correct : sum ((fit.train) != predict(mod, re.form=NA, type='response' ) ) 
          Y.train<-model$data[,Y_col]
          rmse.train<- sqrt( mean( (fit.train - Y.train)^2 ))
          return(rmse.train)
      } 
