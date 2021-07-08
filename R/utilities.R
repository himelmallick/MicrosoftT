get_AICtab<-function(fit){
  
  ########################
  # Flag invalid options #
  ########################
  
  if (!class(fit) %in% c('cpglm', 'zcpglm', 'glmmTMB')){
    stop('Not supported. Valid options are cplm , zcpglm, and glmmTMB')
  }
  
  ######################
  #  Initialize AICtab #
  ######################
  
  AICtab<-rep(NA, 5)
  
  ###########################
  # Case-by-Case Extraction #
  ###########################
  
  if (class(fit)=='cpglm'){
    
    ##########################################
    # Back calculate logLik and BIC from AIC #
    ##########################################
    
    AIC<-fit$aic
    AIC_multiplier<-length(fit$y) - fit$df.residual
    logLik<-(AIC - 2*AIC_multiplier)/2
    BIC_multiplier<-AIC_multiplier*log(length(fit$y))
    BIC<-BIC_multiplier + 2*logLik
    deviance<-fit$deviance
    df.resid<-fit$df.residual
    
    # Coherent output
    AICtab<-c(AIC, BIC, logLik, deviance, df.resid)
  }
  
  if (class(fit)=='zcpglm'){
    
    ##########################################
    # Back calculate AIC and BIC from logLik #
    ##########################################
    
    logLik<--fit$llik
    AIC_multiplier<-length(fit$y) - fit$df.residual
    BIC_multiplier<-AIC_multiplier*log(length(fit$y))
    AIC<-2*AIC_multiplier + 2*logLik
    BIC<-BIC_multiplier + 2*logLik
    deviance<-NA
    df.resid<-fit$df.residual
    
    # Coherent output
    AICtab<-c(AIC, BIC, logLik, deviance, df.resid)
    
  }
  
  if (class(fit)=='glmmTMB'){
    
    #######################################
    # Extract AICtab from glmmTMB objects #
    #######################################
    
    AICtab<-summary(fit)["AICtab"]$AICtab
    
  }
  
  ##########
  # Return #
  ##########
  
  names(AICtab)<-c('AIC', 'BIC', 'logLik', 'deviance', 'df.resid')
  return(AICtab)
}