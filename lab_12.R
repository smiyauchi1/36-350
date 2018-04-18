generate_data = function(n,p){
  covariates = matrix(rnorm(n*p,0,1),nrow = n, ncol = p)
  responses = rnorm(n, 0,1)
  return (list(covariates=covariates,responses = responses))
  }

model_select = function(covariates, responses, cutoff){ 
  data = list(covariates = covariates, responses = responses)
  cov.lm = lm(responses ~ covariates, data)
  cov.noint = summary(cov.lm)$coefficients[-1,]
  index.cutoff = which(cov.noint[,4]  <= cutoff)
  names(index.cutoff)=NULL
  return(lm(responses~covariates[,index.cutoff], data))
}


