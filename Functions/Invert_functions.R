# This function calculates the estimated differences, SEs and CIs. I wrote it for logistic and negbin models, tried to adapt it for lmer and now (Nov 2021) trying to adapt it for gam, glmer and glmmadmb.
# Inputs are the model to estimate from, a unique model matrix for the parameters of interest (with other params set to zero, and the differences matrix which specifies how to calculate the differences):
# Author: Annabel Smith & Wade Blanchard

diff.est<-function(model,unique.mod.mat,diff.matrix){
  # ESTIMATES:
  contrasts<-diff.matrix%*%unique.mod.mat
  if(class(model)[1]=="glmerMod") mod.coef<-summary(model)$coefficients[,1]
  if(class(model)[1]=="glmmadmb") mod.coef<-summary(model)$coefficients[,1]
  if(class(model)[1]=="gam") mod.coef<-summary(sd_mod2a)$p.table[,1]
  diffs<-contrasts%*%mod.coef
  # SE:
  mod.vcov<-vcov(model)
  diff.se<-sqrt(diag(contrasts%*%mod.vcov%*%t(contrasts)))
  # CI
  ndf<-data.frame(est=diffs, se=diff.se)
  ndf$lci<-ndf$est-(ndf$se*1.96)
  ndf$uci<-ndf$est+(ndf$se*1.96)
  return(ndf)
}


# Drop levels and re-assign rownames to subsetted data frames:
tidy.df<-function(df){
  df<-droplevels(df)
  rownames(df)<-1:length(df[,1])
  return(df)
}
