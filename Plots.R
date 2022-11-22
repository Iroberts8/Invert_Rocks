#Plots for the paper

#Plots for presentation

#Richness ----

dev.new(width=14,height=4,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,0,5))
plot(1:4,Ararich_pr$fit.resp,ylim=c(0,max(Ararich_pr$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species Richness',xlab='Year',main='Araneae',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Ararich_pr$lci.resp,1:4,Ararich_pr$uci.resp,length=0.1,angle=90,code=3)
plot(1:4,Colrich_pr$fit.resp,ylim=c(0,max(Colrich_pr$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species Richness',xlab='Year',main='Coleoptera',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Colrich_pr$lci.resp,1:4,Colrich_pr$uci.resp,length=0.1,angle=90,code=3)
plot(1:4,Formrich_pr$fit.resp,ylim=c(0,max(Formrich_pr$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species Richness',xlab='Year',main='Formicidae',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Formrich_pr$lci.resp,1:4,Formrich_pr$uci.resp,length=0.1,angle=90,code=3)
par(xpd=NA)
legend(x=5,y=11,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)


dev.new(width=10,height=8,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,1),mar=c(5,5,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))
plot(1:4,Ararich_pr$fit.resp,ylim=c(0,max(Ararich_pr$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species Richness',xlab='Year',main='Araneae',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Ararich_pr$lci.resp,1:4,Ararich_pr$uci.resp,length=0.1,angle=90,code=3)
text(x=1:4, y=max(Ararich_pr$uci.resp)+0.4,labels=c(rep("ab"),rep("a"),rep("ab"),rep("b")))
par(xpd=NA)
legend(x=5,y=4,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Diversity ----

dev.new(width=14,height=4,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,0,5))
plot(1:4,Arainvdiv_pr$fit,ylim=c(min(Arainvdiv_pr$lci),max(Arainvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='Year',main='Araneae',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Arainvdiv_pr$lci,1:4,Arainvdiv_pr$uci,length=0.1,angle=90,code=3)
ap <- round(summary(Arainvdiv_mod1)$coefficients[3,'Pr(>|z|)'],3)
ap <- ifelse(ap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',ap,sep=''))
plot(1:4,Colinvdiv_pr$fit,ylim=c(min(Colinvdiv_pr$lci),max(Colinvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='Year',main='Coleoptera',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Colinvdiv_pr$lci,1:4,Colinvdiv_pr$uci,length=0.1,angle=90,code=3)
cp <- round(summary(Colinvdiv_mod1)$coefficients[3,'Pr(>|z|)'],3)
cp <- ifelse(cp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',cp,sep=''))
plot(1:4,Forminvdiv_pr$fit,ylim=c(min(Forminvdiv_pr$lci),5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='Year',main='Formicidae',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Forminvdiv_pr$lci,1:4,Forminvdiv_pr$uci,length=0.1,angle=90,code=3)
fp <- round(summary(Forminvdiv_mod1)$coefficients[3,'Pr(>|z|)'],3)
fp <- ifelse(fp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',fp,sep=''))
par(xpd=NA)
legend(x=5,y=5,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

dev.new(width=10,height=8,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,1),mar=c(5,5,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))
plot(1:4,Arainvdiv_pr$fit,ylim=c(min(Arainvdiv_pr$lci),max(Arainvdiv_pr$uci)+0.4),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='Year',main='Araneae',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Arainvdiv_pr$lci,1:4,Arainvdiv_pr$uci,length=0.1,angle=90,code=3)
ap <- round(summary(Arainvdiv_mod1)$coefficients[3,'Pr(>|z|)'],3)
ap <- ifelse(ap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',ap,sep=''))
text(x=1:4, y=max(Arainvdiv_pr$uci)+0.4,labels=c(rep("a"),rep("a"),rep("a"),rep("b")))
par(xpd=NA)
legend(x=5,y=4.5,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Community composition ----

dev.new(width=14,height=4,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,0,5))
plot(1:4,arapca_pr$fit,ylim=c(min(arapca_pr$lci),max(arapca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principal component score',xlab='Year',main='Araneae',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,arapca_pr$lci,1:4,arapca_pr$uci,length=0.1,angle=90,code=3)
arap <- round(summary(arapca2_mod1)$coefficients[3,'Pr(>|t|)'],3)
plot(1:4,colpca_pr$fit,ylim=c(min(colpca_pr$lci),max(colpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principal component score',xlab='Year',main='Coleoptera',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,colpca_pr$lci,1:4,colpca_pr$uci,length=0.1,angle=90,code=3)
colp <- round(summary(colpca2_mod1)$coefficients[3,'Pr(>|t|)'],6)
plot(1:4,formpca_pr$fit,ylim=c(min(formpca_pr$lci),max(formpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principal component score',xlab='Year',main='Formicidae',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,formpca_pr$lci,1:4,formpca_pr$uci,length=0.1,angle=90,code=3)
formp <- round(summary(formpca2_mod1)$coefficients[3,'Pr(>|t|)'],3)
par(xpd=NA)
legend(x=5,y=5,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Beta diversity ----

dev.new(width=14,height=4,dpi=80,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,0,5))

for(i in 1:length(beta_groups)){
  
  pred.beta.run<-b_pred[[i]]
  plot(1:4,pred.beta.run$fit.resp,ylim=c(10,max(pred.beta.run$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Beta diversity',xlab='',main=group_names[i],font.main=1)
  
  axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'),mgp=c(2,0.8,0))
  arrows(1:4,pred.beta.run$lci.resp,1:4,pred.beta.run$uci.resp,length=0.05,angle=90,code=3)
  if(round(beta_dat$b_int[i],3)>0.001) text(x=1, y=(par('usr')[2])-par('usr')[2]/10,labels=paste('Int.p=',round(beta_dat$b_int[i],3),sep=''),adj=0)else text(x=1, y=(par('usr')[2])-par('usr')[2]/10,labels="Int.p <0.001",adj=0)
  title(xlab='Year',mgp=c(2,0.8,0))
  
} #close plot loop

par(xpd=NA)
legend(x=5,y=16,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

dev.new(width=10,height=8,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,1),mar=c(5,5,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))
ara_plot<-b_pred[[1]]
plot(1:4,ara_plot$fit.resp,ylim=c(0,max(ara_plot$uci.resp)+5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Beta diversity',xlab='Year',main="Araneae",font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,ara_plot$lci.resp,1:4,ara_plot$uci.resp,length=0.05,angle=90,code=3)
text(x=1:4, y=max(ara_plot$uci.resp)+2,labels=c(rep("a"),rep("b"),rep("ab"),rep(letters[c(1)],1)))
par(xpd=NA)
legend(x=4.75, y=20, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Functional groups ----
dev.new(width=10,height=8,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,1),mar=c(5,5,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))
big_plot<-formgroup_abpred[[1]]
plot(1:4,big_plot$fit.resp,ylim=c(0,max(big_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main="Big ants",font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,big_plot$lci.resp,1:4,big_plot$uci.resp,length=0.05,angle=90,code=3)
text(x=0.5, y=0.15,labels=paste('Int.p=',round(formgroups_summary$p_abun_int[1],3),sep=''),adj=0)
text(x=1:4, y=max(big_plot$uci.resp)+0.4,labels=c(rep(letters[c(1)],1),rep("a"),rep(letters[c(2)],1),rep(letters[c(1)],1)))
par(xpd=NA)
legend(x=5, y=7, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

dev.new(width=18,height=16,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(2,2),mar=c(5,5,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))

bigomn_plot<-formgroup_abpred[[14]]
plot(1:4,bigomn_plot$fit.resp,ylim=c(0,max(bigomn_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main='Big_omnivores',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,bigomn_plot$lci.resp,1:4,bigomn_plot$uci.resp,length=0.05,angle=90,code=3)
text(x=0.5, y=0.15,labels=paste('Int.p=',round(formgroups_summary$p_abun_int[14],3),sep=''),adj=0)
text(x=1:4, y=max(bigomn_plot$uci.resp)+0.4,labels=c(rep(letters[c(1)],1),rep("a"),rep(letters[c(2)],1),rep(letters[c(1)],1)))
bigmany_plot<-formgroup_abpred[[24]]
plot(1:4,bigmany_plot$fit.resp,ylim=c(0,max(bigmany_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main='Big_large colony',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,bigmany_plot$lci.resp,1:4,bigmany_plot$uci.resp,length=0.05,angle=90,code=3)
text(x=0.5, y=0.15,labels=paste('Int.p=',round(formgroups_summary$p_abun_int[24],3),sep=''),adj=0)
text(x=1:4, y=max(bigmany_plot$uci.resp)+0.4,labels=c(rep(letters[c(1)],1),rep("a"),rep(letters[c(2)],1),rep(letters[c(1)],1)))

par(xpd=NA)
legend(x=5, y=6, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

bigmix_plot<-formgroup_abpred[[18]]
plot(1:4,bigmix_plot$fit.resp,ylim=c(0,max(bigmix_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main='Big_mixed nest site',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,bigmix_plot$lci.resp,1:4,bigmix_plot$uci.resp,length=0.05,angle=90,code=3)
text(x=0.5, y=0.15,labels=paste('Int.p=',round(formgroups_summary$p_abun_int[18],3),sep=''),adj=0)
text(x=1:4, y=max(bigmix_plot$uci.resp)+0.4,labels=c(rep(letters[c(1)],1),rep("ab"),rep("b"),rep(letters[c(1)],1)))
bigcoop_plot<-formgroup_abpred[[27]]
plot(1:4,bigcoop_plot$fit.resp,ylim=c(0,max(bigcoop_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main='Big_cooperative',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,bigcoop_plot$lci.resp,1:4,bigcoop_plot$uci.resp,length=0.05,angle=90,code=3)
text(x=0.5, y=0.15,labels=paste('Int.p=',round(formgroups_summary$p_abun_int[27],3),sep=''),adj=0)
text(x=1:4, y=max(bigcoop_plot$uci.resp)+0.4,labels=c(rep(letters[c(1)],1),rep("ab"),rep("b"),rep(letters[c(1)],1)))