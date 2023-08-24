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
text(x=1:4, y=max(ara_plot$uci.resp)+2,labels=c(rep("a"),rep("ab"),rep("b"),rep(letters[c(1)],1)))
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

#Diversity and richness for focus groups

dev.new(width=15,height=10,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,2.5,5))
plot(1:4,Ararich_pr$fit.resp,ylim=c(min(Ararich_pr$lci.resp)-0.5,max(Ararich_pr$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species Richness',xlab='Year')
title (mtext("(a) Araneae richness", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Ararich_pr$lci.resp,1:4,Ararich_pr$uci.resp,length=0.1,angle=90,code=3)
ap1 <- round(summary(Ararich_mod1)$coefficients[3,'Pr(>|z|)'],3)
ap1 <- ifelse(ap1<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',ap1,sep=''))
legend('bottomleft',legend = c(paste('Treat. P=',round(summary(Ararich_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''),ap1,paste('Int. P=',round(summary(Ararich_mod1)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
plot(1:4,Colrich_pr$fit.resp,ylim=c(min(Colrich_pr$lci.resp),max(Colrich_pr$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species Richness',xlab='Year')
title (mtext("(b) Coleoptera richness", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Colrich_pr$lci.resp,1:4,Colrich_pr$uci.resp,length=0.1,angle=90,code=3)
cp1 <- round(summary(Colrich_mod1)$coefficients[3,'Pr(>|z|)'],3)
cp1 <- ifelse(cp1<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',cp1,sep=''))
legend('topleft',legend = c(paste('Treat. P=',round(summary(Colrich_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''),cp1,paste('Int. P=',round(summary(Colrich_mod1)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
plot(1:4,Formrich_pr$fit.resp,ylim=c(min(Formrich_pr$lci.resp),max(Formrich_pr$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species Richness',xlab='Year')
title (mtext("(c) Formicidae richness", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Formrich_pr$lci.resp,1:4,Formrich_pr$uci.resp,length=0.1,angle=90,code=3)
fp1 <- round(summary(Formrich_mod1)$coefficients[3,'Pr(>|z|)'],3)
fp1 <- ifelse(fp1<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',fp1,sep=''))
legend('topleft',legend = c(paste('Treat. P=',round(summary(Formrich_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''),fp1,paste('Int. P=',round(summary(Formrich_mod1)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
par(xpd=NA)
legend(x=5,y=0.5,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)
plot(1:4,Arainvdiv_pr$fit,ylim=c(min(Arainvdiv_pr$lci),max(Arainvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='Year')
title (mtext("(d) Araneae diversity", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Arainvdiv_pr$lci,1:4,Arainvdiv_pr$uci,length=0.1,angle=90,code=3)
ap <- round(summary(Arainvdiv_mod1)$coefficients[3,'Pr(>|z|)'],3)
ap <- ifelse(ap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',ap,sep=''))
legend('topright',legend = c(paste('Treat. P=',round(summary(Arainvdiv_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''),ap,paste('Int. P=',round(summary(Arainvdiv_mod1)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
plot(1:4,Colinvdiv_pr$fit,ylim=c(min(Colinvdiv_pr$lci),max(Colinvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='Year')
title (mtext("(e) Coleoptera diversity", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Colinvdiv_pr$lci,1:4,Colinvdiv_pr$uci,length=0.1,angle=90,code=3)
cp <- round(summary(Colinvdiv_mod1)$coefficients[3,'Pr(>|z|)'],3)
cp <- ifelse(cp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',cp,sep=''))
legend('topleft',legend = c(paste('Treat. P=',round(summary(Colinvdiv_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''),cp,paste('Int. P=',round(summary(Colinvdiv_mod1)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
plot(1:4,Forminvdiv_pr$fit,ylim=c(min(Forminvdiv_pr$lci),max(Forminvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='Year')
title (mtext("(f) Formicidae diversity", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Forminvdiv_pr$lci,1:4,Forminvdiv_pr$uci,length=0.1,angle=90,code=3)
fp <- round(summary(Forminvdiv_mod1)$coefficients[3,'Pr(>|z|)'],3)
fp <- ifelse(fp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',fp,sep=''))
legend('topleft',legend = c(paste('Treat. P=',round(summary(Forminvdiv_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''),fp,paste('Int. P=',round(summary(Forminvdiv_mod1)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')


dev.new(width=15,height=5,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,2.5,5))
plot(1:4,Blarich_pr$fit,ylim=c(0,1),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Probability of Occurence',xlab='Year')
title (mtext("(a) Blattodea", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Blarich_pr$lci,1:4,Blarich_pr$uci,length=0.1,angle=90,code=3)
bp1 <- round(summary(Blarich_mod1)$coefficients[3,'Pr(>|z|)'],3)
bp1 <- ifelse(bp1<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',bp1,sep=''))
legend('topright',legend = c(paste('Treat. P=',round(summary(Blarich_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''),bp1,paste('Int. P=',round(summary(Blarich_mod1)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
plot(1:4,Ortrich_pr$fit,ylim=c(0,1),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Probability of Occurence',xlab='Year')
title (mtext("(b) Orthoptera", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Ortrich_pr$lci,1:4,Ortrich_pr$uci,length=0.1,angle=90,code=3)
orp1 <- round(summary(Ortrich_mod1)$coefficients[3,'Pr(>|z|)'],3)
orp1 <- ifelse(orp1<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',orp1,sep=''))
legend('bottomright',legend = c(paste('Treat. P=',round(summary(Ortrich_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''),orp1,paste('Int. P=',round(summary(Ortrich_mod1)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
plot(1:4,Otherrich_pr$fit,ylim=c(0,1),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Probability of Occurence',xlab='Year')
title (mtext("(c) Other", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Otherrich_pr$lci,1:4,Otherrich_pr$uci,length=0.1,angle=90,code=3)
op1 <- round(summary(Otherrich_mod1)$coefficients[3,'Pr(>|z|)'],3)
op1 <- ifelse(op1<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',op1,sep=''))
legend('bottomright',legend = c(paste('Treat. P=',round(summary(Otherrich_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''),op1,paste('Int. P=',round(summary(Otherrich_mod1)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
par(xpd=NA)
legend(x=5,y=0.8,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

dev.new(width=15,height=10,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,2.5,5))
plot(1:4,arapca_pr$fit,ylim=c(min(arapca_pr$lci),max(arapca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principal component score',xlab='Year')
title (mtext("(a) Araneae", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,arapca_pr$lci,1:4,arapca_pr$uci,length=0.1,angle=90,code=3)
arap <- round(summary(arapca2_mod1)$coefficients[3,'Pr(>|t|)'],3)
arap <- ifelse(arap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',arap,sep=''))
legend('bottomright',legend = c(paste('Treat. P=',round(summary(arapca2_mod1)$coefficients[2,'Pr(>|t|)'],3),sep=''),arap,paste('Int. P=',round(summary(arapca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep='')),pch="", adj = 0,bty = 'n')
plot(1:4,blapca_pr$fit,ylim=c(min(blapca_pr$lci),max(blapca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principal component score',xlab='Year')
title (mtext("(b) Blattodea", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,blapca_pr$lci,1:4,blapca_pr$uci,length=0.1,angle=90,code=3)
blap <- round(summary(blapca2_mod1)$coefficients[3,'Pr(>|t|)'],6)
blap <- ifelse(blap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',blap,sep=''))
legend('bottomleft',legend = c(paste('Treat. P=',round(summary(blapca2_mod1)$coefficients[2,'Pr(>|t|)'],3),sep=''),blap,paste('Int. P=',round(summary(blapca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep='')),pch="", adj = 0,bty = 'n')
plot(1:4,colpca_pr$fit,ylim=c(min(colpca_pr$lci),max(colpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principal component score',xlab='Year')
title (mtext("(c) Coleoptera", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,colpca_pr$lci,1:4,colpca_pr$uci,length=0.1,angle=90,code=3)
colp <- round(summary(colpca2_mod1)$coefficients[3,'Pr(>|t|)'],6)
colp <- ifelse(colp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',colp,sep=''))
legend('bottomright',legend = c(paste('Treat. P=',round(summary(colpca2_mod1)$coefficients[2,'Pr(>|t|)'],3),sep=''),colp,paste('Int. P=',round(summary(colpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep='')),pch="", adj = 0,bty = 'n')
par(xpd=NA)
legend(x=5,y=5,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)
plot(1:4,formpca_pr$fit,ylim=c(min(formpca_pr$lci),max(formpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principal component score',xlab='Year')
title (mtext("(d) Formicidae", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,formpca_pr$lci,1:4,formpca_pr$uci,length=0.1,angle=90,code=3)
formp <- round(summary(formpca2_mod1)$coefficients[3,'Pr(>|t|)'],3)
formp <- ifelse(formp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',formp,sep=''))
legend('topright',legend = c(paste('Treat. P=',round(summary(formpca2_mod1)$coefficients[2,'Pr(>|t|)'],3),sep=''),formp,paste('Int. P=',round(summary(formpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep='')),pch="", adj = 0,bty = 'n')
plot(1:4,ortpca_pr$fit,ylim=c(min(ortpca_pr$lci),max(ortpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principal component score',xlab='Year')
title (mtext("(e) Orthoptera", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,ortpca_pr$lci,1:4,ortpca_pr$uci,length=0.1,angle=90,code=3)
ortp <- round(summary(ortpca2_mod1)$coefficients[3,'Pr(>|t|)'],3)
ortp <- ifelse(ortp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',ortp,sep=''))
legend('bottomright',legend = c(paste('Treat. P=',round(summary(ortpca2_mod1)$coefficients[2,'Pr(>|t|)'],3),sep=''),ortp,paste('Int. P=',round(summary(ortpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep='')),pch="", adj = 0,bty = 'n')
plot(1:4,otherpca_pr$fit,ylim=c(min(otherpca_pr$lci),max(otherpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principal component score',xlab='Year')
title (mtext("(f) Other", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,otherpca_pr$lci,1:4,otherpca_pr$uci,length=0.1,angle=90,code=3)
othp <- round(summary(otherpca2_mod1)$coefficients[3,'Pr(>|t|)'],3)
othp <- ifelse(othp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',othp,sep=''))
legend('bottomright',legend = c(paste('Treat. P=',round(summary(otherpca2_mod1)$coefficients[2,'Pr(>|t|)'],3),sep=''),othp,paste('Int. P=',round(summary(otherpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep='')),pch="", adj = 0,bty = 'n')

#(a) Araneae
abetapred <- b_pred[[1]]
abmod <- b_mod[[1]]
#(b) Coleoptera
cbetapred <- b_pred[[2]]
cbmod <- b_mod[[2]]
#(c) Formicidae
fbetapred <- b_pred[[3]]
fbmod <- b_mod[[3]]

dev.new(width=15,height=10,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,2.5,5))
plot(1:4,abetapred$fit.resp,ylim=c(0,max(abetapred$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Beta diversity',xlab='')
title (mtext("(a) Araneae", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'),mgp=c(2,0.8,0))
arrows(1:4,abetapred$lci.resp,1:4,abetapred$uci.resp,length=0.05,angle=90,code=3)
betap <- round(summary(abmod)$coefficients[3,'Pr(>|z|)'],3)
betap <- ifelse(betap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',betap,sep=''))
legend('bottomleft',legend = c(paste('Treat. P=',round(summary(abmod)$coefficients[2,'Pr(>|z|)'],3),sep=''),betap,paste('Int. P=',round(summary(abmod)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
title(xlab='Year',mgp=c(2,0.8,0))
plot(1:4,cbetapred$fit.resp,ylim=c(0,max(cbetapred$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Beta diversity',xlab='')
title (mtext("(b) Coleoptera", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'),mgp=c(2,0.8,0))
arrows(1:4,cbetapred$lci.resp,1:4,cbetapred$uci.resp,length=0.05,angle=90,code=3)
betap <- round(summary(cbmod)$coefficients[3,'Pr(>|z|)'],3)
betap <- ifelse(betap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',betap,sep=''))
legend('bottomleft',legend = c(paste('Treat. P=',round(summary(cbmod)$coefficients[2,'Pr(>|z|)'],3),sep=''),betap,paste('Int. P=',round(summary(cbmod)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
title(xlab='Year',mgp=c(2,0.8,0))
plot(1:4,fbetapred$fit.resp,ylim=c(0,max(fbetapred$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Beta diversity',xlab='')
title (mtext("(c) Formicidae", side=3, adj=0, line=0.8, cex=0.75, font=1))
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'),mgp=c(2,0.8,0))
arrows(1:4,fbetapred$lci.resp,1:4,fbetapred$uci.resp,length=0.05,angle=90,code=3)
betap <- round(summary(fbmod)$coefficients[3,'Pr(>|z|)'],3)
betap <- ifelse(betap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',betap,sep=''))
legend('bottomleft',legend = c(paste('Treat. P=',round(summary(fbmod)$coefficients[2,'Pr(>|z|)'],3),sep=''),betap,paste('Int. P=',round(summary(fbmod)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
title(xlab='Year',mgp=c(2,0.8,0))
par(xpd=NA)
legend(x=5,y=15,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)
i=1
bgroup.this.run<-beta_groups[i]
agroup.this.run<-alpha_groups[i]
bdat.this.run<-beta_dat[,bgroup.this.run]
adat.this.run<-beta_dat[,agroup.this.run]
plot(adat.this.run,bdat.this.run,type='p',pch=c(16),las=1,cex=1.5,ylab='Beta diversity',xlab='Alpha diversity (richness)')
title (mtext("(d) Araneae", side=3, adj=0, line=0.8, cex=0.75, font=1))
i=2
bgroup.this.run<-beta_groups[i]
agroup.this.run<-alpha_groups[i]
bdat.this.run<-beta_dat[,bgroup.this.run]
adat.this.run<-beta_dat[,agroup.this.run]
plot(adat.this.run,bdat.this.run,type='p',pch=c(16),las=1,cex=1.5,ylab='Beta diversity',xlab='Alpha diversity (richness)')
title (mtext("(e) Coleoptera", side=3, adj=0, line=0.8, cex=0.75, font=1))
i=3
bgroup.this.run<-beta_groups[i]
agroup.this.run<-alpha_groups[i]
bdat.this.run<-beta_dat[,bgroup.this.run]
adat.this.run<-beta_dat[,agroup.this.run]
plot(adat.this.run,bdat.this.run,type='p',pch=c(16),las=1,cex=1.5,ylab='Beta diversity',xlab='Alpha diversity (richness)')
title (mtext("(f) Formicidae", side=3, adj=0, line=0.8, cex=0.75, font=1))
