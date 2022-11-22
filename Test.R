str(Test)
Test$Treatment <- as.factor(Test$Treatment)
Test$Yr <- Test$Year-min(Test$Year)
Test$Yr <- as.integer(Test$Yr)

Mod1<-glmmadmb(Abundance~Treatment+Yr+Treatment, family="nbinom", data=Test)
summary(Mod1)

nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(Test$Treatment),2),levels=levels(Test$Treatment)))
pr <- predict(Mod1,newdata=nd,se.fit=T,type='link')
pr<-data.frame(nd, fit.link=pr$fit, se=pr$se.fit)
pr$lci.link<-pr$fit-(1.96*pr$se)
pr$uci.link<-pr$fit+(1.96*pr$se)
pr$fit.resp<-exp(pr$fit.link)
pr$lci.resp<-exp(pr$lci.link)
pr$uci.resp<-exp(pr$uci.link)
pr

#made prtest for plotting

plot(1:4,prtest$fit.resp,ylim=c(min(prtest$lci.resp)-0.5,max(prtest$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',yaxt='n',ylab='Response metric',xlab='Year',main='Expectation',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,prtest$lci.resp,1:4,prtest$uci.resp,length=0.1,angle=90,code=3)
par(xpd=NA)
legend(x=5,y=8,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

dev.new(width=10,height=8,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,1),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,0,6))
boxplot(Test$Abundance~Test$Treatment+Test$Year,ylab='Abundance',xlab='',main='Expectation',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))
