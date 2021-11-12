#Species data from 2016 and 2019
#Five reserves

#Five target orders
#1 Araneae (Spiders) - Family
#2 Blattodea (Cockroaches) - Family
#3 Coleoptera (Beetles) - Family
#4 Orthoptera (Crickets and Grasshoppers) - Sub-family
#5 Formicidae (Ants) - Sub-family

#6 Other (Other invertebrates) - identified to Order only, does not include Hemiptera, Hymenoptera

#Load workspace
load('Workspace/Invert_Rocks_E.RData')

#save.image('Workspace/Invert_Rocks_E.RData')

#Species information
head(Morphospecies);dim(Morphospecies)
head(Morpho_abun);dim(Morpho_abun)
hist(Morpho_abun$Abundance[Morpho_abun$Abundance<100])

#Site data (Site_data)
Site <- read.csv('Data/Site_data.csv',header=T)
head(Site);dim(Site)

head(Other[,1:10],3);dim(Other)
colnames(Other)[6:ncol(Other)] %in% Morphospecies$Morphospecies

table(Morphospecies$Order)
Morphospecies[grep(pattern = 'Larvae',x = Morphospecies$Morphospecies),]

#Create taxanomic richness data
basedata <- Col2[,1:5]
basedata$Pit_code <- paste(basedata$Site,basedata$Plot,basedata$Treatment,basedata$Pitfall,basedata$Year,sep='_')
head(basedata);dim(basedata)

taxrich <- basedata
head(taxrich);dim(taxrich)

Col_rich2 <- data.frame(Pit_code=basedata$Pit_code, Col_rich2=apply(X = Col2[,6:ncol(Col2)], MARGIN = 1, FUN = function(x) length(which(x>0))))
head(Col_rich2);dim(Col_rich2)

taxrich2 <- merge(taxrich,Col_rich2,by='Pit_code',all.x=T,all.y=F)
head(taxrich2);dim(taxrich2)

#Summary data - abundance overall and by year/site
#Araneae summary data
Ara2 <- Araneae[,c(-3,-5)]
Ara_summ <- Ara2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)
head(Ara2)
write.csv(Ara_summ,file='Araneae summary')
#Lycosidae 1 vs Treatment
boxplot(Ara2[,28]~Ara2$Treatment)
#Lycosidae 6 vs Year
boxplot(Ara2[,28]~Ara2$Year)

#Araneae total abundance
Ara_ttl <- Ara2[,c(-1,-2,-3)] %>% summarise_all(sum)
Ara_ttlabun <- as.data.frame(t(Ara_ttl))
names(Ara_ttlabun)[names(Ara_ttlabun) == 'V1'] <- "Abundance"
write.csv(Ara_ttlabun,file='Araneae abundance')

#Araneae by year 
Ara_yr <- Ara2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Ara_yrabun <- as.data.frame(t(Ara_yr))
colnames(Ara_yrabun) <- Ara_yrabun[1,]
Ara_yrabun <- Ara_yrabun[(-1),]
write.csv(Ara_yrabun,file='Araneae year')

#Araneae by site
Ara_site <- Ara2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Ara_siteabun <- as.data.frame(t(Ara_site))
colnames(Ara_siteabun) <- Ara_siteabun[1,]
Ara_siteabun <- Ara_siteabun[(-1),]
write.csv(Ara_siteabun,file='Araneae site')

#Blattodea summary data
Bla2 <- Blattodea[,c(-3,-5)]
Bla_summ <- Bla2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)
write.csv(Bla_summ,file='Blattodea summary')

#Blattodea total abundance
Bla_ttl <- Bla2[,c(-1,-2,-3)] %>% summarise_all(sum)
Bla_ttlabun <- as.data.frame(t(Bla_ttl))
names(Bla_ttlabun)[names(Bla_ttlabun) == 'V1'] <- "Abundance"
write.csv(Bla_ttlabun,file='Blattodea abundance')

#Blattodea by year 
Bla_yr <- Bla2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Bla_yrabun <- as.data.frame(t(Bla_yr))
colnames(Bla_yrabun) <- Bla_yrabun[1,]
Bla_yrabun <- Bla_yrabun[(-1),]
write.csv(Bla_yrabun,file='Blattodea year')

#Blattodea by site
Bla_site <- Bla2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Bla_siteabun <- as.data.frame(t(Bla_site))
colnames(Bla_siteabun) <- Bla_siteabun[1,]
Bla_siteabun <- Bla_siteabun[(-1),]
write.csv(Bla_siteabun,file='Blattodea site')

#Coleoptera summary data
Col2 <- Coleoptera[,c(-3,-5,-42,-43,-44,-45,-46,-47,-48,-49)]
Col_summ <- Col2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)
write.csv(Col_summ,file='Coleoptera summary')

#Coleoptera total abundance
Col_ttl <- Col2[,c(-1,-2,-3)] %>% summarise_all(sum)
Col_ttlabun <- as.data.frame(t(Col_ttl))
names(Col_ttlabun)[names(Col_ttlabun) == 'V1'] <- "Abundance"
write.csv(Col_ttlabun,file='Coleoptera abundance')

#Coleoptera by year 
Col_yr <- Col2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Col_yrabun <- as.data.frame(t(Col_yr))
colnames(Col_yrabun) <- Col_yrabun[1,]
Col_yrabun <- Col_yrabun[(-1),]
write.csv(Col_yrabun,file='Coleoptera year')

#Coleoptera by site
Col_site <- Col2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Col_siteabun <- as.data.frame(t(Col_site))
colnames(Col_siteabun) <- Col_siteabun[1,]
Col_siteabun <- Col_siteabun[(-1),]
write.csv(Col_siteabun,file='Coleoptera site')

#Formicidae summary data
Form2 <- Formicidae[,c(-3,-5)]
Form_summ <- Form2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)
write.csv(Form_summ,file='Formicidae summary')
#Dolichoderinae 1 vs treatment
boxplot(Form2[,5]~Form2$Treatment)
#Dolichoderinae 1 vs Year
boxplot(Form2[,5]~Form2$Year)

#Formicidae total abundance
Form_ttl <- Form2[,c(-1,-2,-3)] %>% summarise_all(sum)
Form_ttlabun <- as.data.frame(t(Form_ttl))
names(Form_ttlabun)[names(Form_ttlabun) == 'V1'] <- "Abundance"
write.csv(Form_ttlabun,file='Formicidae abundance')

#Formicidae by year 
Form_yr <- Form2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Form_yrabun <- as.data.frame(t(Form_yr))
colnames(Form_yrabun) <- Form_yrabun[1,]
Form_yrabun <- Form_yrabun[(-1),]
write.csv(Form_yrabun,file='Formicidae year')

#Formicidae by site
Form_site <- Form2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Form_siteabun <- as.data.frame(t(Form_site))
colnames(Form_siteabun) <- Form_siteabun[1,]
Form_siteabun <- Form_siteabun[(-1),]
write.csv(Form_siteabun,file='Formicidae site')

#Orthoptera summary data
Ort2 <- Orthoptera[,c(-3,-5,-7,-11)]
Ort_summ <- Ort2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)
write.csv(Ort_summ,file='Orthoptera summary')

#Orthoptera total abundance
Ort_ttl <- Ort2[,c(-1,-2,-3)] %>% summarise_all(sum)
Ort_ttlabun <- as.data.frame(t(Ort_ttl))
names(Ort_ttlabun)[names(Ort_ttlabun) == 'V1'] <- "Abundance"
write.csv(Ort_ttlabun,file='Orthoptera abundance')

#Orthoptera by year 
Ort_yr <- Ort2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Ort_yrabun <- as.data.frame(t(Ort_yr))
colnames(Ort_yrabun) <- Ort_yrabun[1,]
Ort_yrabun <- Ort_yrabun[(-1),]
write.csv(Ort_yrabun,file='Orthoptera year')

#Orthoptera by site
Ort_site <- Ort2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Ort_siteabun <- as.data.frame(t(Ort_site))
colnames(Ort_siteabun) <- Ort_siteabun[1,]
Ort_siteabun <- Ort_siteabun[(-1),]
write.csv(Ort_siteabun,file='Orthoptera site')

#Other species summary data
Other2 <- Other[,c(-3,-5,-11,-21,-28,-29,-30,-31,-32,-33,-34)]
Other_summ <- Other2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)
write.csv(Other_summ,file='Other summary')

#Other total abundance
Other_ttl <- Other2[,c(-1,-2,-3)] %>% summarise_all(sum)
Other_ttlabun <- as.data.frame(t(Other_ttl))
names(Other_ttlabun)[names(Other_ttlabun) == 'V1'] <- "Abundance"
write.csv(Other_ttlabun,file='Other abundance')

#Other by year 
Other_yr <- Other2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Other_yrabun <- as.data.frame(t(Other_yr))
colnames(Other_yrabun) <- Other_yrabun[1,]
Other_yrabun <- Other_yrabun[(-1),]
write.csv(Other_yrabun,file='Other year')

#Other by site
Other_site <- Other2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Other_siteabun <- as.data.frame(t(Other_site))
colnames(Other_siteabun) <- Other_siteabun[1,]
Other_siteabun <- Other_siteabun[(-1),]
write.csv(Other_siteabun,file='Other site')

Morpho2 <- Morphospecies[,c(-2,-3)]
Morpho2 <- group_by(Morphospecies)
unique(Morpho2$Morphospecies)
unique(Morpho2$Order)
Morpho_summ <- sapply(Morphospecies, function(Order) length(unique(Order)))

full_summ <- cbind(Ara_summ, Bla_summ, Col_summ, Form_summ, Ort_summ)
write.csv(full_summ,file='Target summary')

#Merging abundance data
#Total abundance
ttl_abun <- rbind(Ara_ttlabun,Bla_ttlabun,Col_ttlabun,Form_ttlabun,Ort_ttlabun,Other_ttlabun)

#Abundance by year
year_abun <- rbind(Ara_yrabun,Bla_yrabun,Col_yrabun,Form_yrabun,Ort_yrabun,Other_yrabun)

#Abundance by site
site_abun <- rbind(Ara_siteabun,Bla_siteabun,Col_siteabun,Form_siteabun,Ort_siteabun,Other_siteabun)

#Merge with Morphospecies file
rownames(ttl_abun)[1:nrow(ttl_abun)] %in% Morphospecies$Morphospecies
rownames(year_abun)[1:nrow(year_abun)] %in% Morphospecies$Morphospecies
rownames(site_abun)[1:nrow(site_abun)] %in% Morphospecies$Morphospecies
Morpho_abun <- cbind(Morphospecies[-c(122,123,124,125,126,127,128,129,206,210,226,236,243,244,245,246,247,248,249),],ttl_abun,year_abun,site_abun)
Morpho_abun <- Morpho_abun[,c(-1)]

#Merging all richness files
rich_nolarvae <- cbind(Ara_rich,Bla_rich,Col_rich2,Form_rich,Ort_rich2,Other_rich2)
rich_nolarvae2 <- rich_nolarvae[,c(-3,-5,-7,-9,-11)]
taxrich_full2<- merge(taxrich,rich_nolarvae2,by='Pit_code', all.x=T, all.y=F)
head(taxrich_full2);dim(taxrich_full2)

#Boxplots of richness
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
boxplot(taxrich_full2$Ara_rich~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Araneae richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(taxrich_full2$Bla_rich~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Blattodea richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(taxrich_full2$Col_rich2~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Coleoptera richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(taxrich_full2$Form_rich~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Formicidae richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(taxrich_full2$Ort_rich2~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Orthoptera richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(taxrich_full2$Other_rich2~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Other richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

#Histrograms of richness
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
hist(taxrich_full2$Ara_rich,main = 'Araneae Richness',ylab='Frequency',xlab='Araneae richness',las=1)
hist(taxrich_full2$Bla_rich,main = 'Blattodea richness',ylab='Frequency',xlab='Blattodea richness',las=1)
hist(taxrich_full2$Col_rich2,main = 'Coleoptera richness',ylab='Frequency',xlab='Coleoptera richness',las=1)
hist(taxrich_full2$Form_rich,main = 'Formicidae Richness',ylab='Frequency',xlab='Formicidae richness',las=1)
hist(taxrich_full2$Ort_rich2,main = 'Orthoptera richness',ylab='Frequency',xlab='Orthoptera richness',las=1)
hist(taxrich_full2$Other_rich2,main = 'Other richness',ylab='Frequency',xlab='Other richness',las=1)

#Add species diversity to main data file
head(taxrich_full2);dim(taxrich_full2)
head(Araneae[,1:10]);dim(Araneae)
apply(X = Araneae[,6:ncol(Araneae)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1))

Ara_div <- data.frame(Pit_code=basedata$Pit_code, Ara_div=apply(X = Araneae[,6:ncol(Araneae)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Ara_div);dim(Ara_div)

Bla_div <- data.frame(Pit_code=basedata$Pit_code, Bla_div=apply(X = Blattodea[,6:ncol(Blattodea)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Bla_div);dim(Bla_div)

Col_div <- data.frame(Pit_code=basedata$Pit_code, Col_div=apply(X = Col2[,6:ncol(Col2)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Col_div);dim(Col_div)

Form_div <- data.frame(Pit_code=basedata$Pit_code, Form_div=apply(X = Formicidae[,6:ncol(Formicidae)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Form_div);dim(Form_div)

Ort_div <- data.frame(Pit_code=basedata$Pit_code, Ort_div=apply(X = Ort2[,6:ncol(Ort2)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Ort_div);dim(Ort_div)

Other_div <- data.frame(Pit_code=basedata$Pit_code, Other_div=apply(X = Other2[,6:ncol(Other2)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Other_div);dim(Other_div)

#Merging of the diversity data (Shannons)
div <- cbind(Ara_div,Bla_div,Col_div,Form_div,Ort_div,Other_div)
div_full <- div[,c(-3,-5,-7,-9,-11)]
div_full2 <- merge(taxrich,div_full,by='Pit_code', all.x=T, all.y=F)
head(div_full2);dim(div_full2)

#Merging of diversity and richness
div_rich <- cbind(div_full,rich_nolarvae2)
div_rich2 <- div_rich[,c(-8)]
div_rich_full2<- merge(taxrich,div_rich2,by='Pit_code', all.x=T, all.y=F)
head(div_rich_full2);dim(div_rich_full2)
richtab <- div_rich_full2[,c(2:6,grep(pattern='rich',colnames(div_rich_full2)))]
head(richtab)

#How many zeros are in each morphospecies
apply(richtab[,6:length(richtab)],2,FUN = function(x)table(x==0))

#Histograms of Shannons diversity data - frequency of zeros in the data
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
hist(div_full2$Ara_div,main = 'Araneae Shannons Index',ylab='Frequency',xlab='Araneae diversity')
hist(div_full2$Bla_div,main = 'Blattodea Shannons Index',ylab='Frequency',xlab='Blattodea diversity')
hist(div_full2$Col_div,main = 'Coleoptera Shannons Index',ylab='Frequency',xlab='Coleoptera diversity')
hist(div_full2$Form_div,main = 'Formicidae Shannons Index',ylab='Frequency',xlab='Formicidae diversity')
hist(div_full2$Ort_div,main = 'Orthoptera Shannons Index',ylab='Frequency',xlab='Orthoptera diversity')
hist(div_full2$Other_div,main = 'Other Shannons Index',ylab='Frequency',xlab='Other diversity')

#Boxplots of Shannons diversity data
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
boxplot(div_full2$Ara_div~div_full2$Treatment+div_full2$Year,ylab='Araneae diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(div_full2$Bla_div~div_full2$Treatment+div_full2$Year,ylab='Blattodea diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(div_full2$Col_div~div_full2$Treatment+div_full2$Year,ylab='Coleoptera diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(div_full2$Form_div~div_full2$Treatment+div_full2$Year,ylab='Formicidae diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(div_full2$Ort_div~div_full2$Treatment+div_full2$Year,ylab='Orthoptera diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(div_full2$Other_div~div_full2$Treatment+div_full2$Year,ylab='Other diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

#Calculate inverse Simpson's index
head(taxrich_full2);dim(taxrich_full2)
head(Araneae[,1:10]);dim(Araneae)
apply(X = Araneae[,6:ncol(Araneae)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1))

Ara_invdiv <- data.frame(Pit_code=basedata$Pit_code, Ara_invdiv=apply(X = Araneae[,6:ncol(Araneae)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Ara_invdiv);dim(Ara_invdiv)
Ara_invdiv$Ara_invdiv[which(Ara_invdiv$Ara_invdiv==Inf)] <- 0

Bla_invdiv <- data.frame(Pit_code=basedata$Pit_code, Bla_invdiv=apply(X = Blattodea[,6:ncol(Blattodea)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Bla_invdiv);dim(Bla_invdiv)
Bla_invdiv$Bla_invdiv[which(Bla_invdiv$Bla_invdiv==Inf)] <- 0

Col_invdiv <- data.frame(Pit_code=basedata$Pit_code, Col_invdiv=apply(X = Col2[,6:ncol(Col2)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Col_invdiv);dim(Col_invdiv)
Col_invdiv$Col_invdiv[which(Col_invdiv$Col_invdiv==Inf)] <- 0

Form_invdiv <- data.frame(Pit_code=basedata$Pit_code, Form_invdiv=apply(X = Formicidae[,6:ncol(Formicidae)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Form_invdiv);dim(Form_invdiv)
Form_invdiv$Form_invdiv[which(Form_invdiv$Form_invdiv==Inf)] <- 0

Ort_invdiv <- data.frame(Pit_code=basedata$Pit_code, Ort_invdiv=apply(X = Ort2[,6:ncol(Ort2)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Ort_invdiv);dim(Ort_invdiv)
Ort_invdiv$Ort_invdiv[which(Ort_invdiv$Ort_invdiv==Inf)] <- 0

Other_invdiv <- data.frame(Pit_code=basedata$Pit_code, Other_invdiv=apply(X = Other2[,6:ncol(Other2)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Other_invdiv);dim(Other_invdiv)
Other_invdiv$Other_div[which(Other_invdiv$Other_div==Inf)] <- 0

#Merging of the diversity data (Inverse Simpsons)
divinv <- cbind(Ara_invdiv,Bla_invdiv,Col_invdiv,Form_invdiv,Ort_invdiv,Other_invdiv)
divinv_full <- divinv[,c(-3,-5,-7,-9,-11)]
divinv_full2 <- merge(taxrich,divinv_full,by='Pit_code', all.x=T, all.y=F)
head(divinv_full2);dim(divinv_full2)

#Histograms of Inverse Simpsons diversity data - frequency of zeros in the data
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
hist(divinv_full2$Ara_invdiv,main = 'Araneae Inverse Simpsons Index',ylab='Frequency',xlab='Araneae diversity')
hist(divinv_full2$Bla_invdiv,main = 'Blattodea Inverse Simpsons Index',ylab='Frequency',xlab='Blattodea diversity')
hist(divinv_full2$Col_invdiv,main = 'Coleoptera Inverse Simpsons Index',ylab='Frequency',xlab='Coleoptera diversity')
hist(divinv_full2$Form_invdiv,main = 'Formicidae Inverse Simpsons Index',ylab='Frequency',xlab='Formicidae diversity')
hist(divinv_full2$Ort_invdiv,main = 'Orthoptera Inverse Simpsons Index',ylab='Frequency',xlab='Orthoptera diversity')
hist(divinv_full2$Other_div,main = 'Other Inverse Simpsons Index',ylab='Frequency',xlab='Other diversity')

#Boxplots of Inverse Simpsons diversity data
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
boxplot(divinv_full2$Ara_invdiv~divinv_full2$Treatment+divinv_full2$Year,ylab='Araneae diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(divinv_full2$Bla_invdiv~divinv_full2$Treatment+divinv_full2$Year,ylab='Blattodea diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(divinv_full2$Col_invdiv~divinv_full2$Treatment+divinv_full2$Year,ylab='Coleoptera diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(divinv_full2$Form_invdiv~divinv_full2$Treatment+divinv_full2$Year,ylab='Formicidae diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(divinv_full2$Ort_invdiv~divinv_full2$Treatment+divinv_full2$Year,ylab='Orthoptera diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(divinv_full2$Other_div~divinv_full2$Treatment+divinv_full2$Year,ylab='Other diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

#GLM negative binomial (modelling richness for Araneae, Coleoptera and Formicidae)
head(taxrich_full2)
taxrich_full2$Site <- as.factor(taxrich_full2$Site)
taxrich_full2$Treatment <- as.factor(taxrich_full2$Treatment)
taxrich_full2$Plot <- as.factor(as.character(taxrich_full2$Plot))
taxrich_full2$Yr <- taxrich_full2$Year-min(taxrich_full2$Year)

richgroups <- colnames(taxrich_full2)[7:ncol(taxrich_full2)]

richgroups

form.thisrun <- paste('Ara_rich',"~Treatment+Yr+Treatment:Yr+(1|Site/Plot)", sep="")

mod1<-glmmadmb(as.formula(formula), family="nbinom", data=taxrich_full2)

Ararich_mod1<-glmmadmb(Ara_rich~Treatment+Yr+Treatment:Yr+(1|Site/Plot), family="nbinom", data=taxrich_full2)
Ararich_mod1<-glmmadmb(Ara_rich~Treatment+Yr+Treatment:Yr+(1|Plot), family="nbinom", data=taxrich_full2)
summary(Ararich_mod1)

Colrich_mod1<-glmmadmb(Col_rich2~Treatment+Yr+Treatment:Yr+(1|Site/Plot), family="nbinom", data=taxrich_full2)
Colrich_mod1<-glmmadmb(Col_rich2~Treatment+Yr+Treatment:Yr+(1|Plot), family="nbinom", data=taxrich_full2)
summary(Colrich_mod1)

Formrich_mod1<-glmmadmb(Form_rich~Treatment+Yr+Treatment:Yr+(1|Site/Plot), family="nbinom", data=taxrich_full2)
Formrich_mod1<-glmmadmb(Form_rich~Treatment+Yr+Treatment:Yr+(1|Plot), family="nbinom", data=taxrich_full2)
summary(Formrich_mod1)

#Preparing diversity files for modelling
head(div_full2)
div_full2$Site <- as.factor(div_full2$Site)
div_full2$Treatment <- as.factor(div_full2$Treatment)
div_full2$Plot <- as.factor(as.character(div_full2$Plot))
div_full2$Yr <- div_full2$Year-min(div_full2$Year)

head(divinv_full2)
divinv_full2$Site <- as.factor(divinv_full2$Site)
divinv_full2$Treatment <- as.factor(divinv_full2$Treatment)
divinv_full2$Plot <- as.factor(as.character(divinv_full2$Plot))
divinv_full2$Yr <- divinv_full2$Year-min(divinv_full2$Year)
head(div_rich_full2);dim(div_rich_full2)

#Binomial models for Richness (Blattodea, Orthopera and Other)
bindat <- div_rich_full2[,c(1:6,which(colnames(div_rich_full2) %in% c("Bla_rich","Other_rich2","Ort_rich2")))]
head(bindat)
bindat$Bla_rich[which(bindat$Bla_rich>0)] <- 1
bindat$Other_rich2[which(bindat$Other_rich2>0)] <- 1
bindat$Ort_rich2[which(bindat$Ort_rich2>0)] <- 1
bindat$Yr <- bindat$Year-min(bindat$Year)
bindat$Treatment <- as.factor(bindat$Treatment)

Blarich_mod1 <- glmer(Bla_rich ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=binomial,data=bindat)
summary(Blarich_mod1)

Ortrich_mod1 <- glmer(Ort_rich2 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=binomial,data=bindat)
summary(Ortrich_mod1)

Otherrich_mod1 <- glmer(Other_rich2 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=binomial,data=bindat)
summary(Otherrich_mod1)

#Linear modelling for Shannons diversity (Araneae, Coleoptera and Formicidae)

div_rich_full2$Yr <- div_rich_full2$Year-min(div_rich_full2$Year)
div_rich_full2$Site <- as.factor(div_rich_full2$Site)
div_rich_full2$Treatment <- as.factor(div_rich_full2$Treatment)
div_rich_full2$Plot <- as.factor(as.character(div_rich_full2$Plot))
head(div_rich_full2)

Aradiv_mod1 <- lmer(Ara_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=div_rich_full2)
summary(Aradiv_mod1)

Coldiv_mod1 <- lmer(Col_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=div_rich_full2)
summary(Coldiv_mod1)

Formdiv_mod1 <- lmer(Form_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=div_rich_full2)
summary(Formdiv_mod1)

#Estimates
mod1.b<-lm(ar_neutral~trt, data=gd_all)
summary(mod1.b); anova(mod1.b)

Formdiv_mod1 <- lmer(Form_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=div_rich_full2)
summary(Formdiv_mod1); anova(Formdiv_mod1)
nd_form<-data.frame()

Blarich_mod1 <- glmer(Bla_rich ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=binomial,data=bindat)
summary(Blarich_mod1)
Blarich_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(bindat$Treatment),2))
Blarich_pr <- predictSE(Blarich_mod1,newdata=Blarich_nd,se.fit=T,type='response')
Blarich_pr<-data.frame(Blarich_nd, fit=Blarich_pr$fit, se=Blarich_pr$se.fit)
Blarich_pr$lci<-Blarich_pr$fit-(1.96*Blarich_pr$se)
Blarich_pr$uci<-Blarich_pr$fit+(1.96*Blarich_pr$se)
Blarich_pr
dev.new(width=8,height=8,dpi=80,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,1),mar=c(5,5,1,1))
plot(1:4,Blarich_pr$fit,ylim=c(0,1),type='p',pch=20,xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Probability of Occurence',xlab='',main='Blattodea Richness',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Blarich_pr$lci,1:4,Blarich_pr$uci,length=0.2,angle=90,code=3)

#For plotting between ci
plot(1:4,Blarich_pr$fit,ylim=c(min(Blarich_pr$lci),max(Blarich_pr$uci)),type='p',pch=20,xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Probability of Occurence',xlab='')


nd_neut<-data.frame(trt=levels(gd_all$trt))
p_neut<-predict(mod1.b, newdata = nd_neut, se.fit=T)
p_neut<-data.frame(nd_neut, fit=p_neut$fit, se=p_neut$se.fit)
p_neut$lci<-p_neut$fit-(1.96*p_neut$se)
p_neut$uci<-p_neut$fit+(1.96*p_neut$se)
p_neut

Aradiv_mod1 <- lmer(Ara_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=div_rich_full2)
Ara_coeff <- summary(Aradiv_mod1)$coefficients[,1]
str(summary(Aradiv_mod1))
levels(div_rich_full2$Treatment)
unique(div_rich_full2$Yr)
range(div_rich_full2$Ara_div)
Ara_coeff[1]
Ara_coeff[1]+Ara_coeff[2]
Ara_coeff[1]+(Ara_coeff[3]*3)
Ara_coeff[1]+Ara_coeff[2]+(Ara_coeff[3]*3)+(Ara_coeff[4]*3)
Aradiv_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(div_rich_full2$Treatment),2))
Aradiv_pr <- predictSE(Aradiv_mod1,newdata=Aradiv_nd,se.fit=T)
Aradiv_pr<-data.frame(Aradiv_nd, fit=Aradiv_pr$fit, se=Aradiv_pr$se.fit)
Aradiv_pr$lci<-Aradiv_pr$fit-(1.96*Aradiv_pr$se)
Aradiv_pr$uci<-Aradiv_pr$fit+(1.96*Aradiv_pr$se)
Aradiv_pr
