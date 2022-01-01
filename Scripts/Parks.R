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

#Load data files ----

Araneae <- read.csv("Data/Araneae.csv",header=T)
head(Araneae)
Blattodea <- read.csv("Data/Blattodea.csv",header=T)
head(Blattodea)
Coleoptera <- read.csv("Data/Coleoptera.csv",header=T)
head(Coleoptera)
Formicidae <- read.csv("Data/Formicidae.csv",header=T)
head(Formicidae)
Morphospecies <- read.csv("Data/Morphospecies.csv",header=T)
head(Morphospecies)
Orthoptera <- read.csv("Data/Orthoptera.csv",header=T)
head(Orthoptera)
Other <- read.csv("Data/Other.csv",header=T)
head(Other)
Site <- read.csv("Data/Site_data.csv",header=T)
head(Site);dim(Site)

#Close load data ----

#Species information
head(Morphospecies);dim(Morphospecies)

#Site data (Site_data)
head(Other[,1:10],3);dim(Other)
colnames(Other)[6:ncol(Other)] %in% Morphospecies$Morphospecies

table(Morphospecies$Order)
Morphospecies[grep(pattern = 'Larvae',x = Morphospecies$Morphospecies),]

#Summary data - abundance overall and by year/site ----
#Araneae summary data
Ara2 <- Araneae[,c(-3,-5)]
Ara_summ <- Ara2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)
head(Ara2)

#Araneae total abundance
Ara_ttl <- Ara2[,c(-1,-2,-3)] %>% summarise_all(sum)
Ara_ttlabun <- as.data.frame(t(Ara_ttl))
names(Ara_ttlabun)[names(Ara_ttlabun) == 'V1'] <- "Abundance"

#Araneae by year 
Ara_yr <- Ara2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Ara_yrabun <- as.data.frame(t(Ara_yr))
colnames(Ara_yrabun) <- Ara_yrabun[1,]
Ara_yrabun <- Ara_yrabun[(-1),]

#Araneae by site
Ara_site <- Ara2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Ara_siteabun <- as.data.frame(t(Ara_site))
colnames(Ara_siteabun) <- Ara_siteabun[1,]
Ara_siteabun <- Ara_siteabun[(-1),]

#Blattodea summary data
Bla2 <- Blattodea[,c(-3,-5)]
Bla_summ <- Bla2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)

#Blattodea total abundance
Bla_ttl <- Bla2[,c(-1,-2,-3)] %>% summarise_all(sum)
Bla_ttlabun <- as.data.frame(t(Bla_ttl))
names(Bla_ttlabun)[names(Bla_ttlabun) == 'V1'] <- "Abundance"

#Blattodea by year 
Bla_yr <- Bla2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Bla_yrabun <- as.data.frame(t(Bla_yr))
colnames(Bla_yrabun) <- Bla_yrabun[1,]
Bla_yrabun <- Bla_yrabun[(-1),]

#Blattodea by site
Bla_site <- Bla2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Bla_siteabun <- as.data.frame(t(Bla_site))
colnames(Bla_siteabun) <- Bla_siteabun[1,]
Bla_siteabun <- Bla_siteabun[(-1),]

#Coleoptera summary data
Col2 <- Coleoptera[,c(-3,-5,-42,-43,-44,-45,-46,-47,-48,-49)]
Col_summ <- Col2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)

#Coleoptera total abundance
Col_ttl <- Col2[,c(-1,-2,-3)] %>% summarise_all(sum)
Col_ttlabun <- as.data.frame(t(Col_ttl))
names(Col_ttlabun)[names(Col_ttlabun) == 'V1'] <- "Abundance"

#Coleoptera by year 
Col_yr <- Col2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Col_yrabun <- as.data.frame(t(Col_yr))
colnames(Col_yrabun) <- Col_yrabun[1,]
Col_yrabun <- Col_yrabun[(-1),]

#Coleoptera by site
Col_site <- Col2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Col_siteabun <- as.data.frame(t(Col_site))
colnames(Col_siteabun) <- Col_siteabun[1,]
Col_siteabun <- Col_siteabun[(-1),]

#Formicidae summary data
Form2 <- Formicidae[,c(-3,-5)]
Form_summ <- Form2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)

#Formicidae total abundance
Form_ttl <- Form2[,c(-1,-2,-3)] %>% summarise_all(sum)
Form_ttlabun <- as.data.frame(t(Form_ttl))
names(Form_ttlabun)[names(Form_ttlabun) == 'V1'] <- "Abundance"

#Formicidae by year 
Form_yr <- Form2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Form_yrabun <- as.data.frame(t(Form_yr))
colnames(Form_yrabun) <- Form_yrabun[1,]
Form_yrabun <- Form_yrabun[(-1),]

#Formicidae by site
Form_site <- Form2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Form_siteabun <- as.data.frame(t(Form_site))
colnames(Form_siteabun) <- Form_siteabun[1,]
Form_siteabun <- Form_siteabun[(-1),]

#Orthoptera summary data
Ort2 <- Orthoptera[,c(-3,-5,-7,-11)]
Ort_summ <- Ort2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)

#Orthoptera total abundance
Ort_ttl <- Ort2[,c(-1,-2,-3)] %>% summarise_all(sum)
Ort_ttlabun <- as.data.frame(t(Ort_ttl))
names(Ort_ttlabun)[names(Ort_ttlabun) == 'V1'] <- "Abundance"

#Orthoptera by year 
Ort_yr <- Ort2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Ort_yrabun <- as.data.frame(t(Ort_yr))
colnames(Ort_yrabun) <- Ort_yrabun[1,]
Ort_yrabun <- Ort_yrabun[(-1),]

#Orthoptera by site
Ort_site <- Ort2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Ort_siteabun <- as.data.frame(t(Ort_site))
colnames(Ort_siteabun) <- Ort_siteabun[1,]
Ort_siteabun <- Ort_siteabun[(-1),]

#Other species summary data
Other2 <- Other[,c(-3,-5,-11,-21,-28,-29,-30,-31,-32,-33,-34)]
Other_summ <- Other2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)

#Other total abundance
Other_ttl <- Other2[,c(-1,-2,-3)] %>% summarise_all(sum)
Other_ttlabun <- as.data.frame(t(Other_ttl))
names(Other_ttlabun)[names(Other_ttlabun) == 'V1'] <- "Abundance"

#Other by year 
Other_yr <- Other2[,c(-2,-3)] %>% group_by(Year) %>% summarise_all(sum)
Other_yrabun <- as.data.frame(t(Other_yr))
colnames(Other_yrabun) <- Other_yrabun[1,]
Other_yrabun <- Other_yrabun[(-1),]

#Other by site
Other_site <- Other2[,c(-1,-3)] %>% group_by(Site) %>% summarise_all(sum)
Other_siteabun <- as.data.frame(t(Other_site))
colnames(Other_siteabun) <- Other_siteabun[1,]
Other_siteabun <- Other_siteabun[(-1),]

Morpho2 <- Morphospecies[,c(-2,-3)]
Morpho2 <- group_by(Morphospecies)
unique(Morpho2$Morphospecies)
unique(Morpho2$Order)
Morpho_summ <- sapply(Morphospecies, function(Order) length(unique(Order)))

full_summ <- cbind(Ara_summ, Bla_summ, Col_summ, Form_summ, Ort_summ, Other_summ)
full_summ <- full_summ[,c(-81,-82,-83,-92,-93,-94,-156,-157,-158,-226,-227,-228)]

#Merging abundance data
#Total abundance
ttl_abun <- rbind(Ara_ttlabun,Bla_ttlabun,Col_ttlabun,Form_ttlabun,Ort_ttlabun,Other_ttlabun)

#Abundance by year
year_abun <- rbind(Ara_yrabun,Bla_yrabun,Col_yrabun,Form_yrabun,Ort_yrabun,Other_yrabun)

#Abundance by site
site_abun <- rbind(Ara_siteabun,Bla_siteabun,Col_siteabun,Form_siteabun,Ort_siteabun,Other_siteabun)

#Close summaries ----

#Merge with Morphospecies file (check of row names no longer works once spaces in species names became '.')
rownames(ttl_abun)[1:nrow(ttl_abun)] %in% Morphospecies$Morphospecies
rownames(year_abun)[1:nrow(year_abun)] %in% Morphospecies$Morphospecies
rownames(site_abun)[1:nrow(site_abun)] %in% Morphospecies$Morphospecies
Morpho_abun <- cbind(Morphospecies[-c(122,123,124,125,126,127,128,129,206,210,226,236,243,244,245,246,247,248,249),],ttl_abun,year_abun,site_abun)
head(Morpho_abun);dim(Morpho_abun)
hist(Morpho_abun$Abundance[Morpho_abun$Abundance<100])

#Create taxanomic richness data ----

taxrich <- Araneae[,1:5]
taxrich$Pit_code <- paste(taxrich$Site,taxrich$Plot,taxrich$Treatment,taxrich$Pitfall,taxrich$Year,sep='_')
head(taxrich);dim(taxrich)

Ara_rich <- data.frame(Pit_code=taxrich$Pit_code, Ara_rich=apply(X = Ara2[,4:ncol(Ara2)], MARGIN = 1, FUN = function(x) length(which(x>0))))
head(Ara_rich);dim(Ara_rich)

Bla_rich <- data.frame(Pit_code=taxrich$Pit_code, Bla_rich=apply(X = Bla2[,4:ncol(Bla2)], MARGIN = 1, FUN = function(x) length(which(x>0))))
head(Bla_rich);dim(Bla_rich)

Col_rich <- data.frame(Pit_code=taxrich$Pit_code, Col_rich=apply(X = Col2[,4:ncol(Col2)], MARGIN = 1, FUN = function(x) length(which(x>0))))
head(Col_rich);dim(Col_rich)

Form_rich <- data.frame(Pit_code=taxrich$Pit_code, Form_rich=apply(X = Form2[,4:ncol(Form2)], MARGIN = 1, FUN = function(x) length(which(x>0))))
head(Form_rich);dim(Form_rich)

Ort_rich <- data.frame(Pit_code=taxrich$Pit_code, Ort_rich=apply(X = Ort2[,4:ncol(Ort2)], MARGIN = 1, FUN = function(x) length(which(x>0))))
head(Ort_rich);dim(Ort_rich)

Other_rich <- data.frame(Pit_code=taxrich$Pit_code, Other_rich=apply(X = Other2[,4:ncol(Other2)], MARGIN = 1, FUN = function(x) length(which(x>0))))
head(Other_rich);dim(Other_rich)

#Merging and preparing all richness files
rich_nolarvae <- cbind(Ara_rich,Bla_rich,Col_rich,Form_rich,Ort_rich,Other_rich)
rich_nolarvae <- rich_nolarvae[,c(-3,-5,-7,-9,-11)]
taxrich_full2<- merge(taxrich,rich_nolarvae,by='Pit_code', all.x=T, all.y=F)
head(taxrich_full2);dim(taxrich_full2)

taxrich_full2$Site <- as.factor(taxrich_full2$Site)
taxrich_full2$Treatment <- as.factor(taxrich_full2$Treatment)
taxrich_full2$Plot <- as.factor(as.character(taxrich_full2$Plot))
taxrich_full2$Yr <- taxrich_full2$Year-min(taxrich_full2$Year)

richgroups <- colnames(taxrich_full2)[7:ncol(taxrich_full2)]

richgroups

#Close taxanomic richness ----

#Graphs of richness ----

#Boxplots of richness
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
boxplot(taxrich_full2$Ara_rich~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Araneae richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(taxrich_full2$Bla_rich~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Blattodea richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(taxrich_full2$Col_rich~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Coleoptera richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(taxrich_full2$Form_rich~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Formicidae richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(taxrich_full2$Ort_rich~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Orthoptera richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(taxrich_full2$Other_rich~taxrich_full2$Treatment+taxrich_full2$Year,ylab='Other richness',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

#Histrograms of richness
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
hist(taxrich_full2$Ara_rich,main = 'Araneae Richness',ylab='Frequency',xlab='Araneae richness',las=1)
hist(taxrich_full2$Bla_rich,main = 'Blattodea richness',ylab='Frequency',xlab='Blattodea richness',las=1)
hist(taxrich_full2$Col_rich,main = 'Coleoptera richness',ylab='Frequency',xlab='Coleoptera richness',las=1)
hist(taxrich_full2$Form_rich,main = 'Formicidae Richness',ylab='Frequency',xlab='Formicidae richness',las=1)
hist(taxrich_full2$Ort_rich,main = 'Orthoptera richness',ylab='Frequency',xlab='Orthoptera richness',las=1)
hist(taxrich_full2$Other_rich,main = 'Other richness',ylab='Frequency',xlab='Other richness',las=1)

#Close graphs of richness ----

#Species diversity (Shannons) ----

head(taxrich_full2);dim(taxrich_full2)
head(Ara2[,1:10]);dim(Ara2)
apply(X = Ara2[,4:ncol(Ara2)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1))

Ara_div <- data.frame(Pit_code=taxrich$Pit_code, Ara_div=apply(X = Ara2[,4:ncol(Ara2)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Ara_div);dim(Ara_div)

Bla_div <- data.frame(Pit_code=taxrich$Pit_code, Bla_div=apply(X = Bla2[,4:ncol(Bla2)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Bla_div);dim(Bla_div)

Col_div <- data.frame(Pit_code=taxrich$Pit_code, Col_div=apply(X = Col2[,4:ncol(Col2)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Col_div);dim(Col_div)

Form_div <- data.frame(Pit_code=taxrich$Pit_code, Form_div=apply(X = Form2[,4:ncol(Form2)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Form_div);dim(Form_div)

Ort_div <- data.frame(Pit_code=taxrich$Pit_code, Ort_div=apply(X = Ort2[,4:ncol(Ort2)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Ort_div);dim(Ort_div)

Other_div <- data.frame(Pit_code=taxrich$Pit_code, Other_div=apply(X = Other2[,4:ncol(Other2)], MARGIN = 1, FUN = function(x) diversity(x,index="shannon",MARGIN=1)))
head(Other_div);dim(Other_div)

#Merging of the diversity data (Shannons) and preparing for modelling
div_full <- cbind(Ara_div,Bla_div,Col_div,Form_div,Ort_div,Other_div)
div_full <- div_full[,c(-3,-5,-7,-9,-11)]
div_full <- merge(taxrich,div_full,by='Pit_code', all.x=T, all.y=F)
head(div_full);dim(div_full)

div_full$Site <- as.factor(div_full$Site)
div_full$Treatment <- as.factor(div_full$Treatment)
div_full$Plot <- as.factor(as.character(div_full$Plot))
div_full$Yr <- div_full$Year-min(div_full$Year)

#Close Shannons diversity ----

#Graphs of Shannons diversity ----

#Histograms of Shannons diversity data - frequency of zeros in the data
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
hist(div_full$Ara_div,main = 'Araneae Shannons Index',ylab='Frequency',xlab='Araneae diversity')
hist(div_full$Bla_div,main = 'Blattodea Shannons Index',ylab='Frequency',xlab='Blattodea diversity')
hist(div_full$Col_div,main = 'Coleoptera Shannons Index',ylab='Frequency',xlab='Coleoptera diversity')
hist(div_full$Form_div,main = 'Formicidae Shannons Index',ylab='Frequency',xlab='Formicidae diversity')
hist(div_full$Ort_div,main = 'Orthoptera Shannons Index',ylab='Frequency',xlab='Orthoptera diversity')
hist(div_full$Other_div,main = 'Other Shannons Index',ylab='Frequency',xlab='Other diversity')

#Boxplots of Shannons diversity data
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
boxplot(div_full$Ara_div~div_full$Treatment+div_full$Year,ylab='Araneae diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(div_full$Bla_div~div_full$Treatment+div_full$Year,ylab='Blattodea diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(div_full$Col_div~div_full$Treatment+div_full$Year,ylab='Coleoptera diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(div_full$Form_div~div_full$Treatment+div_full$Year,ylab='Formicidae diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(div_full$Ort_div~div_full$Treatment+div_full$Year,ylab='Orthoptera diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(div_full$Other_div~div_full$Treatment+div_full$Year,ylab='Other diversity (Shannons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

#Close graphs of Shannons diversity ---- 

#Merging of diversity and richness
div_rich_full2 <- cbind(div_full,taxrich_full2)
div_rich_full2 <- div_rich_full2[,c(-14,-15,-16,-17,-18,-19)]
head(div_rich_full2);dim(div_rich_full2)
richtab <- div_rich_full2[,c(2:6,grep(pattern='rich',colnames(div_rich_full2)))]
head(richtab)

div_rich_full2$Yr <- div_rich_full2$Year-min(div_rich_full2$Year)
div_rich_full2$Site <- as.factor(div_rich_full2$Site)
div_rich_full2$Treatment <- as.factor(div_rich_full2$Treatment)
div_rich_full2$Plot <- as.factor(as.character(div_rich_full2$Plot))

#How many zeros are in each morphospecies
apply(richtab[,6:length(richtab)],2,FUN = function(x)table(x==0))

#Species diversity (Inverse Simpson's) ----

#Warning numeric subsets
head(taxrich_full2);dim(taxrich_full2)
head(Araneae[,1:10]);dim(Araneae)
apply(X = Araneae[,6:ncol(Araneae)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1))

Ara_invdiv <- data.frame(Pit_code=taxrich$Pit_code, Ara_invdiv=apply(X = Ara2[,4:ncol(Ara2)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Ara_invdiv);dim(Ara_invdiv)
Ara_invdiv$Ara_invdiv[which(Ara_invdiv$Ara_invdiv==Inf)] <- 0

Bla_invdiv <- data.frame(Pit_code=taxrich$Pit_code, Bla_invdiv=apply(X = Bla2[,4:ncol(Bla2)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Bla_invdiv);dim(Bla_invdiv)
Bla_invdiv$Bla_invdiv[which(Bla_invdiv$Bla_invdiv==Inf)] <- 0

Col_invdiv <- data.frame(Pit_code=taxrich$Pit_code, Col_invdiv=apply(X = Col2[,4:ncol(Col2)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Col_invdiv);dim(Col_invdiv)
Col_invdiv$Col_invdiv[which(Col_invdiv$Col_invdiv==Inf)] <- 0

Form_invdiv <- data.frame(Pit_code=taxrich$Pit_code, Form_invdiv=apply(X = Form2[,4:ncol(Form2)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Form_invdiv);dim(Form_invdiv)
Form_invdiv$Form_invdiv[which(Form_invdiv$Form_invdiv==Inf)] <- 0

Ort_invdiv <- data.frame(Pit_code=taxrich$Pit_code, Ort_invdiv=apply(X = Ort2[,4:ncol(Ort2)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Ort_invdiv);dim(Ort_invdiv)
Ort_invdiv$Ort_invdiv[which(Ort_invdiv$Ort_invdiv==Inf)] <- 0

Other_invdiv <- data.frame(Pit_code=taxrich$Pit_code, Other_invdiv=apply(X = Other2[,4:ncol(Other2)], MARGIN = 1, FUN = function(x) diversity(x,index="invsimpson",MARGIN=1)))
head(Other_invdiv);dim(Other_invdiv)
Other_invdiv$Other_invdiv[which(Other_invdiv$Other_invdiv==Inf)] <- 0

#Merging and preparing the diversity (Inverse Simpsons) data for modelling
divinv <- cbind(Ara_invdiv,Bla_invdiv,Col_invdiv,Form_invdiv,Ort_invdiv,Other_invdiv)
divinv_full <- divinv[,c(-3,-5,-7,-9,-11)]
divinv_full <- merge(taxrich,divinv_full,by='Pit_code', all.x=T, all.y=F)
head(divinv_full);dim(divinv_full)

#Make zeros tiny number
divinv_full[which(divinv_full$Ara_invdiv==0),]
divinv_full[which(divinv_full$Col_invdiv==0),]
divinv_full[which(divinv_full$Form_invdiv==0),]

divinv_full$Ara_invdiv[which(divinv_full$Ara_invdiv==0)]<-0.000001
divinv_full$Col_invdiv[which(divinv_full$Col_invdiv==0)]<-0.000001

head(divinv_full)
divinv_full$Site <- as.factor(divinv_full$Site)
divinv_full$Treatment <- as.factor(divinv_full$Treatment)
divinv_full$Plot <- as.factor(as.character(divinv_full$Plot))
divinv_full$Yr <- divinv_full$Year-min(divinv_full$Year)

#Close Inverse Simpsons diversity ----

#Graphs for Inverse Simpsons diversity ----

#Histograms of Inverse Simpsons diversity data - frequency of zeros in the data
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
hist(divinv_full$Ara_invdiv,main = 'Araneae Inverse Simpsons Index',ylab='Frequency',xlab='Araneae diversity')
hist(divinv_full$Bla_invdiv,main = 'Blattodea Inverse Simpsons Index',ylab='Frequency',xlab='Blattodea diversity')
hist(divinv_full$Col_invdiv,main = 'Coleoptera Inverse Simpsons Index',ylab='Frequency',xlab='Coleoptera diversity')
hist(divinv_full$Form_invdiv,main = 'Formicidae Inverse Simpsons Index',ylab='Frequency',xlab='Formicidae diversity')
hist(divinv_full$Ort_invdiv,main = 'Orthoptera Inverse Simpsons Index',ylab='Frequency',xlab='Orthoptera diversity')
hist(divinv_full$Other_invdiv,main = 'Other Inverse Simpsons Index',ylab='Frequency',xlab='Other diversity')

#Boxplots of Inverse Simpsons diversity data

dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
boxplot(divinv_full$Ara_invdiv~divinv_full$Treatment+divinv_full$Year,ylab='Araneae diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(divinv_full$Bla_invdiv~divinv_full$Treatment+divinv_full$Year,ylab='Blattodea diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(divinv_full$Col_invdiv~divinv_full$Treatment+divinv_full$Year,ylab='Coleoptera diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(divinv_full$Form_invdiv~divinv_full$Treatment+divinv_full$Year,ylab='Formicidae diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(divinv_full$Ort_invdiv~divinv_full$Treatment+divinv_full$Year,ylab='Orthoptera diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

boxplot(divinv_full$Other_invdiv~divinv_full$Treatment+divinv_full$Year,ylab='Other diversity (Inv Simpsons)',xlab='',xaxt='n',las=1)
axis(side=1,at=1:4,labels=c('C_2016','R_2016','C_2019','R_2019'))

#Close graphs of Inverse Simpsons diversity ----

#GLM negative binomial (modelling richness for Araneae, Coleoptera and Formicidae) ----

form.thisrun <- paste('Ara_rich',"~Treatment+Yr+Treatment:Yr+(1|Site/Plot)", sep="")

mod1<-glmmadmb(as.formula(formula), family="nbinom", data=taxrich_full2)

Ararich_mod1<-glmmadmb(Ara_rich~Treatment+Yr+Treatment:Yr+(1|Site/Plot), family="nbinom", data=taxrich_full2,admb.opts=admbControl(impSamp=200,shess=FALSE,noinit=FALSE))
Ararich_mod1<-glmmadmb(Ara_rich~Treatment+Yr+Treatment:Yr+(1|Plot), family="nbinom", data=taxrich_full2)
summary(Ararich_mod1)

Colrich_mod1<-glmmadmb(Col_rich~Treatment+Yr+Treatment:Yr+(1|Site/Plot), family="nbinom", data=taxrich_full2)
Colrich_mod1<-glmmadmb(Col_rich~Treatment+Yr+Treatment:Yr+(1|Plot), family="nbinom", data=taxrich_full2)
summary(Colrich_mod1)

Formrich_mod1<-glmmadmb(Form_rich~Treatment+Yr+Treatment:Yr+(1|Site/Plot), family="nbinom", data=taxrich_full2)
Formrich_mod1<-glmmadmb(Form_rich~Treatment+Yr+Treatment:Yr+(1|Plot), family="nbinom", data=taxrich_full2)
summary(Formrich_mod1)

#Close GLM(nb) modelling ----

#Binomial models for Richness (Blattodea, Orthopera and Other)----

bindat <- div_rich_full2[,c(1:6,which(colnames(div_rich_full2) %in% c("Bla_rich","Other_rich","Ort_rich")))]
head(bindat)
bindat$Bla_rich[which(bindat$Bla_rich>0)] <- 1
bindat$Other_rich[which(bindat$Other_rich>0)] <- 1
bindat$Ort_rich[which(bindat$Ort_rich>0)] <- 1
bindat$Yr <- bindat$Year-min(bindat$Year)
bindat$Treatment <- as.factor(bindat$Treatment)

Blarich_mod1 <- glmer(Bla_rich ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=binomial,data=bindat)
summary(Blarich_mod1)

Ortrich_mod1 <- glmer(Ort_rich ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=binomial,data=bindat)
summary(Ortrich_mod1)

Otherrich_mod1 <- glmer(Other_rich ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=binomial,data=bindat)
summary(Otherrich_mod1)

#Close binomial modelling of Richness ----

#Linear modelling for Shannons diversity (Araneae, Coleoptera and Formicidae) ----
  
#Initial attempt with Shannons - made NA to model as 1's in richness were equating to inflation of 0's in the diversity data
#Cannot use gamma to model with 0's in the data

head(div_rich_full2)
str(div_rich_full2)

Aradiv_mod1 <- lmer(Ara_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=div_rich_full2)
summary(Aradiv_mod1)

Coldiv_mod1 <- lmer(Col_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=div_rich_full2)
summary(Coldiv_mod1)

Formdiv_mod1 <- lmer(Form_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=div_rich_full2)
summary(Formdiv_mod1)

#Gamma models (values have to be >0)
div_rich_full2$Col_div[which(div_rich_full2$Col_div==0)]<-NA
div_rich_full2$Ara_div[which(div_rich_full2$Ara_div==0)]<-NA

Aradiv_mod1 <- glmer(Ara_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=div_rich_full2)
summary(Aradiv_mod1)

Coldiv_mod1 <- glmer(Col_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=div_rich_full2)
summary(Coldiv_mod1)

Formdiv_mod1 <- glmer(Form_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=div_rich_full2)
summary(Formdiv_mod1)

#Close linear modelling for Shannons diversity ----

#Modelling for Inverse Simpsons diversity ----

head(divinv_full)

Arainvdiv_mod1 <- glmer(Ara_invdiv ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=divinv_full)
summary(Arainvdiv_mod1)

Colinvdiv_mod1 <- glmer(Col_invdiv ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=divinv_full)
summary(Colinvdiv_mod1)

Forminvdiv_mod1 <- glmer(Form_invdiv ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=divinv_full)
summary(Forminvdiv_mod1)

#Close modelling for Inverse Simpsons diversity ----

#Estimates for Richness ----
mod1.b<-lm(ar_neutral~trt, data=gd_all)
summary(mod1.b); anova(mod1.b)

Blarich_mod1 <- glmer(Bla_rich ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=binomial,data=bindat)
summary(Blarich_mod1)
Blarich_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(bindat$Treatment),2))
Blarich_pr <- predictSE(Blarich_mod1,newdata=Blarich_nd,se.fit=T,type='response')
Blarich_pr<-data.frame(Blarich_nd, fit=Blarich_pr$fit, se=Blarich_pr$se.fit)
Blarich_pr$lci<-Blarich_pr$fit-(1.96*Blarich_pr$se)
Blarich_pr$uci<-Blarich_pr$fit+(1.96*Blarich_pr$se)
Blarich_pr
dev.new(width=12,height=8,dpi=80,pointsize=20,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(5,5,1,1))
plot(1:4,Blarich_pr$fit,ylim=c(0,1),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Probability of Occurence',xlab='',main='Blattodea Richness',font.main=1)
legend("bottomleft", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Blarich_pr$lci,1:4,Blarich_pr$uci,length=0.1,angle=90,code=3)

Ortrich_mod1 <- glmer(Ort_rich ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=binomial,data=bindat)
summary(Ortrich_mod1)
Ortrich_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(bindat$Treatment),2))
Ortrich_pr <- predictSE(Ortrich_mod1,newdata=Ortrich_nd,se.fit=T,type='response')
Ortrich_pr<-data.frame(Ortrich_nd, fit=Ortrich_pr$fit, se=Ortrich_pr$se.fit)
Ortrich_pr$lci<-Ortrich_pr$fit-(1.96*Ortrich_pr$se)
Ortrich_pr$uci<-Ortrich_pr$fit+(1.96*Ortrich_pr$se)
Ortrich_pr
plot(1:4,Ortrich_pr$fit,ylim=c(0,1),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Probability of Occurence',xlab='',main='Orthoptera Richness',font.main=1)
legend("bottomleft", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Ortrich_pr$lci,1:4,Ortrich_pr$uci,length=0.1,angle=90,code=3)

Otherrich_mod1 <- glmer(Other_rich ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=binomial,data=bindat)
summary(Otherrich_mod1)
Otherrich_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(bindat$Treatment),2))
Otherrich_pr <- predictSE(Otherrich_mod1,newdata=Otherrich_nd,se.fit=T,type='response')
Otherrich_pr<-data.frame(Otherrich_nd, fit=Otherrich_pr$fit, se=Otherrich_pr$se.fit)
Otherrich_pr$lci<-Otherrich_pr$fit-(1.96*Otherrich_pr$se)
Otherrich_pr$uci<-Otherrich_pr$fit+(1.96*Otherrich_pr$se)
Otherrich_pr
plot(1:4,Otherrich_pr$fit,ylim=c(0,1),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Probability of Occurence',xlab='',main='Other Richness',font.main=1)
legend("bottomleft", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Otherrich_pr$lci,1:4,Otherrich_pr$uci,length=0.1,angle=90,code=3)

str(taxrich_full2)

admbControl(impSamp=0,maxfn=10000,imaxfn=500,maxph=5,noinit=TRUE,shess=TRUE,run=TRUE, ZI_kluge=FALSE, poiss_prob_bound=TRUE)
mcmcControl(mcmc = 10000, mcmc2=0, mcsave, mcnoscale = FALSE, mcgrope = FALSE, mcmult = 1)

table(taxrich_full2$Ara_rich,taxrich_full2$Treatment,taxrich_full2$Year)
aggregate(Ara_rich ~ Treatment+Year+Site,data=taxrich_full2,FUN=mean)
head(taxrich_full2)

Ararich_mod1<-glmmadmb(Ara_rich~Treatment+Yr+Treatment:Yr+(1|Site/Plot), family="nbinom", data=taxrich_full2,admb.opts=admbControl(impSamp=200,shess=FALSE,noinit=FALSE))
summary(Ararich_mod1)
Ararich_nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(taxrich_full2$Treatment),2),levels=levels(taxrich_full2$Treatment)))
Ararich_pr <- predict(Ararich_mod1,newdata=Ararich_nd,se.fit=T,type='link')
Ararich_pr<-data.frame(Ararich_nd, fit.link=Ararich_pr$fit, se=Ararich_pr$se.fit)
Ararich_pr$lci.link<-Ararich_pr$fit-(1.96*Ararich_pr$se)
Ararich_pr$uci.link<-Ararich_pr$fit+(1.96*Ararich_pr$se)
Ararich_pr$fit.resp<-exp(Ararich_pr$fit.link)
Ararich_pr$lci.resp<-exp(Ararich_pr$lci.link)
Ararich_pr$uci.resp<-exp(Ararich_pr$uci.link)
Ararich_pr
plot(1:4,Ararich_pr$fit.resp,ylim=c(min(Ararich_pr$lci.resp),max(Ararich_pr$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species Richness',xlab='',main='Araneae Richness',font.main=1)
legend("bottomleft", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Ararich_pr$lci.resp,1:4,Ararich_pr$uci.resp,length=0.1,angle=90,code=3)

Colrich_mod1<-glmmadmb(Col_rich~Treatment+Yr+Treatment:Yr+(1|Site/Plot), family="nbinom", data=taxrich_full2)
summary(Colrich_mod1)
Colrich_nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(taxrich_full2$Treatment),2),levels=levels(taxrich_full2$Treatment)))
Colrich_pr <- predict(Colrich_mod1,newdata=Colrich_nd,se.fit=TRUE,type='link')
Colrich_pr<-data.frame(Colrich_nd, fit.link=Colrich_pr$fit, se=Colrich_pr$se.fit)
Colrich_pr$lci.link<-Colrich_pr$fit-(1.96*Colrich_pr$se)
Colrich_pr$uci.link<-Colrich_pr$fit+(1.96*Colrich_pr$se)
Colrich_pr$fit.resp<-exp(Colrich_pr$fit.link)
Colrich_pr$lci.resp<-exp(Colrich_pr$lci.link)
Colrich_pr$uci.resp<-exp(Colrich_pr$uci.link)
Colrich_pr
plot(1:4,Colrich_pr$fit.resp,ylim=c(min(Colrich_pr$lci.resp),max(Colrich_pr$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species Richness',xlab='',main='Coleoptera Richness',font.main=1)
legend("bottomleft", legend=c("Control", "Rock"), pch = c(20,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Colrich_pr$lci.resp,1:4,Colrich_pr$uci.resp,length=0.1,angle=90,code=3)

Formrich_mod1<-glmmadmb(Form_rich~Treatment+Yr+Treatment:Yr+(1|Site/Plot), family="nbinom", data=taxrich_full2)
summary(Formrich_mod1)
Formrich_nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(taxrich_full2$Treatment),2),levels=levels(taxrich_full2$Treatment)))
Formrich_pr <- predict(Formrich_mod1,newdata=Formrich_nd,se.fit=T,type='link')
Formrich_pr<-data.frame(Formrich_nd, fit.link=Formrich_pr$fit, se=Formrich_pr$se.fit)
Formrich_pr$lci.link<-Formrich_pr$fit-(1.96*Formrich_pr$se)
Formrich_pr$uci.link<-Formrich_pr$fit+(1.96*Formrich_pr$se)
Formrich_pr$fit.resp<-exp(Formrich_pr$fit.link)
Formrich_pr$lci.resp<-exp(Formrich_pr$lci.link)
Formrich_pr$uci.resp<-exp(Formrich_pr$uci.link)
Formrich_pr
plot(1:4,Formrich_pr$fit.resp,ylim=c(min(Formrich_pr$lci.resp),max(Formrich_pr$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species Richness',xlab='',main='Formicidae Richness',font.main=1)
legend("bottomleft", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Formrich_pr$lci.resp,1:4,Formrich_pr$uci.resp,length=0.1,angle=90,code=3)

#For plotting between ci
plot(1:4,Blarich_pr$fit,ylim=c(min(Blarich_pr$lci),max(Blarich_pr$uci)),type='p',pch=20,xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Probability of Occurence',xlab='')

#Close plotting of estimates for richness ----

#Estimates for diversity (Shannons) ----
head(div_rich_full2)
range(div_rich_full2$Ara_div,na.rm=T)
plot(div_rich_full2$Ara_rich,div_rich_full2$Ara_div)

Aradiv_mod1 <- lmer(Ara_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=div_rich_full2)
plot(Aradiv_mod1)
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
dev.new(width=24,height=8,dpi=80,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,3),mar=c(5,5,1,1))
plot(1:4,Aradiv_pr$fit,ylim=c(0.8,2),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='',main='Araneae diversity',font.main=1)
legend("topright", legend=c("Control", "Rock"), pch = c(20,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Aradiv_pr$lci,1:4,Aradiv_pr$uci,length=0.2,angle=90,code=3)

Coldiv_mod1 <- lmer(Col_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=div_rich_full2)
summary(Coldiv_mod1)
plot(Coldiv_mod1)
Col_coeff <- summary(Coldiv_mod1)$coefficients[,1]
str(summary(Coldiv_mod1))
levels(div_rich_full2$Treatment)
unique(div_rich_full2$Yr)
range(div_rich_full2$Col_div)
Coldiv_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(div_rich_full2$Treatment),2))
Coldiv_pr <- predictSE(Coldiv_mod1,newdata=Coldiv_nd,se.fit=T)
Coldiv_pr<-data.frame(Coldiv_nd, fit=Coldiv_pr$fit, se=Coldiv_pr$se.fit)
Coldiv_pr$lci<-Coldiv_pr$fit-(1.96*Coldiv_pr$se)
Coldiv_pr$uci<-Coldiv_pr$fit+(1.96*Coldiv_pr$se)
Coldiv_pr
plot(1:4,Coldiv_pr$fit,ylim=c(0.8,2),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='',main='Coleoptera diversity',font.main=1)
legend("topright", legend=c("Control", "Rock"), pch = c(20,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Coldiv_pr$lci,1:4,Coldiv_pr$uci,length=0.2,angle=90,code=3)

Formdiv_mod1 <- lmer(Form_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=div_rich_full2)
summary(Formdiv_mod1)
plot(Formdiv_mod1)
Form_coeff <- summary(Formdiv_mod1)$coefficients[,1]
str(summary(Formdiv_mod1))
levels(div_rich_full2$Treatment)
unique(div_rich_full2$Yr)
range(div_rich_full2$Form_div)
Form_coeff[1]
Form_coeff[1]+Form_coeff[2]
Form_coeff[1]+(Form_coeff[3]*3)
Form_coeff[1]+Form_coeff[2]+(Form_coeff[3]*3)+(Form_coeff[4]*3)
Formdiv_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(div_rich_full2$Treatment),2))
Formdiv_pr <- predictSE(Formdiv_mod1,newdata=Formdiv_nd,se.fit=T)
Formdiv_pr<-data.frame(Formdiv_nd, fit=Formdiv_pr$fit, se=Formdiv_pr$se.fit)
Formdiv_pr$lci<-Formdiv_pr$fit-(1.96*Formdiv_pr$se)
Formdiv_pr$uci<-Formdiv_pr$fit+(1.96*Formdiv_pr$se)
Formdiv_pr
plot(1:4,Formdiv_pr$fit,ylim=c(0.8,2),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='',main='Formicidae diversity',font.main=1)
legend("topright", legend=c("Control", "Rock"), pch = c(20,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Formdiv_pr$lci,1:4,Formdiv_pr$uci,length=0.2,angle=90,code=3)

#Models/estimates/plots for diversity glm(Gamma)
Aradiv_mod1 <- glmer(Ara_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=div_rich_full2)
summary(Aradiv_mod1)
plot(Aradiv_mod1)
Ara_coeff <- summary(Aradiv_mod1)$coefficients[,1]
str(summary(Aradiv_mod1))
levels(div_rich_full2$Treatment)
unique(div_rich_full2$Yr)
range(div_rich_full2$Form_div)
Ara_coeff[1]
Ara_coeff[1]+Ara_coeff[2]
Ara_coeff[1]+(Ara_coeff[3]*3)
Ara_coeff[1]+Ara_coeff[2]+(Ara_coeff[3]*3)+(Ara_coeff[4]*3)
Aradiv_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(div_rich_full2$Treatment),2))
Aradiv_pr <- predictSE(Aradiv_mod1,newdata=Aradiv_nd,se.fit=T,type='response')
Aradiv_pr<-data.frame(Aradiv_nd, fit=Aradiv_pr$fit, se=Aradiv_pr$se.fit)
Aradiv_pr$lci<-Aradiv_pr$fit-(1.96*Aradiv_pr$se)
Aradiv_pr$uci<-Aradiv_pr$fit+(1.96*Aradiv_pr$se)
Aradiv_pr
dev.new(width=12,height=4,dpi=80,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,3),mar=c(5,5,1,1))
plot(1:4,Aradiv_pr$fit,ylim=c(0.8,2),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='',main='Araneae diversity',font.main=1)
legend("topright", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Aradiv_pr$lci,1:4,Aradiv_pr$uci,length=0.1,angle=90,code=3)

Coldiv_mod1 <- glmer(Col_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=div_rich_full2)
summary(Coldiv_mod1)
Col_coeff <- summary(Coldiv_mod1)$coefficients[,1]
str(summary(Coldiv_mod1))
levels(div_rich_full2$Treatment)
unique(div_rich_full2$Yr)
range(div_rich_full2$Form_div)
Col_coeff[1]
Col_coeff[1]+Col_coeff[2]
Col_coeff[1]+(Col_coeff[3]*3)
Col_coeff[1]+Col_coeff[2]+(Col_coeff[3]*3)+(Col_coeff[4]*3)
Coldiv_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(div_rich_full2$Treatment),2))
Coldiv_pr <- predictSE(Coldiv_mod1,newdata=Coldiv_nd,se.fit=T,type='response')
Coldiv_pr<-data.frame(Coldiv_nd, fit=Coldiv_pr$fit, se=Coldiv_pr$se.fit)
Coldiv_pr$lci<-Coldiv_pr$fit-(1.96*Coldiv_pr$se)
Coldiv_pr$uci<-Coldiv_pr$fit+(1.96*Coldiv_pr$se)
Coldiv_pr
plot(1:4,Coldiv_pr$fit,ylim=c(0.8,2),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='',main='Coleoptera diversity',font.main=1)
legend("topright", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Coldiv_pr$lci,1:4,Coldiv_pr$uci,length=0.1,angle=90,code=3)

Coldiv_mod2 <- glmmTMB(Col_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=div_rich_full2)
summary(Coldiv_mod2)
Col_coeff <- summary(Coldiv_mod2)$coefficients[,1]
str(summary(Coldiv_mod2))
levels(div_rich_full2$Treatment)
unique(div_rich_full2$Yr)
range(div_rich_full2$Form_div)
Col_coeff[1]
Col_coeff[1]+Col_coeff[2]
Col_coeff[1]+(Col_coeff[3]*3)
Col_coeff[1]+Col_coeff[2]+(Col_coeff[3]*3)+(Col_coeff[4]*3)
Coldiv_nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(div_rich_full2$Treatment),2),levels=levels(div_rich_full2$Treatment)))
Coldiv_pr <- predict(Coldiv_mod2,newdata=Coldiv_nd,se.fit=T,type='link')
Coldiv_pr<-data.frame(Coldiv_nd, fit=Coldiv_pr$fit, se=Coldiv_pr$se.fit)
Coldiv_pr$lci<-Coldiv_pr$fit-(1.96*Coldiv_pr$se)
Coldiv_pr$uci<-Coldiv_pr$fit+(1.96*Coldiv_pr$se)
Coldiv_pr
plot(1:4,Coldiv_pr$fit,ylim=c(0.8,2),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='',main='Coleoptera diversity',font.main=1)
legend("topright", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Coldiv_pr$lci,1:4,Coldiv_pr$uci,length=0.1,angle=90,code=3)

Formdiv_mod1 <- glmer(Form_div ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=div_rich_full2)
summary(Formdiv_mod1)
plot(Formdiv_mod1)
Form_coeff <- summary(Formdiv_mod1)$coefficients[,1]
str(summary(Formdiv_mod1))
levels(div_rich_full2$Treatment)
unique(div_rich_full2$Yr)
range(div_rich_full2$Form_div)
Form_coeff[1]
Form_coeff[1]+Form_coeff[2]
Form_coeff[1]+(Form_coeff[3]*3)
Form_coeff[1]+Form_coeff[2]+(Form_coeff[3]*3)+(Form_coeff[4]*3)
Formdiv_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(div_rich_full2$Treatment),2))
Formdiv_pr <- predictSE(Formdiv_mod1,newdata=Formdiv_nd,se.fit=T,type='response')
Formdiv_pr<-data.frame(Formdiv_nd, fit=Formdiv_pr$fit, se=Formdiv_pr$se.fit)
Formdiv_pr$lci<-Formdiv_pr$fit-(1.96*Formdiv_pr$se)
Formdiv_pr$uci<-Formdiv_pr$fit+(1.96*Formdiv_pr$se)
Formdiv_pr
plot(1:4,Formdiv_pr$fit,ylim=c(0.8,2),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='',main='Formicidae diversity',font.main=1)
legend("topright", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Formdiv_pr$lci,1:4,Formdiv_pr$uci,length=0.1,angle=90,code=3)

range(divinv_full2$Ara_invdiv)

#Close plotting estimates of Shannons diversity ----

#Estimates for diversity (Inverse Simpsons) ----

#Models/estimates/plots for diversity glm(Gamma)
Arainvdiv_mod1 <- glmer(Ara_invdiv ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=divinv_full)
summary(Arainvdiv_mod1)
plot(Arainvdiv_mod1)
Ara_invcoeff <- summary(Arainvdiv_mod1)$coefficients[,1]
str(summary(Arainvdiv_mod1))
levels(divinv_full$Treatment)
unique(divinv_full$Yr)
range(divinv_full$Ara_invdiv)
Ara_invcoeff[1]
Ara_invcoeff[1]+Ara_invcoeff[2]
Ara_invcoeff[1]+(Ara_invcoeff[3]*3)
Ara_invcoeff[1]+Ara_invcoeff[2]+(Ara_invcoeff[3]*3)+(Ara_invcoeff[4]*3)
Arainvdiv_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(divinv_full$Treatment),2))
Arainvdiv_pr <- predictSE(Arainvdiv_mod1,newdata=Arainvdiv_nd,se.fit=T,type='response')
Arainvdiv_pr<-data.frame(Arainvdiv_nd, fit=Arainvdiv_pr$fit, se=Arainvdiv_pr$se.fit)
Arainvdiv_pr$lci<-Arainvdiv_pr$fit-(1.96*Arainvdiv_pr$se)
Arainvdiv_pr$uci<-Arainvdiv_pr$fit+(1.96*Arainvdiv_pr$se)
Arainvdiv_pr
dev.new(width=12,height=4,dpi=80,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,3),mar=c(5,5,1,1))
plot(1:4,Arainvdiv_pr$fit,ylim=c(min(Arainvdiv_pr$lci),max(Arainvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='',main='Araneae diversity',font.main=1)
legend("topright", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Arainvdiv_pr$lci,1:4,Arainvdiv_pr$uci,length=0.1,angle=90,code=3)

Colinvdiv_mod1 <- glmer(Col_invdiv ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=divinv_full)
summary(Colinvdiv_mod1)
Col_invcoeff <- summary(Colinvdiv_mod1)$coefficients[,1]
str(summary(Colinvdiv_mod1))
levels(divinv_full$Treatment)
unique(divinv_full$Yr)
range(divinv_full$Col_invdiv)
Col_invcoeff[1]
Col_invcoeff[1]+Col_invcoeff[2]
Col_invcoeff[1]+(Col_invcoeff[3]*3)
Col_invcoeff[1]+Col_invcoeff[2]+(Col_invcoeff[3]*3)+(Col_invcoeff[4]*3)
Colinvdiv_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(divinv_full$Treatment),2))
Colinvdiv_pr <- predictSE(Colinvdiv_mod1,newdata=Colinvdiv_nd,se.fit=T,type='response')
Colinvdiv_pr<-data.frame(Colinvdiv_nd, fit=Colinvdiv_pr$fit, se=Colinvdiv_pr$se.fit)
Colinvdiv_pr$lci<-Colinvdiv_pr$fit-(1.96*Colinvdiv_pr$se)
Colinvdiv_pr$uci<-Colinvdiv_pr$fit+(1.96*Colinvdiv_pr$se)
Colinvdiv_pr
plot(1:4,Colinvdiv_pr$fit,ylim=c(min(Colinvdiv_pr$lci),max(Colinvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='',main='Coleoptera diversity',font.main=1)
legend("topright", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Colinvdiv_pr$lci,1:4,Colinvdiv_pr$uci,length=0.1,angle=90,code=3)

Forminvdiv_mod1 <- glmer(Form_invdiv ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),family=Gamma(link='log'),data=divinv_full)
summary(Forminvdiv_mod1)
plot(Forminvdiv_mod1)
Form_invcoeff <- summary(Forminvdiv_mod1)$coefficients[,1]
str(summary(Forminvdiv_mod1))
levels(divinv_full$Treatment)
unique(divinv_full$Yr)
range(divinv_full$Form_invdiv)
Form_invcoeff[1]
Form_invcoeff[1]+Form_invcoeff[2]
Form_invcoeff[1]+(Form_invcoeff[3]*3)
Form_invcoeff[1]+Form_invcoeff[2]+(Form_invcoeff[3]*3)+(Form_invcoeff[4]*3)
Forminvdiv_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(divinv_full$Treatment),2))
Forminvdiv_pr <- predictSE(Forminvdiv_mod1,newdata=Forminvdiv_nd,se.fit=T,type='response')
Forminvdiv_pr<-data.frame(Forminvdiv_nd, fit=Forminvdiv_pr$fit, se=Forminvdiv_pr$se.fit)
Forminvdiv_pr$lci<-Forminvdiv_pr$fit-(1.96*Forminvdiv_pr$se)
Forminvdiv_pr$uci<-Forminvdiv_pr$fit+(1.96*Forminvdiv_pr$se)
Forminvdiv_pr
plot(1:4,Forminvdiv_pr$fit,ylim=c(min(Forminvdiv_pr$lci),max(Forminvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='',main='Formicidae diversity',font.main=1)
legend("topright", legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Forminvdiv_pr$lci,1:4,Forminvdiv_pr$uci,length=0.1,angle=90,code=3)

#Close plotting estimates for Inverse Simpsons diversity ----

#Proportion of plots with zero values ----

#Individual species
sum(Araneae$col[6,50]==0)/160

#Richness using (taxrich_full2)
sum(taxrich_full2$Ara_rich==0)/160
which(taxrich_full2$Bla_rich==0)/160
which(taxrich_full2$Col_rich==0)/160
which(taxrich_full2$Form_rich==0)/160
which(taxrich_full2$Ort_rich==0)/160
which(taxrich_full2$Other_rich==0)/160

which(taxrich_full2$Ara_rich==0)/80
which(taxrich_full2$Bla_rich==0)/80
which(taxrich_full2$Col_rich==0)/80
which(taxrich_full2$Form_rich==0)/80
which(taxrich_full2$Ort_rich==0)/80
which(taxrich_full2$Other_rich==0)/80

#MDS attempt ----

Morpho_abun$Abundance <- as.numeric(Morpho_abun$Abundance)
Morpho_abun$`2016` <- as.numeric(Morpho_abun$`2016`)
Morpho_abun$`2019` <- as.numeric(Morpho_abun$`2019`)
Morpho_abun$CR <- as.numeric(Morpho_abun$CR)
Morpho_abun$JE <- as.numeric(Morpho_abun$JE)
Morpho_abun$JW <- as.numeric(Morpho_abun$JW)
Morpho_abun$K <- as.numeric(Morpho_abun$K)
Morpho_abun$MUL <- as.numeric(Morpho_abun$MUL)

# Remove species with less than 80 samples (by yr/pitfall) or 16 samples (by site/reserve):
mds_abun <- subset(Morpho_abun,Abundance>80)
head(mds_abun,3)
mds_abunyr<-mds_abun[,c(-1,-2,-3,-4,-5,-8,-9,-10,-11,-12)]
mds_abunsite<-mds_abun[,c(-1,-2,-3,-4,-5,-6,-7)]

mds_abun2 <- subset(Morpho_abun,Abundance>16)
head(mds_abun2)
mds_abun2yr<-mds_abun2[,c(-1,-2,-3,-4,-5,-8,-9,-10,-11,-12)]
mds_abun2site<-mds_abun2[,c(-1,-2,-3,-4,-5,-6,-7)]

# Do MDS analysis using the Bray-Curtis dissimilarity index:
simmdsyr<-capscale(mds_abunyr~1, distance="bray")
simmdsyr
head(summary(simmdsyr))
str(summary(simmdsyr))
head(summary(simmdsyr)$sites)
length(summary(simmdsyr)$sites[,1])
summary(simmdsyr)$cont$importance[,1:6]

simmdssite<-capscale(mds_abunsite~1, distance="bray")
simmdssite
head(summary(simmdssite))
str(summary(simmdssite))
head(summary(simmdssite)$sites)
length(summary(simmdssite)$sites[,1])
summary(simmdssite)$cont$importance[,1:6]

simmds2yr<-capscale(mds_abun2yr~1, distance="bray")
simmds2yr
head(summary(simmds2yr))
str(summary(simmds2yr))
head(summary(simmds2yr)$sites)
length(summary(simmds2yr)$sites[,1])
summary(simmds2yr)$cont$importance[,1:6]

simmds2site<-capscale(mds_abun2site~1, distance="bray")
simmds2site
head(summary(simmds2site))
str(summary(simmds2site))
head(summary(simmds2site)$sites)
length(summary(simmds2site)$sites[,1])
summary(simmds2site)$cont$importance[,1:6]

#Analyse dissimilarity (not sure about this)

site_mds<-cbind(mds_abun,site_mds1=as.numeric(summary(simmdssite)$sites[,1]),site_mds2=as.numeric(summary(simmdssite)$sites[,2]),site_mds3=as.numeric(summary(simmdssite)$sites[,3]),site_mds4=as.numeric(summary(simmdssite)$sites[,4]),site_mds5=as.numeric(summary(simmdssite)$sites[,5]),site_mds6=as.numeric(summary(simmdssite)$sites[,6]))
head(site_mds)

