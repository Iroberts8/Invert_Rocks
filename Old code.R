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

# Drop levels and re-assign rownames to subsetted data frames:
tidy.df<-function(df){
  df<-droplevels(df)
  rownames(df)<-1:length(df[,1])
  return(df)
}

#Load data files ----

Araneae <- read.csv("Data/Araneae.csv",header=T)
head(Araneae)
Blattodea <- read.csv("Data/Blattodea.csv",header=T)
head(Blattodea);dim(Blattodea)
Coleoptera <- read.csv("Data/Coleoptera.csv",header=T)
head(Coleoptera)
Formicidae <- read.csv("Data/Formicidae.csv",header=T)
head(Formicidae)
Morphospecies <- read.csv("Data/Morphospecies.csv",header=T)
head(Morphospecies)
Orthoptera <- read.csv("Data/Orthoptera.csv",header=T)
head(Orthoptera);dim(Orthoptera)
Other <- read.csv("Data/Other.csv",header=T)
head(Other)
Site <- read.csv("Data/Site_data.csv",header=T)
head(Site);dim(Site)

#Function traits, abundance and proportion zeros data
Ara_funcabun <- read.csv("Data/Ara_funcabun.csv",header=T)
head(Ara_funcabun);dim(Ara_funcabun)
Col_funcabun <- read.csv("Data/Col_funcabun.csv",header=T)
head(Col_funcabun);dim(Col_funcabun)

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

Morpho_abun<-tidy.df(Morpho_abun)

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

dev.new(width=14,height=4,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,0,5))
plot(1:4,Arainvdiv_pr$fit,ylim=c(min(Arainvdiv_pr$lci),max(Arainvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='Year',main='Araneae',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Arainvdiv_pr$lci,1:4,Arainvdiv_pr$uci,length=0.1,angle=90,code=3)
ap <- round(summary(Arainvdiv_mod1)$coefficients[3,'Pr(>|z|)'],3)
ap <- ifelse(ap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',ap,sep=''))
text(3.5,4.9,labels=ap)
text(3.5,5.05,labels=paste('Int. P=',round(summary(Arainvdiv_mod1)$coefficients[4,'Pr(>|z|)'],3),sep=''))
text(3.5,5.2,labels=paste('Treat. P=',round(summary(Arainvdiv_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''))
plot(1:4,Colinvdiv_pr$fit,ylim=c(min(Colinvdiv_pr$lci),max(Colinvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='Year',main='Coleoptera',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Colinvdiv_pr$lci,1:4,Colinvdiv_pr$uci,length=0.1,angle=90,code=3)
cp <- round(summary(Colinvdiv_mod1)$coefficients[3,'Pr(>|z|)'],3)
cp <- ifelse(cp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',cp,sep=''))
text(1.5,4.65,labels=cp)
text(1.5,4.8,labels=paste('Int. P=',round(summary(Colinvdiv_mod1)$coefficients[4,'Pr(>|z|)'],3),sep=''))
text(1.5,4.95,labels=paste('Treat. P=',round(summary(Colinvdiv_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''))
plot(1:4,Forminvdiv_pr$fit,ylim=c(min(Forminvdiv_pr$lci),max(Forminvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='Year',main='Formicidae',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Forminvdiv_pr$lci,1:4,Forminvdiv_pr$uci,length=0.1,angle=90,code=3)
fp <- round(summary(Forminvdiv_mod1)$coefficients[3,'Pr(>|z|)'],3)
fp <- ifelse(fp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',fp,sep=''))
text(3.5,4.5,labels=fp)
text(3.5,4.7,labels=paste('Int. P=',round(summary(Forminvdiv_mod1)$coefficients[4,'Pr(>|z|)'],3),sep=''))
text(3.5,4.9,labels=paste('Treat. P=',round(summary(Forminvdiv_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''))
par(xpd=NA)
legend(x=5,y=5,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Close plotting estimates for Inverse Simpsons diversity ----

#Proportion of plots with zero values ----

#Individual species (total plots /160, year /80, reserve /32 (over two years thus 16*2))

#Araneae

arazero <- Ara2
arazero <- arazero[,c(-1,-2,-3)]
arazero <- as.data.frame(t(arazero))
arazero1 <- as.data.frame(apply(arazero,1,function(x) sum(x==0)/160))
arazero1 <- rename(arazero1,c("apply(arazero, 1, function(x) sum(x == 0)/160)"="propzeros_total"))

azero16 <- Ara2[-c(1:80),]
azero16 <- azero16[,c(-1,-2,-3)]
azero16 <- as.data.frame(t(azero16))
azero16a <- as.data.frame(apply(azero16,1,function(x) sum(x==0)/80))
azero16a <- rename(azero16a,c("apply(azero16, 1, function(x) sum(x == 0)/80)"="propzeros_2016"))

azero19 <- Ara2[-c(81:160),]
azero19 <- azero19[,c(-1,-2,-3)]
azero19 <- as.data.frame(t(azero19))
azero19a <- as.data.frame(apply(azero19,1,function(x) sum(x==0)/80))
azero19a <- rename(azero19a,c("apply(azero19, 1, function(x) sum(x == 0)/80)"="propzeros_2019"))

asite <- Ara2[,c(-1,-3)]
aJE <- subset(asite, Site == "JE")
aJE <- aJE[,c(-1)]
aJE <- as.data.frame(t(aJE))
aJE <- as.data.frame(apply(aJE,1,function(x) sum(x==0)/32))
aJE <- rename(aJE,c("apply(aJE, 1, function(x) sum(x == 0)/32)"="propzeros_JE"))

aJW <- subset(asite, Site == "JW")
aJW <- aJW[,c(-1)]
aJW <- as.data.frame(t(aJW))
aJW <- as.data.frame(apply(aJW,1,function(x) sum(x==0)/32))
aJW <- rename(aJW,c("apply(aJW, 1, function(x) sum(x == 0)/32)"="propzeros_JW"))

aCR <- subset(asite, Site == "CR")
aCR <- aCR[,c(-1)]
aCR <- as.data.frame(t(aCR))
aCR <- as.data.frame(apply(aCR,1,function(x) sum(x==0)/32))
aCR <- rename(aCR,c("apply(aCR, 1, function(x) sum(x == 0)/32)"="propzeros_CR"))

aMUL <- subset(asite, Site == "MUL")
aMUL <- aMUL[,c(-1)]
aMUL <- as.data.frame(t(aMUL))
aMUL <- as.data.frame(apply(aMUL,1,function(x) sum(x==0)/32))
aMUL <- rename(aMUL,c("apply(aMUL, 1, function(x) sum(x == 0)/32)"="propzeros_MUL"))

aK <- subset(asite, Site == "K")
aK <- aK[,c(-1)]
aK <- as.data.frame(t(aK))
aK <- as.data.frame(apply(aK,1,function(x) sum(x==0)/32))
aK <- rename(aK,c("apply(aK, 1, function(x) sum(x == 0)/32)"="propzeros_K"))

#Blattodea

blazero <- Bla2
blazero <- blazero[,c(-1,-2,-3)]
blazero <- as.data.frame(t(blazero))
blazero1 <- as.data.frame(apply(blazero,1,function(x) sum(x==0)/160))
blazero1 <- rename(blazero1,c("apply(blazero, 1, function(x) sum(x == 0)/160)"="propzeros_total"))

bzero16 <- Bla2[-c(1:80),]
bzero16 <- bzero16[,c(-1,-2,-3)]
bzero16 <- as.data.frame(t(bzero16))
bzero16a <- as.data.frame(apply(bzero16,1,function(x) sum(x==0)/80))
bzero16a <- rename(bzero16a,c("apply(bzero16, 1, function(x) sum(x == 0)/80)"="propzeros_2016"))

bzero19 <- Bla2[-c(81:160),]
bzero19 <- bzero19[,c(-1,-2,-3)]
bzero19 <- as.data.frame(t(bzero19))
bzero19a <- as.data.frame(apply(bzero19,1,function(x) sum(x==0)/80))
bzero19a <- rename(bzero19a,c("apply(bzero19, 1, function(x) sum(x == 0)/80)"="propzeros_2019"))

bsite <- Bla2[,c(-1,-3)]
bJE <- subset(bsite, Site == "JE")
bJE <- bJE[,c(-1)]
bJE <- as.data.frame(t(bJE))
bJE <- as.data.frame(apply(bJE,1,function(x) sum(x==0)/32))
bJE <- rename(bJE,c("apply(bJE, 1, function(x) sum(x == 0)/32)"="propzeros_JE"))

bJW <- subset(bsite, Site == "JW")
bJW <- bJW[,c(-1)]
bJW <- as.data.frame(t(bJW))
bJW <- as.data.frame(apply(bJW,1,function(x) sum(x==0)/32))
bJW <- rename(bJW,c("apply(bJW, 1, function(x) sum(x == 0)/32)"="propzeros_JW"))

bCR <- subset(bsite, Site == "CR")
bCR <- bCR[,c(-1)]
bCR <- as.data.frame(t(bCR))
bCR <- as.data.frame(apply(bCR,1,function(x) sum(x==0)/32))
bCR <- rename(bCR,c("apply(bCR, 1, function(x) sum(x == 0)/32)"="propzeros_CR"))

bMUL <- subset(bsite, Site == "MUL")
bMUL <- bMUL[,c(-1)]
bMUL <- as.data.frame(t(bMUL))
bMUL <- as.data.frame(apply(bMUL,1,function(x) sum(x==0)/32))
bMUL <- rename(bMUL,c("apply(bMUL, 1, function(x) sum(x == 0)/32)"="propzeros_MUL"))

bK <- subset(bsite, Site == "K")
bK <- bK[,c(-1)]
bK <- as.data.frame(t(bK))
bK <- as.data.frame(apply(bK,1,function(x) sum(x==0)/32))
bK <- rename(bK,c("apply(bK, 1, function(x) sum(x == 0)/32)"="propzeros_K"))

#Coleoptera

colzero <- Col2
colzero <- colzero[,c(-1,-2,-3)]
colzero <- as.data.frame(t(colzero))
colzero1 <- as.data.frame(apply(colzero,1,function(x) sum(x==0)/160))
colzero1 <- rename(colzero1,c("apply(colzero, 1, function(x) sum(x == 0)/160)"="propzeros_total"))

czero16 <- Col2[-c(1:80),]
czero16 <- czero16[,c(-1,-2,-3)]
czero16 <- as.data.frame(t(czero16))
czero16a <- as.data.frame(apply(czero16,1,function(x) sum(x==0)/80))
czero16a <- rename(czero16a,c("apply(czero16, 1, function(x) sum(x == 0)/80)"="propzeros_2016"))

czero19 <- Col2[-c(81:160),]
czero19 <- czero19[,c(-1,-2,-3)]
czero19 <- as.data.frame(t(czero19))
czero19a <- as.data.frame(apply(czero19,1,function(x) sum(x==0)/80))
czero19a <- rename(czero19a,c("apply(czero19, 1, function(x) sum(x == 0)/80)"="propzeros_2019"))

csite <- Col2[,c(-1,-3)]
cJE <- subset(csite, Site == "JE")
cJE <- cJE[,c(-1)]
cJE <- as.data.frame(t(cJE))
cJE <- as.data.frame(apply(cJE,1,function(x) sum(x==0)/32))
cJE <- rename(cJE,c("apply(cJE, 1, function(x) sum(x == 0)/32)"="propzeros_JE"))

cJW <- subset(csite, Site == "JW")
cJW <- cJW[,c(-1)]
cJW <- as.data.frame(t(cJW))
cJW <- as.data.frame(apply(cJW,1,function(x) sum(x==0)/32))
cJW <- rename(cJW,c("apply(cJW, 1, function(x) sum(x == 0)/32)"="propzeros_JW"))

cCR <- subset(csite, Site == "CR")
cCR <- cCR[,c(-1)]
cCR <- as.data.frame(t(cCR))
cCR <- as.data.frame(apply(cCR,1,function(x) sum(x==0)/32))
cCR <- rename(cCR,c("apply(cCR, 1, function(x) sum(x == 0)/32)"="propzeros_CR"))

cMUL <- subset(csite, Site == "MUL")
cMUL <- cMUL[,c(-1)]
cMUL <- as.data.frame(t(cMUL))
cMUL <- as.data.frame(apply(cMUL,1,function(x) sum(x==0)/32))
cMUL <- rename(cMUL,c("apply(cMUL, 1, function(x) sum(x == 0)/32)"="propzeros_MUL"))

cK <- subset(csite, Site == "K")
cK <- cK[,c(-1)]
cK <- as.data.frame(t(cK))
cK <- as.data.frame(apply(cK,1,function(x) sum(x==0)/32))
cK <- rename(cK,c("apply(cK, 1, function(x) sum(x == 0)/32)"="propzeros_K"))

#Formicidae

formzero <- Form2
formzero <- formzero[,c(-1,-2,-3)]
formzero <- as.data.frame(t(formzero))
formzero1 <- as.data.frame(apply(formzero,1,function(x) sum(x==0)/160))
formzero1 <- rename(formzero1,c("apply(formzero, 1, function(x) sum(x == 0)/160)"="propzeros_total"))

fzero16 <- Form2[-c(1:80),]
fzero16 <- fzero16[,c(-1,-2,-3)]
fzero16 <- as.data.frame(t(fzero16))
fzero16a <- as.data.frame(apply(fzero16,1,function(x) sum(x==0)/80))
fzero16a <- rename(fzero16a,c("apply(fzero16, 1, function(x) sum(x == 0)/80)"="propzeros_2016"))

fzero19 <- Form2[-c(81:160),]
fzero19 <- fzero19[,c(-1,-2,-3)]
fzero19 <- as.data.frame(t(fzero19))
fzero19a <- as.data.frame(apply(fzero19,1,function(x) sum(x==0)/80))
fzero19a <- rename(fzero19a,c("apply(fzero19, 1, function(x) sum(x == 0)/80)"="propzeros_2019"))

fsite <- Form2[,c(-1,-3)]
fJE <- subset(fsite, Site == "JE")
fJE <- fJE[,c(-1)]
fJE <- as.data.frame(t(fJE))
fJE <- as.data.frame(apply(fJE,1,function(x) sum(x==0)/32))
fJE <- rename(fJE,c("apply(fJE, 1, function(x) sum(x == 0)/32)"="propzeros_JE"))

fJW <- subset(fsite, Site == "JW")
fJW <- fJW[,c(-1)]
fJW <- as.data.frame(t(fJW))
fJW <- as.data.frame(apply(fJW,1,function(x) sum(x==0)/32))
fJW <- rename(fJW,c("apply(fJW, 1, function(x) sum(x == 0)/32)"="propzeros_JW"))

fCR <- subset(fsite, Site == "CR")
fCR <- fCR[,c(-1)]
fCR <- as.data.frame(t(fCR))
fCR <- as.data.frame(apply(fCR,1,function(x) sum(x==0)/32))
fCR <- rename(fCR,c("apply(fCR, 1, function(x) sum(x == 0)/32)"="propzeros_CR"))

fMUL <- subset(fsite, Site == "MUL")
fMUL <- fMUL[,c(-1)]
fMUL <- as.data.frame(t(fMUL))
fMUL <- as.data.frame(apply(fMUL,1,function(x) sum(x==0)/32))
fMUL <- rename(fMUL,c("apply(fMUL, 1, function(x) sum(x == 0)/32)"="propzeros_MUL"))

fK <- subset(fsite, Site == "K")
fK <- fK[,c(-1)]
fK <- as.data.frame(t(fK))
fK <- as.data.frame(apply(fK,1,function(x) sum(x==0)/32))
fK <- rename(fK,c("apply(fK, 1, function(x) sum(x == 0)/32)"="propzeros_K"))

#Orthoptera

ortzero <- Ort2
ortzero <- ortzero[,c(-1,-2,-3)]
ortzero <- as.data.frame(t(ortzero))
ortzero1 <- as.data.frame(apply(ortzero,1,function(x) sum(x==0)/160))
ortzero1 <- rename(ortzero1,c("apply(ortzero, 1, function(x) sum(x == 0)/160)"="propzeros_total"))

orzero16 <- Ort2[-c(1:80),]
orzero16 <- orzero16[,c(-1,-2,-3)]
orzero16 <- as.data.frame(t(orzero16))
orzero16a <- as.data.frame(apply(orzero16,1,function(x) sum(x==0)/80))
orzero16a <- rename(orzero16a,c("apply(orzero16, 1, function(x) sum(x == 0)/80)"="propzeros_2016"))

orzero19 <- Ort2[-c(81:160),]
orzero19 <- orzero19[,c(-1,-2,-3)]
orzero19 <- as.data.frame(t(orzero19))
orzero19a <- as.data.frame(apply(orzero19,1,function(x) sum(x==0)/80))
orzero19a <- rename(orzero19a,c("apply(orzero19, 1, function(x) sum(x == 0)/80)"="propzeros_2019"))

orsite <- Ort2[,c(-1,-3)]
orJE <- subset(orsite, Site == "JE")
orJE <- orJE[,c(-1)]
orJE <- as.data.frame(t(orJE))
orJE <- as.data.frame(apply(orJE,1,function(x) sum(x==0)/32))
orJE <- rename(orJE,c("apply(orJE, 1, function(x) sum(x == 0)/32)"="propzeros_JE"))

orJW <- subset(orsite, Site == "JW")
orJW <- orJW[,c(-1)]
orJW <- as.data.frame(t(orJW))
orJW <- as.data.frame(apply(orJW,1,function(x) sum(x==0)/32))
orJW <- rename(orJW,c("apply(orJW, 1, function(x) sum(x == 0)/32)"="propzeros_JW"))

orCR <- subset(orsite, Site == "CR")
orCR <- orCR[,c(-1)]
orCR <- as.data.frame(t(orCR))
orCR <- as.data.frame(apply(orCR,1,function(x) sum(x==0)/32))
orCR <- rename(orCR,c("apply(orCR, 1, function(x) sum(x == 0)/32)"="propzeros_CR"))

orMUL <- subset(orsite, Site == "MUL")
orMUL <- orMUL[,c(-1)]
orMUL <- as.data.frame(t(orMUL))
orMUL <- as.data.frame(apply(orMUL,1,function(x) sum(x==0)/32))
orMUL <- rename(orMUL,c("apply(orMUL, 1, function(x) sum(x == 0)/32)"="propzeros_MUL"))

orK <- subset(orsite, Site == "K")
orK <- orK[,c(-1)]
orK <- as.data.frame(t(orK))
orK <- as.data.frame(apply(orK,1,function(x) sum(x==0)/32))
orK <- rename(orK,c("apply(orK, 1, function(x) sum(x == 0)/32)"="propzeros_K"))

#Other

otherzero <- Other2
otherzero <- otherzero[,c(-1,-2,-3)]
otherzero <- as.data.frame(t(otherzero))
otherzero1 <- as.data.frame(apply(otherzero,1,function(x) sum(x==0)/160))
otherzero1 <- rename(otherzero1,c("apply(otherzero, 1, function(x) sum(x == 0)/160)"="propzeros_total"))

otzero16 <- Other2[-c(1:80),]
otzero16 <- otzero16[,c(-1,-2,-3)]
otzero16 <- as.data.frame(t(otzero16))
otzero16a <- as.data.frame(apply(otzero16,1,function(x) sum(x==0)/80))
otzero16a <- rename(otzero16a,c("apply(otzero16, 1, function(x) sum(x == 0)/80)"="propzeros_2016"))

otzero19 <- Other2[-c(81:160),]
otzero19 <- otzero19[,c(-1,-2,-3)]
otzero19 <- as.data.frame(t(otzero19))
otzero19a <- as.data.frame(apply(otzero19,1,function(x) sum(x==0)/80))
otzero19a <- rename(otzero19a,c("apply(otzero19, 1, function(x) sum(x == 0)/80)"="propzeros_2019"))

otsite <- Other2[,c(-1,-3)]
otJE <- subset(otsite, Site == "JE")
otJE <- otJE[,c(-1)]
otJE <- as.data.frame(t(otJE))
otJE <- as.data.frame(apply(otJE,1,function(x) sum(x==0)/32))
otJE <- rename(otJE,c("apply(otJE, 1, function(x) sum(x == 0)/32)"="propzeros_JE"))

otJW <- subset(otsite, Site == "JW")
otJW <- otJW[,c(-1)]
otJW <- as.data.frame(t(otJW))
otJW <- as.data.frame(apply(otJW,1,function(x) sum(x==0)/32))
otJW <- rename(otJW,c("apply(otJW, 1, function(x) sum(x == 0)/32)"="propzeros_JW"))

otCR <- subset(otsite, Site == "CR")
otCR <- otCR[,c(-1)]
otCR <- as.data.frame(t(otCR))
otCR <- as.data.frame(apply(otCR,1,function(x) sum(x==0)/32))
otCR <- rename(otCR,c("apply(otCR, 1, function(x) sum(x == 0)/32)"="propzeros_CR"))

otMUL <- subset(otsite, Site == "MUL")
otMUL <- otMUL[,c(-1)]
otMUL <- as.data.frame(t(otMUL))
otMUL <- as.data.frame(apply(otMUL,1,function(x) sum(x==0)/32))
otMUL <- rename(otMUL,c("apply(otMUL, 1, function(x) sum(x == 0)/32)"="propzeros_MUL"))

otK <- subset(otsite, Site == "K")
otK <- otK[,c(-1)]
otK <- as.data.frame(t(otK))
otK <- as.data.frame(apply(otK,1,function(x) sum(x==0)/32))
otK <- rename(otK,c("apply(otK, 1, function(x) sum(x == 0)/32)"="propzeros_K"))

#Merge data

zeroplot <- rbind(arazero1,blazero1,colzero1,formzero1,ortzero1,otherzero1)
zeroyear16 <- rbind(azero16a,bzero16a,czero16a,fzero16a,orzero16a,otzero16a)
zeroyear19 <- rbind(azero19a,bzero19a,czero19a,fzero19a,orzero19a,otzero19a)
JE <- rbind(aJE,bJE,cJE,fJE,orJE,otJE)
JW <- rbind(aJW,bJW,cJW,fJW,orJW,otJW)
CR <- rbind(aCR,bCR,cCR,fCR,orCR,otCR)
MUL <- rbind(aMUL,bMUL,cMUL,fMUL,orMUL,otMUL)
K <- rbind(aK,bK,cK,fK,orK,otK)

morpho_abun2 <- cbind(Morpho_abun,zeroplot,zeroyear16,zeroyear19,JE,JW,CR,MUL,K)

write.csv(morpho_abun2,file = 'Morpho_abun2.csv')

head(morpho_abun2,2);dim(morpho_abun2)
plot(morpho_abun2$Abundance[morpho_abun2$Abundance<4000],morpho_abun2$propzeros_total[morpho_abun2$Abundance<4000],ylab='')
cor.test(morpho_abun2$Abundance[morpho_abun2$Abundance<4000],morpho_abun2$propzeros_total[morpho_abun2$Abundance<4000])
table(morpho_abun2$propzeros_total>0.8)

m_abun <- morpho_abun2[which(morpho_abun2$propzeros_total<0.8),]

hist(morpho_abun2$Abundance)
hist(morpho_abun2$Abundance[morpho_abun2$Abundance<2000])
hist(morpho_abun2$Abundance[morpho_abun2$Abundance<500])
hist(morpho_abun2$Abundance[morpho_abun2$Abundance<100])
hist(morpho_abun2$Abundance[morpho_abun2$Abundance<20])

rarity <- ifelse(morpho_abun2$Abundance<20,'rare','common')

#Close proportion of zeroes ----

# Remove species with less than 80 samples (by yr/pitfall) or 16 samples (by site/reserve) - move to individual species analysis:

mds_abun <- subset(Morpho_abun,Abundance>80)
head(mds_abun,3)
mds_abunyr<-mds_abun[,c(-1,-2,-3,-4,-5,-8,-9,-10,-11,-12)]
mds_abunsite<-mds_abun[,c(-1,-2,-3,-4,-5,-6,-7)]

mds_abun2 <- subset(Morpho_abun,Abundance>16)
head(mds_abun2)
mds_abun2yr<-mds_abun2[,c(-1,-2,-3,-4,-5,-8,-9,-10,-11,-12)]
mds_abun2site<-mds_abun2[,c(-1,-2,-3,-4,-5,-6,-7)]

#MDS attempt ----

Morpho_abun$Abundance <- as.numeric(Morpho_abun$Abundance)
Morpho_abun$`2016` <- as.numeric(Morpho_abun$`2016`)
Morpho_abun$`2019` <- as.numeric(Morpho_abun$`2019`)
Morpho_abun$CR <- as.numeric(Morpho_abun$CR)
Morpho_abun$JE <- as.numeric(Morpho_abun$JE)
Morpho_abun$JW <- as.numeric(Morpho_abun$JW)
Morpho_abun$K <- as.numeric(Morpho_abun$K)
Morpho_abun$MUL <- as.numeric(Morpho_abun$MUL)




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

#Close mds ----

#Principal components analysis ----

# ?princomp ----
Ara2[1:4,1:10];dim(Ara2)
range(Ara2[,4:ncol(Ara2)])
a2 <- as.matrix(Ara2[,4:ncol(Ara2)])
hist(a2[which(a2<10 & a2>0)])
arapca <- princomp(~ ., data = Ara2[,4:ncol(Ara2)], cor = TRUE)
summary(arapca)
arapca$loadings
#Ara2$arapca<-arapca$scores[,1]
#Ara2$arapca2<-arapca$scores[,2]*-1
head(arapca$scores)
head(Ara2)
autoplot(arapca, data=Ara2, colour = 'Treatment')

Bla2[1:4,1:10];dim(Bla2)
range(Bla2[,4:ncol(Bla2)])
blapca <- princomp(~ ., data = Bla2[,4:ncol(Bla2)], cor = TRUE)
summary(blapca)
blapca$loadings
Bla2$blapca<-blapca$scores[,1]
Bla2$blapca2<-blapca$scores[,2]*-1
head(blapca$scores)
head(Bla2)
autoplot(blapca, data=Bla2, colour = 'Treatment')

Col2[1:4,1:10];dim(Col2)
range(Col2[,4:ncol(Col2)])
colpca <- princomp(~ ., data = Col2[,4:ncol(Col2)], cor = TRUE)
summary(colpca)
colpca$loadings
Col2$colpca<-colpca$scores[,1]
Col2$colpca2<-colpca$scores[,2]*-1
head(colpca$scores)
head(Col2)
autoplot(colpca, data=Col2, colour = 'Treatment')

Form2[1:4,1:10];dim(Form2)
range(Form2[,4:ncol(Form2)])
formpca <- princomp(~ ., data = Form2[,4:ncol(Form2)], cor = TRUE)
summary(formpca)
formpca$loadings
Form2$formpca<-formpca$scores[,1]
Form2$formpca2<-formpca$scores[,2]*-1
head(formpca$scores)
head(Form2)
autoplot(formpca, data=Form2, colour = 'Treatment')

Ort2[1:4,1:10];dim(Ort2)
range(Ort2[,4:ncol(Ort2)])
ortpca <- princomp(~ ., data = Ort2[,4:ncol(Ort2)], cor = TRUE)
summary(ortpca)
ortpca$loadings
Ort2$ortpca<-ortpca$scores[,1]
Ort2$ortpca2<-ortpca$scores[,2]*-1
head(ortpca$scores)
head(Ort2)
autoplot(ortpca, data=Ort2, colour = 'Treatment')

Other2[1:4,1:10];dim(Other2)
range(Other2[,4:ncol(Other2)])
otherpca <- princomp(~ ., data = Other2[,4:ncol(Other2)], cor = TRUE)
summary(otherpca)
otherpca$loadings
Other2$otherpca<-otherpca$scores[,1]
Other2$ortpca2<-otherpca$scores[,2]*-1
head(otherpca$scores)
head(Other2)
autoplot(otherpca, data=Other2, colour = 'Treatment')

#Close princomp ----

# ?prcomp ----

arapca2 <- prcomp(~ ., data = Ara2[,4:ncol(Ara2)])
str(summary(arapca2))
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(4,4,1,1))
plot(1:10,summary(arapca2)$importance[2,][1:10],pch=20,main='Araneae',xlab='Component',ylab='Proportion variance explained')
lines(1:10,summary(arapca2)$importance[2,][1:10])

blapca2 <- prcomp(~ ., data = Bla2[,4:ncol(Bla2)])
str(summary(blapca2))
plot(1:10,summary(blapca2)$importance[2,][1:10],pch=20,main='Blattodea',xlab='Component',ylab='Proportion variance explained')
lines(1:10,summary(blapca2)$importance[2,][1:10])

colpca2 <- prcomp(~ ., data = Col2[,4:ncol(Col2)])
str(summary(colpca2))
plot(1:10,summary(colpca2)$importance[2,][1:10],pch=20,main='Coleoptera',xlab='Component',ylab='Proportion variance explained')
lines(1:10,summary(colpca2)$importance[2,][1:10])

formpca2 <- prcomp(~ ., data = Form2[,4:ncol(Form2)])
str(summary(formpca2))
plot(1:10,summary(formpca2)$importance[2,][1:10],pch=20,main='Formicidae',xlab='Component',ylab='Proportion variance explained')
lines(1:10,summary(formpca2)$importance[2,][1:10])

ortpca2 <- prcomp(~ ., data = Ort2[,4:ncol(Ort2)])
str(summary(ortpca2))
plot(1:10,summary(ortpca2)$importance[2,][1:10],pch=20,main='Orthoptera',xlab='Component',ylab='Proportion variance explained')
lines(1:10,summary(ortpca2)$importance[2,][1:10])

otherpca2 <- prcomp(~ ., data = Other2[,4:ncol(Other2)])
str(summary(otherpca2))
plot(1:10,summary(otherpca2)$importance[2,][1:10],pch=20,main='Other',xlab='Component',ylab='Proportion variance explained')
lines(1:10,summary(otherpca2)$importance[2,][1:10])

#Close prcomp ----

#Biplots ----

dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(5,5,4,1),mgp=c(2.5,1,0),oma=c(0,0,0,6))
biplot(prcomp(~ ., data = Ara2[,4:ncol(Ara2)]),main='Araneae')
biplot(prcomp(~ ., data = Bla2[,4:ncol(Bla2)]),main='Blattodea')
biplot(prcomp(~ ., data = Col2[,4:ncol(Col2)]),main='Coleoptera')
biplot(prcomp(~ ., data = Form2[,4:ncol(Form2)]),main='Formicidae')
biplot(prcomp(~ ., data = Ort2[,4:ncol(Ort2)]),main='Orthoptera')
biplot(prcomp(~ ., data = Other2[,4:ncol(Other2)]),main='Other')

#Graphing prcomp ----

summary(arapca2)$importance[2,]
autoplot(arapca2, data=Ara2, colour = 'Treatment')
head(arapca2$x[,1:10])
str(arapca2)

summary(blapca2)$importance[2,]
autoplot(blapca2, data=Bla2, colour = 'Treatment')
head(blapca2$x[,1:10])
str(blapca2)

summary(colpca2)$importance[2,]
autoplot(colpca2, data=Col2, colour = 'Treatment')
head(colpca2$x[,1:10])
str(colpca2)

summary(formpca2)$importance[2,]
autoplot(formpca2, data=Form2, colour = 'Treatment')
head(formpca2$x[,1:10])
str(formpca2)

summary(ortpca2)$importance[2,]
autoplot(ortpca2, data=Ort2, colour = 'Treatment')
head(ortpca2$x[,1:10])
str(ortpca2)

summary(otherpca2)$importance[2,]
autoplot(otherpca2, data=Other2, colour = 'Treatment')
head(otherpca2$x[,1:10])
str(otherpca2)

dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,0,6))
plot(arapca2$x[,1],arapca2$x[,2],col=as.numeric(as.factor(Ara2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Ara2$Year))],xlim=c(-15,5),ylim=c(-2,8),xlab=paste("PC 1 (",summary(arapca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(arapca2)$importance[2,][2]*100,"%)",sep=""),main="Araneae PCA")
plot(blapca2$x[,1],blapca2$x[,2],col=as.numeric(as.factor(Bla2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Bla2$Year))],xlim=c(-5,10),ylim=c(-2,10),xlab=paste("PC 1 (",summary(blapca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(blapca2)$importance[2,][2]*100,"%)",sep=""),main="Blattodea PCA")
plot(colpca2$x[,1],colpca2$x[,2],col=as.numeric(as.factor(Col2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Col2$Year))],xlim=c(-50,10),ylim=c(-2,10),xlab=paste("PC 1 (",summary(colpca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(colpca2)$importance[2,][2]*100,"%)",sep=""),main="Coleoptera PCA")
par(xpd=NA)
legend(x=15,y=10,legend = c("Control","Rock","2016","2019"),pch=c(rep(16,3),17),col=c(1,2,1,1))
par(xpd=F)
plot(formpca2$x[,1],formpca2$x[,2],col=as.numeric(as.factor(Form2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Form2$Year))],xlim=c(-400,100),ylim=c(-50,100),xlab=paste("PC 1 (",summary(formpca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(formpca2)$importance[2,][2]*100,"%)",sep=""),main="Formicidae PCA")
plot(ortpca2$x[,1],ortpca2$x[,2],col=as.numeric(as.factor(Ort2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Ort2$Year))],xlim=c(-15,5),ylim=c(-5,20),xlab=paste("PC 1 (",summary(ortpca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(ortpca2)$importance[2,][2]*100,"%)",sep=""),main="Orthoptera PCA")
plot(otherpca2$x[,1],otherpca2$x[,2],col=as.numeric(as.factor(Other2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Other2$Year))],xlim=c(-15,5),ylim=c(-5,20),xlab=paste("PC 1 (",summary(otherpca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(otherpca2)$importance[2,][2]*100,"%)",sep=""),main="Other PCA")

dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,0,6))
plot(arapca2$x[,1],arapca2$x[,2],col=as.numeric(as.factor(Ara2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Ara2$Year))],xlim=NULL,ylim=NULL,xlab=paste("PC 1 (",summary(arapca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(arapca2)$importance[2,][2]*100,"%)",sep=""),main="Araneae PCA")
plot(blapca2$x[,1],blapca2$x[,2],col=as.numeric(as.factor(Bla2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Bla2$Year))],xlim=NULL,ylim=NULL,xlab=paste("PC 1 (",summary(blapca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(blapca2)$importance[2,][2]*100,"%)",sep=""),main="Blattodea PCA")
plot(colpca2$x[,1],colpca2$x[,2],col=as.numeric(as.factor(Col2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Col2$Year))],xlim=NULL,ylim=NULL,xlab=paste("PC 1 (",summary(colpca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(colpca2)$importance[2,][2]*100,"%)",sep=""),main="Coleoptera PCA")
par(xpd=NA)
legend(x=15,y=10,legend = c("Control","Rock","2016","2019"),pch=c(rep(16,3),17),col=c(1,2,1,1))
par(xpd=F)
plot(formpca2$x[,1],formpca2$x[,2],col=as.numeric(as.factor(Form2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Form2$Year))],xlim=NULL,ylim=NULL,xlab=paste("PC 1 (",summary(formpca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(formpca2)$importance[2,][2]*100,"%)",sep=""),main="Formicidae PCA")
plot(ortpca2$x[,1],ortpca2$x[,2],col=as.numeric(as.factor(Ort2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Ort2$Year))],xlim=NULL,ylim=NULL,xlab=paste("PC 1 (",summary(ortpca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(ortpca2)$importance[2,][2]*100,"%)",sep=""),main="Orthoptera PCA")
plot(otherpca2$x[,1],otherpca2$x[,2],col=as.numeric(as.factor(Other2$Treatment)),pch=c(16,17)[as.numeric(as.factor(Other2$Year))],xlim=NULL,ylim=NULL,xlab=paste("PC 1 (",summary(otherpca2)$importance[2,][1]*100,"%)",sep=""),ylab=paste("PC 1 (",summary(otherpca2)$importance[2,][2]*100,"%)",sep=""),main="Other PCA")

#Close graphing of prcomp ----

#Model PCA ----

Ara3 <- Araneae[,1:5]
Ara3$PC1 <- arapca2$x[,1]
Ara3$PC2 <- arapca2$x[,2]
head(Ara3)
Ara3$Yr <- Ara3$Year-min(Ara3$Year)
arapca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Ara3)
summary(arapca2_mod1)
arapca2_mod2 <- lmer(PC2 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Ara3)
summary(arapca2_mod1)

Bla3 <- Blattodea[,1:5]
Bla3$PC1 <- blapca2$x[,1]
Bla3$PC2 <- blapca2$x[,2]
Bla3$PC3 <- blapca2$x[,3]
head(Bla3)
Bla3$Yr <- Bla3$Year-min(Bla3$Year)
blapca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Bla3)
summary(blapca2_mod1)
blapca2_mod2 <- lmer(PC2 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Bla3)
summary(blapca2_mod2)
blapca2_mod3 <- lmer(PC3 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Bla3)
summary(blapca2_mod3)

Col3 <- Coleoptera[,1:5]
Col3$PC1 <- colpca2$x[,1]
Col3$PC2 <- colpca2$x[,2]
Col3$PC3 <- colpca2$x[,3]
head(Col3)
Col3$Yr <- Col3$Year-min(Col3$Year)
colpca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Col3)
summary(colpca2_mod1)
colpca2_mod2 <- lmer(PC2 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Col3)
summary(colpca2_mod2)
colpca2_mod3 <- lmer(PC3 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Col3)
summary(colpca2_mod3)

Form3 <- Formicidae[,1:5]
Form3$PC1 <- formpca2$x[,1]
Form3$PC2 <- formpca2$x[,2]
head(Form3)
Form3$Yr <- Form3$Year-min(Form3$Year)
formpca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Form3)
summary(formpca2_mod1)
formpca2_mod2 <- lmer(PC2 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Form3)
summary(formpca2_mod2)

Ort3 <- Orthoptera[,1:5]
Ort3$PC1 <- ortpca2$x[,1]
Ort3$PC2 <- ortpca2$x[,2]
Ort3$PC3 <- ortpca2$x[,3]
head(Ort3)
Ort3$Yr <- Ort3$Year-min(Ort3$Year)
ortpca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Ort3)
summary(ortpca2_mod1)
ortpca2_mod2 <- lmer(PC2 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Ort3)
summary(ortpca2_mod2)
ortpca2_mod3 <- lmer(PC3 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Ort3)
summary(ortpca2_mod3)

Other3 <- Other[,1:5]
Other3$PC1 <- otherpca2$x[,1]
Other3$PC2 <- colpca2$x[,2]
Other3$PC3 <- colpca2$x[,3]
head(Other3)
Other3$Yr <- Other3$Year-min(Other3$Year)
otherpca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Other3)
summary(otherpca2_mod1)
otherpca2_mod2 <- lmer(PC2 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Other3)
summary(otherpca2_mod2)
otherpca3_mod3 <- lmer(PC3 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Other3)
summary(otherpca3_mod3)

#Close modelling ----

#Graphing PCA models ----

Ara3$Treatment <- as.factor(Ara3$Treatment)

arapca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Ara3)
summary(arapca2_mod1)$coefficients
pca_nd <- data.frame(Yr=c(0,0,3,3),Treatment=rep(levels(Ara3$Treatment),2))
arapca_pr <- predictSE(arapca2_mod1,newdata=pca_nd,se.fit=T,type='response')
arapca_pr<-data.frame(pca_nd, fit=arapca_pr$fit, se=arapca_pr$se.fit)
arapca_pr$lci<-arapca_pr$fit-(1.96*arapca_pr$se)
arapca_pr$uci<-arapca_pr$fit+(1.96*arapca_pr$se)
arapca_pr
dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,0,6))
plot(1:4,arapca_pr$fit,ylim=c(min(arapca_pr$lci),max(arapca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Araneae PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,arapca_pr$lci,1:4,arapca_pr$uci,length=0.1,angle=90,code=3)
text(3,-7,labels=paste('Int. P=',round(summary(arapca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))

blapca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Bla3)
summary(blapca2_mod1)$coefficients
blapca_pr <- predictSE(blapca2_mod1,newdata=pca_nd,se.fit=T,type='response')
blapca_pr<-data.frame(pca_nd, fit=blapca_pr$fit, se=blapca_pr$se.fit)
blapca_pr$lci<-blapca_pr$fit-(1.96*blapca_pr$se)
blapca_pr$uci<-blapca_pr$fit+(1.96*blapca_pr$se)
blapca_pr
plot(1:4,blapca_pr$fit,ylim=c(min(blapca_pr$lci),max(blapca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Blattodea PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,blapca_pr$lci,1:4,blapca_pr$uci,length=0.1,angle=90,code=3)
text(3,-7,labels=paste('Int. P=',round(summary(blapca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))

colpca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Col3)
summary(colpca2_mod1)
colpca_pr <- predictSE(colpca2_mod1,newdata=pca_nd,se.fit=T,type='response')
colpca_pr<-data.frame(pca_nd, fit=colpca_pr$fit, se=colpca_pr$se.fit)
colpca_pr$lci<-colpca_pr$fit-(1.96*colpca_pr$se)
colpca_pr$uci<-colpca_pr$fit+(1.96*colpca_pr$se)
colpca_pr
plot(1:4,colpca_pr$fit,ylim=c(min(colpca_pr$lci),max(colpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Coleoptera PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,colpca_pr$lci,1:4,colpca_pr$uci,length=0.1,angle=90,code=3)
text(3,-7,labels=paste('Int. P=',round(summary(colpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))

formpca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Form3)
summary(formpca2_mod1)
formpca_pr <- predictSE(formpca2_mod1,newdata=pca_nd,se.fit=T,type='response')
formpca_pr<-data.frame(pca_nd, fit=formpca_pr$fit, se=formpca_pr$se.fit)
formpca_pr$lci<-formpca_pr$fit-(1.96*formpca_pr$se)
formpca_pr$uci<-formpca_pr$fit+(1.96*formpca_pr$se)
formpca_pr
plot(1:4,formpca_pr$fit,ylim=c(min(formpca_pr$lci),max(formpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Formicidae PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,formpca_pr$lci,1:4,formpca_pr$uci,length=0.1,angle=90,code=3)
text(3,-7,labels=paste('Int. P=',round(summary(formpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))

ortpca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Ort3)
summary(ortpca2_mod1)
ortpca_pr <- predictSE(ortpca2_mod1,newdata=pca_nd,se.fit=T,type='response')
ortpca_pr<-data.frame(pca_nd, fit=ortpca_pr$fit, se=ortpca_pr$se.fit)
ortpca_pr$lci<-ortpca_pr$fit-(1.96*ortpca_pr$se)
ortpca_pr$uci<-ortpca_pr$fit+(1.96*ortpca_pr$se)
ortpca_pr
plot(1:4,ortpca_pr$fit,ylim=c(min(ortpca_pr$lci),max(ortpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Orthoptera PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,ortpca_pr$lci,1:4,ortpca_pr$uci,length=0.1,angle=90,code=3)
text(3,-7,labels=paste('Int. P=',round(summary(ortpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))

otherpca2_mod1 <- lmer(PC1 ~ Treatment+Yr+Treatment:Yr+(1|Site/Plot),data=Other3)
summary(otherpca2_mod1)
otherpca_pr <- predictSE(otherpca2_mod1,newdata=pca_nd,se.fit=T,type='response')
otherpca_pr<-data.frame(pca_nd, fit=otherpca_pr$fit, se=otherpca_pr$se.fit)
otherpca_pr$lci<-otherpca_pr$fit-(1.96*otherpca_pr$se)
otherpca_pr$uci<-otherpca_pr$fit+(1.96*otherpca_pr$se)
otherpca_pr
plot(1:4,otherpca_pr$fit,ylim=c(min(otherpca_pr$lci),max(otherpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Other PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,otherpca_pr$lci,1:4,otherpca_pr$uci,length=0.1,angle=90,code=3)
text(3,-7,labels=paste('Int. P=',round(summary(otherpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))

dev.new(width=12,height=8,dpi=100,pointsize=16,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(5,5,1,1),mgp=c(2.5,1,0),oma=c(0,0,0,10))
plot(1:4,arapca_pr$fit,ylim=c(min(arapca_pr$lci),max(arapca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Araneae PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,arapca_pr$lci,1:4,arapca_pr$uci,length=0.1,angle=90,code=3)
arap <- round(summary(arapca2_mod1)$coefficients[3,'Pr(>|t|)'],3)
arap <- ifelse(arap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',arap,sep=''))
text(3.5,-7,labels=arap)
text(3.5,-5,labels=paste('Int. P=',round(summary(arapca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))
plot(1:4,blapca_pr$fit,ylim=c(min(blapca_pr$lci),max(blapca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Blattodea PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,blapca_pr$lci,1:4,blapca_pr$uci,length=0.1,angle=90,code=3)
blap <- round(summary(blapca2_mod1)$coefficients[3,'Pr(>|t|)'],6)
blap <- ifelse(blap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',blap,sep=''))
text(1.75,-0.75,labels=blap)
text(1.75,-0.5,labels=paste('Int. P=',round(summary(blapca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))
plot(1:4,colpca_pr$fit,ylim=c(min(colpca_pr$lci),max(colpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Coleoptera PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,colpca_pr$lci,1:4,colpca_pr$uci,length=0.1,angle=90,code=3)
colp <- round(summary(colpca2_mod1)$coefficients[3,'Pr(>|t|)'],6)
colp <- ifelse(colp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',colp,sep=''))
text(3.5,-7.5,labels=colp)
text(3.5,-5,labels=paste('Int. P=',round(summary(colpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))
par(xpd=NA)
legend(x=5,y=5,legend = c("Control","Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)
plot(1:4,formpca_pr$fit,ylim=c(min(formpca_pr$lci),max(formpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Formicidae PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,formpca_pr$lci,1:4,formpca_pr$uci,length=0.1,angle=90,code=3)
formp <- round(summary(formpca2_mod1)$coefficients[3,'Pr(>|t|)'],3)
formp <- ifelse(formp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',formp,sep=''))
text(1.75,-80,labels=formp)
text(1.75,-60,labels=paste('Int. P=',round(summary(formpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))
plot(1:4,ortpca_pr$fit,ylim=c(min(ortpca_pr$lci),max(ortpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Orthoptera PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,ortpca_pr$lci,1:4,ortpca_pr$uci,length=0.1,angle=90,code=3)
ortp <- round(summary(ortpca2_mod1)$coefficients[3,'Pr(>|t|)'],3)
ortp <- ifelse(ortp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',ortp,sep=''))
text(3.5,-1.5,labels=ortp)
text(3.5,-1,labels=paste('Int. P=',round(summary(ortpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))
plot(1:4,otherpca_pr$fit,ylim=c(min(otherpca_pr$lci),max(otherpca_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Principle component score',xlab='',main='Other PCA',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,otherpca_pr$lci,1:4,otherpca_pr$uci,length=0.1,angle=90,code=3)
othp <- round(summary(otherpca2_mod1)$coefficients[3,'Pr(>|t|)'],3)
othp <- ifelse(othp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',othp,sep=''))
text(3.5,-7,labels=othp)
text(3.5,-5,labels=paste('Int. P=',round(summary(otherpca2_mod1)$coefficients[4,'Pr(>|t|)'],3),sep=''))

#Close model graphs ----

#Functional group analysis

#Araneae----
head(Araneae[,1:8])
head(Ara2[,1:8])
head(Ara_funcabun[,1:8]);dim(Ara_funcabun)
table(colnames(Ara2)[which(colnames(Ara2)=="Actinopodidae1"):ncol(Ara2)] %in% Ara_funcabun$Morphospecies)
colnames(Ara2)[which(colnames(Ara2)=="Actinopodidae1"):ncol(Ara2)][which(!colnames(Ara2)[which(colnames(Ara2)=="Actinopodidae1"):ncol(Ara2)] %in% Ara_funcabun$Morphospecies)]
Ara_occur <- colnames(Ara2)[which(colnames(Ara2)=="Actinopodidae1"):ncol(Ara2)]
Ara_func <- Ara_funcabun$Morphospecies
Ara_occur[which(!Ara_occur %in% Ara_func)]
Ara_func[which(!Ara_func %in% Ara_occur)]

which(duplicated(Ara_occur))
which(duplicated(Ara_func))

Ara_funcabun$Retreat_type <- as.factor(Ara_funcabun$Retreat_type)
levels(Ara_funcabun$Retreat_type)
table(Ara_funcabun$Retreat_type)
Ara_funcabun$Capture_style <- as.factor(Ara_funcabun$Capture_style)
levels(Ara_funcabun$Capture_style)
table(Ara_funcabun$Capture_style)
Ara_funcabun$Retreat_Capture <- paste(Ara_funcabun$Capture_style,Ara_funcabun$Retreat_type,sep="_")
Ara_funcabun$Retreat_Capture <- as.factor(Ara_funcabun$Retreat_Capture)
levels(Ara_funcabun$Retreat_Capture)
table(Ara_funcabun$Retreat_Capture)

burrowers <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_type=="Burrow")]
Ara_sited <- Ara2[,1:6]
head(Ara_sited);dim(Ara_sited)
Ara_spd <- Ara2[,which(colnames(Ara2)=="Actinopodidae1"):ncol(Ara2)]
head(Ara_spd[,1:6])
burrowd <- Ara_spd[,which(colnames(Ara_spd) %in% burrowers)]
head(burrowd[,1:6]);dim(burrowd)
burrowsum <- rowSums(burrowd)
length(burrowsum)

free <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_type=="Free")]
freed <- Ara_spd[,which(colnames(Ara_spd) %in% free)]
head(freed[,1:6]);dim(freed)
freesum <- rowSums(freed)
length(freesum)

sac <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_type=="Sac")]
sacd <- Ara_spd[,which(colnames(Ara_spd) %in% sac)]
head(sacd[,1:6]);dim(sacd)
sacsum <- rowSums(sacd)
length(sacsum)

web <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_type=="Web")]
webd <- Ara_spd[,which(colnames(Ara_spd) %in% web)]
head(webd[,1:6]);dim(webd)
websum <- rowSums(webd)
length(websum)

active <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Capture_style=="Active")]
actived <- Ara_spd[,which(colnames(Ara_spd) %in% active)]
head(actived[,1:6]);dim(actived)
activesum <- rowSums(actived)
length(activesum)

snare <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Capture_style=="Snare")]
snared <- Ara_spd[,which(colnames(Ara_spd) %in% snare)]
head(snared[,1:6]);dim(snared)
snaresum <- rowSums(snared)
length(snaresum)

vagrant <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Capture_style=="Vagrant")]
vagrantd <- Ara_spd[,which(colnames(Ara_spd) %in% vagrant)]
head(vagrantd[,1:6]);dim(vagrantd)
vagrantsum <- rowSums(vagrantd)
length(vagrantsum)

a_b <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_Capture=="Active_Burrow")]
a_bd <- Ara_spd[,which(colnames(Ara_spd) %in% a_b)]
head(a_bd[,1:6]);dim(a_bd)
a_bsum <- rowSums(a_bd)
length(a_bsum)

a_f <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_Capture=="Active_Free")]
a_fd <- Ara_spd[,which(colnames(Ara_spd) %in% a_f)]
head(a_fd);dim(a_fd)
a_fsum <- rowSums(a_fd)
length(a_fsum)

a_s <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_Capture=="Active_Sac")]
a_sd <- Ara_spd[,which(colnames(Ara_spd) %in% a_s)]
head(a_sd);dim(a_sd)
a_ssum <- rowSums(a_sd)
length(a_ssum)
head(a_ssum)

a_w <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_Capture=="Active_Web")]
a_wd <- Ara_spd[,which(colnames(Ara_spd) %in% a_w)]
head(a_wd)
a_wsum <- a_wd
length(a_wd)
head(a_wd)

s_w <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_Capture=="Snare_Web")]
s_wd <- Ara_spd[,which(colnames(Ara_spd) %in% s_w)]
head(s_wd)
s_wsum <- rowSums(s_wd)
length(s_wsum)

v_b <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_Capture=="Vagrant_Burrow")]
v_bd <- Ara_spd[,which(colnames(Ara_spd) %in% v_b)]
head(v_bd)
v_bsum <- rowSums(v_bd)
length(v_bsum)

v_f <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_Capture=="Vagrant_Free")]
v_fd <- Ara_spd[,which(colnames(Ara_spd) %in% v_f)]
head(v_fd)
v_fsum <- rowSums(v_fd)
length(v_fsum)

v_s <- Ara_funcabun$Morphospecies[which(Ara_funcabun$Retreat_Capture=="Vagrant_Sac")]
v_sd <- Ara_spd[,which(colnames(Ara_spd) %in% v_s)]
head(v_sd)
v_ssum <- rowSums(v_sd)
length(v_ssum)
ara_groups <- cbind(Ara_sited,data.frame(Burrow=burrowsum),data.frame(Free=freesum),data.frame(Sac=sacsum),data.frame(Web=websum),data.frame(Active=activesum),data.frame(Snare=snaresum),data.frame(Vagrant=vagrantsum),data.frame(Active_Burrow=a_bsum),data.frame(Active_Free=a_fsum),data.frame(Active_Sac=a_ssum),data.frame(Active_Web=a_wd),data.frame(Snare_Web=s_wsum),data.frame(Vagrant_Burrow=v_bsum),data.frame(Vagrant_Free=v_fsum),data.frame(Vagrant_Sac=v_ssum))
ara_groups$Yr <- ara_groups$Year-min(ara_groups$Year)
ara_groups <- ara_groups[,c(1:6,which(colnames(ara_groups)=="Yr"),which(colnames(ara_groups)=="Burrow"):which(colnames(ara_groups)=="Vagrant_Sac"))]
ara_groups$Site <- as.factor(ara_groups$Site)
ara_groups$Plot <- as.factor(ara_groups$Plot)
ara_groups$Treatment <- as.factor(ara_groups$Treatment)
ara_groups$Replicate <- as.factor(ara_groups$Replicate)
head(ara_groups)

aragroup_summary1 <- apply(ara_groups[,which(colnames(ara_groups)=="Burrow"):ncol(ara_groups)],MARGIN = 2,FUN=function(x)table(x==0)[2]/sum(table(x==0)))
aragroup_summary <- data.frame(group=names(aragroup_summary1),propzero=aragroup_summary1)
rownames(aragroup_summary) <- 1:nrow(aragroup_summary)
#table(ara_groups$burrow==0)[2]/sum(table(ara_groups$burrow==0))

aragroup_summary$abun <- apply(ara_groups[,which(colnames(ara_groups)=="Burrow"):ncol(ara_groups)],MARGIN = 2,FUN=function(x)sum(x))
aragroup_summary$fit_binom <- ifelse(aragroup_summary$propzero<0.2,'no','yes')
aragroup_summary$fit_abun <- ifelse(aragroup_summary$abun<80,'no','yes')

head(ara_groups);dim(ara_groups)
ara_gr <- colnames(ara_groups)[which(colnames(ara_groups)=="Burrow"):ncol(ara_groups)]
aragroup_summary
aragroup_summary$p_binom_int <- NA
aragroup_summary$p_abun_int <- NA
aragroup_binom <- list()
aragroup_bicoef <- list()
aragroup_bipred <- list()
aragroup_abun <- list()
aragroup_abcoef <- list()
aragroup_abpred <- list()

for(i in 1:length(ara_gr)){
  group_this_run <- ara_gr[i]
  sum_this_run <- aragroup_summary[which(aragroup_summary$group==group_this_run),]
  dat_this_run <- ara_groups[,c(1:7,which(colnames(ara_groups)==group_this_run))]
  dg_this_run <- dat_this_run[,which(colnames(dat_this_run)==group_this_run)]
  head(dat_this_run)
  if(sum_this_run$fit_binom=='yes'){
    dg_binom <- ifelse(dg_this_run>0,1,0)
    binom_this_run <- glmer(dg_binom ~ Treatment+Yr+Treatment:Yr+(1|Site/Replicate),family=binomial,data=dat_this_run)
    binom_sum<-summary(binom_this_run)$coefficients
    aragroup_binom[[i]]<-binom_this_run
    aragroup_bicoef[[i]]<-binom_sum
    aragroup_summary$p_binom_int[i]<-round(binom_sum[which(rownames(binom_sum)=='TreatmentRock:Yr'),which(colnames(binom_sum)=='Pr(>|z|)')],4)
    
    binom_nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(dat_this_run$Treatment),2),levels=levels(dat_this_run$Treatment)))
    binom_pr <- predictSE(mod=binom_this_run,newdata=binom_nd,se.fit=T,type='link')
    binom_pr<-data.frame(binom_nd, fit.link=binom_pr$fit, se=binom_pr$se.fit)
    binom_pr$lci.link<-binom_pr$fit-(1.96*binom_pr$se)
    binom_pr$uci.link<-binom_pr$fit+(1.96*binom_pr$se)
    binom_pr$fit.resp<-invlogit(binom_pr$fit.link)
    binom_pr$lci.resp<-invlogit(binom_pr$lci.link)
    binom_pr$uci.resp<-invlogit(binom_pr$uci.link)
    binom_pr
    aragroup_bipred[[i]]<-binom_pr
  } #close fit_binom
  
  if(sum_this_run$fit_abun=='yes'){
    abun_this_run<-glmmadmb(dg_this_run~Treatment+Yr+Treatment:Yr+(1|Site/Replicate), family="nbinom", data=dat_this_run)
    abun_sum<-summary(abun_this_run)$coefficients
    aragroup_abun[[i]]<-abun_this_run
    aragroup_abcoef[[i]]<-abun_sum
    aragroup_summary$p_abun_int[i]<-round(abun_sum[which(rownames(abun_sum)=='TreatmentRock:Yr'),which(colnames(abun_sum)=='Pr(>|z|)')],4)
    
    abun_nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(dat_this_run$Treatment),2),levels=levels(dat_this_run$Treatment)))
    abun_pr <- predict(abun_this_run,newdata=abun_nd,se.fit=T,type='link')
    abun_pr<-data.frame(abun_nd, fit.link=abun_pr$fit, se=abun_pr$se.fit)
    abun_pr$lci.link<-abun_pr$fit-(1.96*abun_pr$se)
    abun_pr$uci.link<-abun_pr$fit+(1.96*abun_pr$se)
    abun_pr$fit.resp<-exp(abun_pr$fit.link)
    abun_pr$lci.resp<-exp(abun_pr$lci.link)
    abun_pr$uci.resp<-exp(abun_pr$uci.link)
    abun_pr
    aragroup_abpred[[i]]<-abun_pr
  } #close fit_abun
  
} #close i

#save.image('Workspace/Invert_Rocks_E.RData')

aragroup_abun
summary(aragroup_abun[[7]])
aragroup_abcoef[[7]]
aragroup_abpred[[7]]

aragroup_binom
summary(aragroup_binom[[6]])
aragroup_bicoef
aragroup_bipred[[]]

#Abundance contrasts
ara_absignif <- data.frame(group=aragroup_summary$group[which(aragroup_summary$p_abun_int<0.05)])
dim(aragroup_summary)
ara_absignif$index <- which(aragroup_summary$p_abun_int<0.05)
sac_coeff <- aragroup_abcoef[[ara_absignif$index[which(ara_absignif$group=='Sac')]]]
snare_coeff <- aragroup_abcoef[[ara_absignif$index[which(ara_absignif$group=='Snare')]]]
actsac_coeff <- aragroup_abcoef[[ara_absignif$index[which(ara_absignif$group=='Active_Sac')]]]

sac_mod <- aragroup_abun[[ara_absignif$index[which(ara_absignif$group=='Sac')]]]
snare_mod <- aragroup_abun[[ara_absignif$index[which(ara_absignif$group=='Snare')]]]
actsac_mod <- aragroup_abun[[ara_absignif$index[which(ara_absignif$group=='Active_Sac')]]]

#There are 6 contrasts for four categories (c16:r16,c16:c19, C16:r19, r16:c19, r16:r19, c19:r19)
ara_c<-data.frame(Year=rep(unique(ara_groups$Year)[order(unique(ara_groups$Year))],rep(2,2)),Treatment=c('C','R'))
ara_c$Year_Treatment <- paste(ara_c$Year,ara_c$Treatment,sep='_')
ara_contrast<-data.frame(contrast=paste(combn(ara_c$Year_Treatment,2)[1,],combn(ara_c$Year_Treatment,2)[2,],sep=':'))

#Create unique model matrix
mm_ara <- lm(dg_binom ~ Treatment+Yr+Treatment:Yr,data=ara_groups,x=T)$x
umm_ara <- unique(mm_ara)

#WARNING NUMERIC SUBSETS - put them in the natural order 2016-2019, c to r
umm_ara <- umm_ara[c(4,3,2,1),]
rownames(umm_ara) <- 1:nrow(umm_ara)

#Create a difference matrix
#Each row must be a vector with a length equal to the number of rows in the unique model matrix (umm), e.g. four rows in umm_form matrix will give 6 contrasts. Each row will specify one contrast.
diffm_ara <- rbind(
  c(-1,1,0,0),
  c(-1,0,1,0),
  c(-1,0,0,1),
  c(0,-1,1,0),
  c(0,-1,0,1),
  c(0,0,-1,1)
)

#Now we have a unique model matrix
umm_ara

#and we have a difference matrix
diffm_ara

#and we have the names for the contrast
ara_contrast

#calculate the differences and CI's (abun)
sac_diff<-data.frame(contrast=ara_contrast,diff.est(model = sac_mod,unique.mod.mat = umm_ara,diff.matrix = diffm_ara))
sac_diff$diff <- ifelse(sign(sac_diff$lci)==sign(sac_diff$uci),1,0)

snare_diff<-data.frame(contrast=ara_contrast,diff.est(model = snare_mod,unique.mod.mat = umm_ara,diff.matrix = diffm_ara))
snare_diff$diff <- ifelse(sign(snare_diff$lci)==sign(snare_diff$uci),1,0)

actsac_diff<-data.frame(contrast=ara_contrast,diff.est(model = actsac_mod,unique.mod.mat = umm_ara,diff.matrix = diffm_ara))
actsac_diff$diff <- ifelse(sign(actsac_diff$lci)==sign(actsac_diff$uci),1,0)

dev.new(width=18,height=8,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,2),mar=c(5,5,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))

sac_plot<-aragroup_abpred[[3]]
plot(1:4,sac_plot$fit.resp,ylim=c(0,max(sac_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main=aragroup_summary$group[3],font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,sac_plot$lci.resp,1:4,sac_plot$uci.resp,length=0.05,angle=90,code=3)
text(x=0.5, y=0.15,labels=paste('Int.p=',round(aragroup_summary$p_abun_int[3],3),sep=''),adj=0)
text(x=1:4, y=max(sac_plot$uci.resp)+0.4,labels=c(rep("ab"),rep("ab"),rep(letters[c(1)],1),rep(letters[c(2)],1)))
actsac_plot<-aragroup_abpred[[10]]
plot(1:4,actsac_plot$fit.resp,ylim=c(0,max(actsac_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main=aragroup_summary$group[10],font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,actsac_plot$lci.resp,1:4,actsac_plot$uci.resp,length=0.05,angle=90,code=3)
text(x=0.5, y=0.15,labels=paste('Int.p=',round(aragroup_summary$p_abun_int[10],3),sep=''),adj=0)
text(x=1:4, y=max(actsac_plot$uci.resp)+0.4,labels=c(rep("ab"),rep("ab"),rep(letters[c(1)],1),rep(letters[c(2)],1)))
par(xpd=NA)
legend(x=4.75, y=2.5, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Check for models that do not converge and remove.

aragroup_summary$p_binom_int[which(aragroup_summary$group=='Active_Web')] <- NA

length(which(!is.na(aragroup_summary$p_binom_int)))
binom.plot <- which(!is.na(aragroup_summary$p_binom_int))

dev.new(width=14,height=12,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(3,3),mar=c(5,5,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))

for(i in binom.plot){
  
  pred.this.run<-aragroup_bipred[[i]]
  plot(1:4,pred.this.run$fit.resp,ylim=c(0,1),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Prob. occurence',xlab='Year',main=aragroup_summary$group[i],font.main=1)
  
  axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
  arrows(1:4,pred.this.run$lci.resp,1:4,pred.this.run$uci.resp,length=0.05,angle=90,code=3)
  text(x=0.75, y=0.9,labels=paste('Int.p=',round(aragroup_summary$p_binom_int[i],3),sep=''),adj=0)
  
} #close plot loop

par(xpd=NA)
legend(x=6, y=1, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Abundance model plots - Spiders

length(which(!is.na(aragroup_summary$p_abun_int)))
abun.plot <- which(!is.na(aragroup_summary$p_abun_int))

dev.new(width=18,height=20,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(4,3),mar=c(5,5,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))

for(i in abun.plot){
  
  pred.this.run<-aragroup_abpred[[i]]
  plot(1:4,pred.this.run$fit.resp,ylim=c(0,max(pred.this.run$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main=aragroup_summary$group[i],font.main=1)
  
  axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
  arrows(1:4,pred.this.run$lci.resp,1:4,pred.this.run$uci.resp,length=0.05,angle=90,code=3)
  text(x=0.5, y=0.15,labels=paste('Int.p=',round(aragroup_summary$p_abun_int[i],3),sep=''),adj=0)
  
} #close plot loop

par(xpd=NA)
legend(x=5, y=10, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

# if needed can add this to correct model errors admb.opts=admbControl(impSamp=200,shess=FALSE,noinit=FALSE)

#Close Araneae----

#Coleoptera----
head(Col2[,1:8])
head(Col_funcabun[,1:8]);dim(Col_funcabun)
table(colnames(Col2)[which(colnames(Col2)=="Anthicidae1"):ncol(Col2)] %in% Col_funcabun$Morphospecies)
colnames(Col2)[which(colnames(Col2)=="Anthicidae1"):ncol(Col2)][which(!colnames(Col2)[which(colnames(Col2)=="Anthicidae1"):ncol(Col2)] %in% Col_funcabun$Morphospecies)]
Col_occur <- colnames(Col2)[which(colnames(Col2)=="Anthicidae1"):ncol(Col2)]
Col_func <- Col_funcabun$Morphospecies
Col_occur[which(!Col_occur %in% Col_func)]
Col_func[which(!Col_func %in% Col_occur)]

which(duplicated(Col_occur))
which(duplicated(Col_func))

Col_funcabun$Size2<-ifelse(Col_funcabun$Size<5,'small','big')

Col_funcabun$Size2 <- as.factor(Col_funcabun$Size2)
levels(Col_funcabun$Size2)
table(Col_funcabun$Size2)
Col_funcabun$Trophic.level <- as.factor(Col_funcabun$Trophic.level)
levels(Col_funcabun$Trophic.level)
table(Col_funcabun$Trophic.level)
Col_funcabun$Flight <- as.factor(Col_funcabun$Flight)
levels(Col_funcabun$Flight)
table(Col_funcabun$Flight)
Col_funcabun$Larval_hab <- as.factor(Col_funcabun$Larval_hab)
levels(Col_funcabun$Larval_hab)
table(Col_funcabun$Larval_hab)
Col_funcabun$Size_Trophic <- paste(Col_funcabun$Size2,Col_funcabun$Trophic.level,sep="_")
Col_funcabun$Size_Trophic <- as.factor(Col_funcabun$Size_Trophic)
levels(Col_funcabun$Size_Trophic)
table(Col_funcabun$Size_Trophic)
Col_funcabun$Size_Flight <- paste(Col_funcabun$Size2,Col_funcabun$Flight,sep="_")
Col_funcabun$Size_Flight <- as.factor(Col_funcabun$Size_Flight)
levels(Col_funcabun$Size_Flight)
table(Col_funcabun$Size_Flight)
Col_funcabun$Size_Larval_hab <- paste(Col_funcabun$Size2,Col_funcabun$Larval_hab,sep="_")
Col_funcabun$Size_Larval_hab <- as.factor(Col_funcabun$Size_Larval_hab)
levels(Col_funcabun$Size_Larval_hab)
table(Col_funcabun$Size_Larval_hab)
Col_funcabun$Trophic_Flight <- paste(Col_funcabun$Trophic.level,Col_funcabun$Flight,sep="_")
Col_funcabun$Trophic_Flight <- as.factor(Col_funcabun$Trophic_Flight)
levels(Col_funcabun$Trophic_Flight)
table(Col_funcabun$Trophic_Flight)
Col_funcabun$Trophic_Larval_hab <- paste(Col_funcabun$Trophic.level,Col_funcabun$Larval_hab,sep="_")
Col_funcabun$Trophic_Larval_hab <- as.factor(Col_funcabun$Trophic_Larval_hab)
levels(Col_funcabun$Trophic_Larval_hab)
table(Col_funcabun$Trophic_Larval_hab)
Col_funcabun$Flight_Larval_hab <- paste(Col_funcabun$Larval_hab,Col_funcabun$Flight,sep="_")
Col_funcabun$Flight_Larval_hab <- as.factor(Col_funcabun$Flight_Larval_hab)
levels(Col_funcabun$Flight_Larval_hab)
table(Col_funcabun$Flight_Larval_hab)

#Unsure not analysed

head(Col_funcabun);dim(Col_funcabun)
Col_sited <- Col2[,1:6]
head(Col_sited);dim(Col_sited)
Col_spd <- Col2[,which(colnames(Col2)=="Anthicidae1"):ncol(Col2)]
head(Col_spd[,1:6])

big <- Col_funcabun$Morphospecies[which(Col_funcabun$Size2=="big")]
big_d <- Col_spd[,which(colnames(Col_spd) %in% big)]
head(big_d);dim(big_d)
big_sum <- rowSums(big_d)
length(big_sum)

small <- Col_funcabun$Morphospecies[which(Col_funcabun$Size2=="small")]
small_d <- Col_spd[,which(colnames(Col_spd) %in% small)]
head(small_d);dim(small_d)
small_sum <- rowSums(small_d)
length(small_sum)

detritivore <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic.level=="Detritivore")]
det_d <- Col_spd[,which(colnames(Col_spd) %in% detritivore)]
head(det_d);dim(det_d)
det_sum <- rowSums(det_d)
length(det_sum)

omnivore <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic.level=="Omnivore")]
omn_d <- Col_spd[,which(colnames(Col_spd) %in% omnivore)]
head(omn_d);dim(omn_d)
omn_sum <- rowSums(omn_d)
length(omn_sum)

predator <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic.level=="Predator")]
pred_d <- Col_spd[,which(colnames(Col_spd) %in% predator)]
head(pred_d);dim(pred_d)
pred_sum <- rowSums(pred_d)
length(pred_sum)

herbivore <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic.level=="Herbivore")]
herb_d <- Col_spd[,which(colnames(Col_spd) %in% herbivore)]
head(herb_d);dim(herb_d)
herb_sum <- rowSums(herb_d)
length(herb_sum)

dnf <- Col_funcabun$Morphospecies[which(Col_funcabun$Flight=="no")]
dnf_d <- Col_spd[,which(colnames(Col_spd) %in% dnf)]
head(dnf_d);dim(dnf_d)
dnf_sum <- rowSums(dnf_d)
length(dnf_sum)

fly <- Col_funcabun$Morphospecies[which(Col_funcabun$Flight=="yes")]
flyd <- Col_spd[,which(colnames(Col_spd) %in% fly)]
head(flyd[,1:6]);dim(flyd)
flysum <- rowSums(flyd)
length(flysum)

hab_above <- Col_funcabun$Morphospecies[which(Col_funcabun$Larval_hab=="above")]
aboved <- Col_spd[,which(colnames(Col_spd) %in% hab_above)]
head(aboved[,1:6]);dim(aboved)
abovesum <- rowSums(aboved)
length(abovesum)

hab_below <- Col_funcabun$Morphospecies[which(Col_funcabun$Larval_hab=="below")]
belowd <- Col_spd[,which(colnames(Col_spd) %in% hab_below)]
head(belowd[,1:6]);dim(belowd)
belowsum <- rowSums(belowd)
length(belowsum)

hab_on <- Col_funcabun$Morphospecies[which(Col_funcabun$Larval_hab=="on")]
ond <- Col_spd[,which(colnames(Col_spd) %in% hab_on)]
head(ond[,1:6]);dim(ond)
onsum <- rowSums(ond)
length(onsum)

#Mixed trait groups

big_det <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Trophic=="big_Detritivore")]
big_det_d <- Col_spd[,which(colnames(Col_spd) %in% big_det)]
head(big_det_d);dim(big_det_d)
big_detsum <- big_det_d
length(big_detsum)

big_herb <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Trophic=="big_Herbivore")]
big_herb_d <- Col_spd[,which(colnames(Col_spd) %in% big_herb)]
head(big_herb_d);dim(big_herb_d)
big_herbsum <- rowSums(big_herb_d)
length(big_herbsum)

big_omn <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Trophic=="big_Omnivore")]
big_omn_d <- Col_spd[,which(colnames(Col_spd) %in% big_omn)]
head(big_omn_d);dim(big_omn_d)
big_omnsum <- rowSums(big_omn_d)
length(big_omnsum)

big_pred <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Trophic=="big_Predator")]
big_pred_d <- Col_spd[,which(colnames(Col_spd) %in% big_pred)]
head(big_pred_d);dim(big_pred_d)
big_predsum <- rowSums(big_pred_d)
length(big_predsum)

small_det <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Trophic=="small_Detritivore")]
small_det_d <- Col_spd[,which(colnames(Col_spd) %in% small_det)]
head(small_det_d);dim(small_det_d)
small_detsum <- rowSums(small_det_d)
length(small_detsum)

small_herb <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Trophic=="small_Herbivore")]
small_herb_d <- Col_spd[,which(colnames(Col_spd) %in% small_herb)]
head(small_herb_d);dim(small_herb_d)
small_herbsum <- rowSums(small_herb_d)
length(small_herbsum)

small_omn <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Trophic=="small_Omnivore")]
small_omn_d <- Col_spd[,which(colnames(Col_spd) %in% small_omn)]
head(small_omn_d);dim(small_omn_d)
small_omnsum <- rowSums(small_omn_d)
length(small_omnsum)

small_pred <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Trophic=="small_Predator")]
small_pred_d <- Col_spd[,which(colnames(Col_spd) %in% small_pred)]
head(small_pred_d);dim(small_pred_d)
small_predsum <- rowSums(small_pred_d)
length(small_predsum)

big_dnf <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Flight=="big_no")]
big_dnf_d <- Col_spd[,which(colnames(Col_spd) %in% big_dnf)]
head(big_dnf_d);dim(big_dnf_d)
big_dnfsum <- rowSums(big_dnf_d)
length(big_dnfsum)

big_fly <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Flight=="big_yes")]
big_fly_d <- Col_spd[,which(colnames(Col_spd) %in% big_fly)]
head(big_fly_d);dim(big_fly_d)
big_flysum <- rowSums(big_fly_d)
length(big_flysum)

small_dnf <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Flight=="small_no")]
small_dnf_d <- Col_spd[,which(colnames(Col_spd) %in% small_dnf)]
head(small_dnf_d);dim(small_dnf_d)
small_dnfsum <- rowSums(small_dnf_d)
length(small_dnfsum)

small_fly <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Flight=="small_yes")]
small_fly_d <- Col_spd[,which(colnames(Col_spd) %in% small_fly)]
head(small_fly_d);dim(small_fly_d)
small_flysum <- rowSums(small_fly_d)
length(small_flysum)

big_above <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Larval_hab=="big_above")]
big_above_d <- Col_spd[,which(colnames(Col_spd) %in% big_above)]
head(big_above_d);dim(big_above_d)
big_abovesum <- rowSums(big_above_d)
length(big_abovesum)

big_below <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Larval_hab=="big_below")]
big_below_d <- Col_spd[,which(colnames(Col_spd) %in% big_below)]
head(big_below_d);dim(big_below_d)
big_belowsum <- rowSums(big_below_d)
length(big_belowsum)

big_on <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Larval_hab=="big_on")]
big_on_d <- Col_spd[,which(colnames(Col_spd) %in% big_on)]
head(big_on_d);dim(big_on_d)
big_onsum <- rowSums(big_on_d)
length(big_onsum)

small_above <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Larval_hab=="small_above")]
small_above_d <- Col_spd[,which(colnames(Col_spd) %in% small_above)]
head(small_above_d);dim(small_above_d)
small_abovesum <- rowSums(small_above_d)
length(small_abovesum)

small_below <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Larval_hab=="small_below")]
small_below_d <- Col_spd[,which(colnames(Col_spd) %in% small_below)]
head(small_below_d);dim(small_below_d)
small_belowsum <- rowSums(small_below_d)
length(small_belowsum)

small_on <- Col_funcabun$Morphospecies[which(Col_funcabun$Size_Larval_hab=="small_on")]
small_on_d <- Col_spd[,which(colnames(Col_spd) %in% small_on)]
head(small_on_d);dim(small_on_d)
small_onsum <- rowSums(small_on_d)
length(small_onsum)

det_dnf <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Flight=="Detritivore_no")]
det_dnf_d <- Col_spd[,which(colnames(Col_spd) %in% det_dnf)]
head(det_dnf_d);dim(det_dnf_d)
det_dnfsum <- det_dnf_d
length(det_dnfsum)

det_fly <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Flight=="Detritivore_yes")]
det_fly_d <- Col_spd[,which(colnames(Col_spd) %in% det_fly)]
head(det_fly_d);dim(det_fly_d)
det_flysum <- rowSums(det_fly_d)
length(det_flysum)

herb_dnf <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Flight=="Herbivore_no")]
herb_dnf_d <- Col_spd[,which(colnames(Col_spd) %in% herb_dnf)]
head(herb_dnf_d);dim(herb_dnf_d)
herb_dnfsum <- rowSums(herb_dnf_d)
length(herb_dnfsum)

herb_fly <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Flight=="Herbivore_yes")]
herb_fly_d <- Col_spd[,which(colnames(Col_spd) %in% herb_fly)]
head(herb_fly_d);dim(herb_fly_d)
herb_flysum <- rowSums(herb_fly_d)
length(herb_flysum)

omn_dnf <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Flight=="Omnivore_no")]
omn_dnf_d <- Col_spd[,which(colnames(Col_spd) %in% omn_dnf)]
head(omn_dnf_d);dim(omn_dnf_d)
omn_dnfsum <- rowSums(omn_dnf_d)
length(omn_dnfsum)

omn_fly <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Flight=="Omnivore_yes")]
omn_fly_d <- Col_spd[,which(colnames(Col_spd) %in% omn_fly)]
head(omn_fly_d);dim(omn_fly_d)
omn_flysum <- rowSums(omn_fly_d)
length(omn_flysum)

pred_dnf <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Flight=="Predator_no")]
pred_dnf_d <- Col_spd[,which(colnames(Col_spd) %in% pred_dnf)]
head(pred_dnf_d);dim(pred_dnf_d)
pred_dnfsum <- rowSums(pred_dnf_d)
length(pred_dnfsum)

pred_fly <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Flight=="Predator_yes")]
pred_fly_d <- Col_spd[,which(colnames(Col_spd) %in% pred_fly)]
head(pred_fly_d);dim(pred_fly_d)
pred_flysum <- rowSums(pred_fly_d)
length(pred_flysum)

det_above <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Larval_hab=="Detritivore_above")]
det_above_d <- Col_spd[,which(colnames(Col_spd) %in% det_above)]
head(det_above_d);dim(det_above_d)
det_abovesum <- rowSums(det_above_d)
length(det_abovesum)

det_on <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Larval_hab=="Detritivore_on")]
det_on_d <- Col_spd[,which(colnames(Col_spd) %in% det_on)]
head(det_on_d);dim(det_on_d)
det_onsum <- rowSums(det_on_d)
length(det_onsum)

herb_above <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Larval_hab=="Herbivore_above")]
herb_above_d <- Col_spd[,which(colnames(Col_spd) %in% herb_above)]
head(herb_above_d);dim(herb_above_d)
herb_abovesum <- rowSums(herb_above_d)
length(herb_abovesum)

herb_below <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Larval_hab=="Herbivore_below")]
herb_below_d <- Col_spd[,which(colnames(Col_spd) %in% herb_below)]
head(herb_below_d);dim(herb_below_d)
herb_belowsum <- rowSums(herb_below_d)
length(herb_belowsum)

herb_on <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Larval_hab=="Herbivore_on")]
herb_on_d <- Col_spd[,which(colnames(Col_spd) %in% herb_on)]
head(herb_on_d);dim(herb_on_d)
herb_onsum <- rowSums(herb_on_d)
length(herb_onsum)

omn_above <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Larval_hab=="Omnivore_above")]
omn_above_d <- Col_spd[,which(colnames(Col_spd) %in% omn_above)]
head(omn_above_d);dim(omn_above_d)
omn_abovesum <- rowSums(omn_above_d)
length(omn_abovesum)

omn_on <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Larval_hab=="Omnivore_on")]
omn_on_d <- Col_spd[,which(colnames(Col_spd) %in% omn_on)]
head(omn_on_d);dim(omn_on_d)
omn_onsum <- rowSums(omn_on_d)
length(omn_onsum)

pred_above <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Larval_hab=="Predator_above")]
pred_above_d <- Col_spd[,which(colnames(Col_spd) %in% pred_above)]
head(pred_above_d);dim(pred_above_d)
pred_abovesum <- pred_above_d
length(pred_abovesum)

pred_below <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Larval_hab=="Predator_below")]
pred_below_d <- Col_spd[,which(colnames(Col_spd) %in% pred_below)]
head(pred_below_d);dim(pred_below_d)
pred_belowsum <- rowSums(pred_below_d)
length(pred_belowsum)

pred_on <- Col_funcabun$Morphospecies[which(Col_funcabun$Trophic_Larval_hab=="Predator_on")]
pred_on_d <- Col_spd[,which(colnames(Col_spd) %in% pred_on)]
head(pred_on_d);dim(pred_on_d)
pred_onsum <- rowSums(pred_on_d)
length(pred_onsum)

above_dnf <- Col_funcabun$Morphospecies[which(Col_funcabun$Flight_Larval_hab=="above_no")]
above_dnf_d <- Col_spd[,which(colnames(Col_spd) %in% above_dnf)]
head(above_dnf_d);dim(above_dnf_d)
above_dnfsum <- rowSums(above_dnf_d)
length(above_dnfsum)

above_fly <- Col_funcabun$Morphospecies[which(Col_funcabun$Flight_Larval_hab=="above_yes")]
above_flyd <- Col_spd[,which(colnames(Col_spd) %in% above_fly)]
head(above_flyd);dim(above_flyd)
above_flysum <- rowSums(above_flyd)
length(above_flysum)
head(above_flysum)

below_fly <- Col_funcabun$Morphospecies[which(Col_funcabun$Flight_Larval_hab=="below_yes")]
below_flyd <- Col_spd[,which(colnames(Col_spd) %in% below_fly)]
head(below_flyd);dim(below_flyd)
below_flysum <- rowSums(below_flyd)
length(below_flysum)
head(below_flysum)

on_dnf <- Col_funcabun$Morphospecies[which(Col_funcabun$Flight_Larval_hab=="on_no")]
on_dnf_d <- Col_spd[,which(colnames(Col_spd) %in% on_dnf)]
head(on_dnf_d)
on_dnfsum <- rowSums(on_dnf_d)
length(on_dnfsum)
head(on_dnfsum)

on_fly <- Col_funcabun$Morphospecies[which(Col_funcabun$Flight_Larval_hab=="on_yes")]
on_flyd <- Col_spd[,which(colnames(Col_spd) %in% on_fly)]
head(on_flyd)
on_flysum <- rowSums(on_flyd)
length(on_flysum)
head(on_flysum)

col_groups <- cbind(Col_sited,data.frame(Big=big_sum),data.frame(Small=small_sum),data.frame(Detritivore=det_sum),data.frame(Omnivore=omn_sum),data.frame(Carnivore=pred_sum),data.frame(Herbivore=herb_sum),data.frame(Do_not_fly=dnf_sum),data.frame(Fly=flysum),data.frame(Hab_above=abovesum),data.frame(Hab_below=belowsum),data.frame(Hab_on=onsum),data.frame(Big_det=big_detsum),data.frame(Big_herb=big_herbsum),data.frame(Big_omn=big_omnsum),data.frame(Big_carn=big_predsum),data.frame(Small_det=small_detsum),data.frame(Small_herb=small_herbsum),data.frame(Small_omn=small_omnsum),data.frame(Small_carn=small_predsum),data.frame(Big_dnf=big_dnfsum),data.frame(Big_fly=big_flysum),data.frame(Small_dnf=small_dnfsum),data.frame(Small_fly=small_flysum),data.frame(Big_above=big_abovesum),data.frame(Big_below=big_belowsum),data.frame(Big_on=big_onsum),data.frame(Small_above=small_abovesum),data.frame(Small_below=small_belowsum),data.frame(Small_on=small_onsum),data.frame(Det_dnf=det_dnfsum),data.frame(Det_fly=det_flysum),data.frame(Herb_dnf=herb_dnfsum),data.frame(Herb_fly=herb_flysum),data.frame(Omn_dnf=omn_dnfsum),data.frame(Omn_fly=omn_flysum),data.frame(Carn_dnf=pred_dnfsum),data.frame(Carn_fly=pred_flysum),data.frame(Det_above=det_abovesum),data.frame(Det_on=det_onsum),data.frame(Herb_above=herb_abovesum),data.frame(Herb_below=herb_belowsum),data.frame(Herb_on=herb_onsum),data.frame(Omn_above=omn_abovesum),data.frame(Omn_on=omn_onsum),data.frame(Carn_above=pred_abovesum),data.frame(Carn_below=pred_belowsum),data.frame(Carn_on=pred_onsum),data.frame(Above_dnf=above_dnfsum),data.frame(Above_fly=above_flysum),data.frame(Below_fly=below_flysum),data.frame(On_dnf=on_dnfsum),data.frame(On_fly=on_flysum))
col_groups$Yr <- col_groups$Year-min(col_groups$Year)
col_groups <- col_groups[,c(1:6,which(colnames(col_groups)=="Yr"),which(colnames(col_groups)=="Big"):which(colnames(col_groups)=="On_fly"))]
col_groups$Site <- as.factor(col_groups$Site)
col_groups$Plot <- as.factor(col_groups$Plot)
col_groups$Treatment <- as.factor(col_groups$Treatment)
col_groups$Replicate <- as.factor(col_groups$Replicate)
head(col_groups);dim(col_groups)

colgroups_summary1 <- apply(col_groups[,which(colnames(col_groups)=="Big"):ncol(col_groups)],MARGIN = 2,FUN=function(x)table(x==0)[2]/sum(table(x==0)))
colgroups_summary <- data.frame(group=names(colgroups_summary1),propzero=colgroups_summary1)
rownames(colgroups_summary) <- 1:nrow(colgroups_summary)
#table(col_groups$big==0)[2]/sum(table(col_groups$big==0))

colgroups_summary$abun <- apply(col_groups[,which(colnames(col_groups)=="Big"):ncol(col_groups)],MARGIN = 2,FUN=function(x)sum(x))
colgroups_summary$fit_binom <- ifelse(colgroups_summary$propzero<0.2|colgroups_summary$propzero>0.9,'no','yes')
colgroups_summary$fit_abun <- ifelse(colgroups_summary$abun<80,'no','yes')

head(colgroups_summary)
head(col_groups)
table(col_groups$Site,col_groups$Hab_below)

head(col_groups);dim(col_groups)
col_gr <- colnames(col_groups)[which(colnames(col_groups)=="Big"):ncol(col_groups)]
colgroups_summary
colgroups_summary$p_binom_int <- NA
colgroups_summary$p_abun_int <- NA
colgroup_binom <- list()
colgroup_bicoef <- list()
colgroup_bipred <- list()
colgroup_abun <- list()
colgroup_abcoef <- list()
colgroup_abpred <- list()

for(i in 48:length(col_gr)){
  group_this_run <- col_gr[i]
  sum_this_run <- colgroups_summary[which(colgroups_summary$group==group_this_run),]
  dat_this_run <- col_groups[,c(1:7,which(colnames(col_groups)==group_this_run))]
  dg_this_run <- dat_this_run[,which(colnames(dat_this_run)==group_this_run)]
  head(dat_this_run);dim(dat_this_run)
  if(sum_this_run$fit_binom=='yes'){
    dg_binom <- ifelse(dg_this_run>0,1,0)
    binom_this_run <- glmer(dg_binom ~ Treatment+Yr+Treatment:Yr+(1|Site/Replicate),family=binomial,data=dat_this_run)
    binom_sum<-summary(binom_this_run)$coefficients
    colgroup_binom[[i]]<-binom_this_run
    colgroup_bicoef[[i]]<-binom_sum
    colgroups_summary$p_binom_int[i]<-round(binom_sum[which(rownames(binom_sum)=='TreatmentRock:Yr'),which(colnames(binom_sum)=='Pr(>|z|)')],4)
    
    binom_nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(dat_this_run$Treatment),2),levels=levels(dat_this_run$Treatment)))
    binom_pr <- predictSE(mod=binom_this_run,newdata=binom_nd,se.fit=T,type='link')
    binom_pr<-data.frame(binom_nd, fit.link=binom_pr$fit, se=binom_pr$se.fit)
    binom_pr$lci.link<-binom_pr$fit-(1.96*binom_pr$se)
    binom_pr$uci.link<-binom_pr$fit+(1.96*binom_pr$se)
    binom_pr$fit.resp<-invlogit(binom_pr$fit.link)
    binom_pr$lci.resp<-invlogit(binom_pr$lci.link)
    binom_pr$uci.resp<-invlogit(binom_pr$uci.link)
    binom_pr
    colgroup_bipred[[i]]<-binom_pr
    
  } #close fit_binom
  
  if(sum_this_run$fit_abun=='yes'){
    abun_this_run<-glmmadmb(dg_this_run~Treatment+Yr+Treatment:Yr+(1|Site/Replicate), family="nbinom", data=dat_this_run)
    abun_sum<-summary(abun_this_run)$coefficients
    colgroup_abun[[i]]<-abun_this_run
    colgroup_abcoef[[i]]<-abun_sum
    colgroups_summary$p_abun_int[i]<-round(abun_sum[which(rownames(abun_sum)=='TreatmentRock:Yr'),which(colnames(abun_sum)=='Pr(>|z|)')],4)
    
    abun_nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(dat_this_run$Treatment),2),levels=levels(dat_this_run$Treatment)))
    abun_pr <- predict(abun_this_run,newdata=abun_nd,se.fit=T,type='link')
    abun_pr<-data.frame(abun_nd, fit.link=abun_pr$fit, se=abun_pr$se.fit)
    abun_pr$lci.link<-abun_pr$fit-(1.96*abun_pr$se)
    abun_pr$uci.link<-abun_pr$fit+(1.96*abun_pr$se)
    abun_pr$fit.resp<-exp(abun_pr$fit.link)
    abun_pr$lci.resp<-exp(abun_pr$lci.link)
    abun_pr$uci.resp<-exp(abun_pr$uci.link)
    abun_pr
    colgroup_abpred[[i]]<-abun_pr
  } #close fit_abun
  
} #close i

#Remove sites without data (if needed)
site_sum<-tapply(dat_this_run[,which(colnames(dat_this_run)==group_this_run)],INDEX=dat_this_run$Site,FUN=sum)
site_to_remove<-names(which(site_sum==0))
if(length(site_to_remove)>0){
  dat_this_run<-dat_this_run[-which(dat_this_run$Site %in% site_to_remove),]
  dat_this_run<-droplevels(dat_this_run)
  rownames(dat_this_run)<-1:nrow(dat_this_run)}#close remove sites

colgroup_abun
colgroup_abcoef
colgroup_abpred

colgroup_binom
summary(colgroup_binom[[]])
colgroup_bicoef
colgroup_bipred

#Abundance contrasts
col_absignif <- data.frame(group=colgroups_summary$group[which(colgroups_summary$p_abun_int<0.05)])
dim(colgroups_summary)
col_absignif$index <- which(colgroups_summary$p_abun_int<0.05)
col_absignif
herb_coeff <- colgroup_abcoef[[col_absignif$index[which(col_absignif$group=='Herbivore')]]]

herb_mod <- colgroup_abun[[col_absignif$index[which(col_absignif$group=='Herbivore')]]]

#There are 6 contrasts for four categories (c16:r16,c16:c19, C16:r19, r16:c19, r16:r19, c19:r19)
col_c<-data.frame(Year=rep(unique(col_groups$Year)[order(unique(col_groups$Year))],rep(2,2)),Treatment=c('C','R'))
col_c$Year_Treatment <- paste(col_c$Year,col_c$Treatment,sep='_')
col_contrast<-data.frame(contrast=paste(combn(col_c$Year_Treatment,2)[1,],combn(col_c$Year_Treatment,2)[2,],sep=':'))

#Create unique model matrix
mm_col <- lm(dg_binom ~ Treatment+Yr+Treatment:Yr,data=col_groups,x=T)$x
umm_col <- unique(mm_col)

#WARNING NUMERIC SUBSETS - put them in the natural order 2016-2019, c to r
umm_col <- umm_col[c(4,3,2,1),]
rownames(umm_col) <- 1:nrow(umm_col)

#Create a difference matrix
#Each row must be a vector with a length equal to the number of rows in the unique model matrix (umm), e.g. four rows in umm_form matrix will give 6 contrasts. Each row will specify one contrast.
diffm_col <- rbind(
  c(-1,1,0,0),
  c(-1,0,1,0),
  c(-1,0,0,1),
  c(0,-1,1,0),
  c(0,-1,0,1),
  c(0,0,-1,1)
)

#Now we have a unique model matrix
umm_col

#and we have a difference matrix
diffm_col

#and we have the names for the contrast
col_contrast

#calculate the differences and CI's (abun)
herb_diff<-data.frame(contrast=col_contrast,diff.est(model = herb_mod,unique.mod.mat = umm_col,diff.matrix = diffm_col))
herb_diff$diff <- ifelse(sign(herb_diff$lci)==sign(herb_diff$uci),1,0)

#Groups excluded due to errors. Adjustments not necessary.
colgroups_summary$p_binom_int[which(colgroups_summary$group=='Det_dnf')] <- NA

colgroups_summary$p_b_int_adj<-p.adjust(colgroups_summary$p_binom_int,method="hochberg",n=nrow(colgroups_summary)-length(which(is.na(colgroups_summary$p_binom_int))))
colgroups_summary$p_a_int_adj<-p.adjust(colgroups_summary$p_abun_int,method="hochberg",n=nrow(colgroups_summary)-length(which(is.na(colgroups_summary$p_abun_int))))

length(which(!is.na(colgroups_summary$p_binom_int)))
binom.plot <- which(!is.na(colgroups_summary$p_binom_int))

dev.new(width=18,height=20,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(5,4),mar=c(5,5,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))

for(i in binom.plot){
  
  pred.this.run<-colgroup_bipred[[i]]
  plot(1:4,pred.this.run$fit.resp,ylim=c(0,1),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Prob. occurence',xlab='',main=colgroups_summary$group[i],font.main=1)
  
  axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'),mgp=c(2,0.8,0))
  arrows(1:4,pred.this.run$lci.resp,1:4,pred.this.run$uci.resp,length=0.05,angle=90,code=3)
  text(x=0.5, y=0.1,labels=paste('Int.p=',round(colgroups_summary$p_binom_int[i],2),sep=''),adj=0)
  title(xlab='Year',mgp=c(2,0.8,0))
  
} #close plot loop

par(xpd=NA)
legend(x=8, y=1, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Abundance model plots - Beetles

length(which(!is.na(colgroups_summary$p_abun_int)))
abun.plot <- which(!is.na(colgroups_summary$p_abun_int))

dev.new(width=60,height=36,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(5,8),mar=c(4,4,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))

for(i in abun.plot){
  
  pred.this.run<-colgroup_abpred[[i]]
  plot(1:4,pred.this.run$fit.resp,ylim=c(0,max(pred.this.run$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='',main=colgroups_summary$group[i],font.main=1)
  
  axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'),mgp=c(2,0.8,0))
  arrows(1:4,pred.this.run$lci.resp,1:4,pred.this.run$uci.resp,length=0.05,angle=90,code=3)
  text(x=2, y=(par('usr')[4])-par('usr')[4]/10,labels=paste('Int.p=',round(colgroups_summary$p_abun_int[i],2),sep=''),adj=0)
  title(xlab='Year',mgp=c(2,0.8,0))
  
} #close plot loop

par(xpd=NA)
legend(x=6, y=36, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Close Coleoptera ----

#Formicidae ----

head(Form2[,1:10])
head(Form_funcabun[,1:10]);dim(Form_funcabun)
table(colnames(Form2)[which(colnames(Form2)=="Amblyoponinae1"):ncol(Form2)] %in% Form_funcabun$Morphospecies)
colnames(Form2)[which(colnames(Form2)=="Amblyoponinae1"):ncol(Form2)][which(!colnames(Form2)[which(colnames(Form2)=="Amblyoponinae1"):ncol(Form2)] %in% Form_funcabun$Morphospecies)]
Form_occur <- colnames(Form2)[which(colnames(Form2)=="Amblyoponinae1"):ncol(Form2)]
Form_func <- Form_funcabun$Morphospecies
Form_occur[which(!Form_occur %in% Form_func)]
Form_func[which(!Form_func %in% Form_occur)]

which(duplicated(Form_occur))
which(duplicated(Form_func))

Form_funcabun$Size2<-ifelse(Form_funcabun$Size<5,'Small','Big')
Form_funcabun$Colony_size2<-Form_funcabun$Colony_size
Form_funcabun$Colony_size2[which(Form_funcabun$Colony_size=='Tens')] <- 'Few'
Form_funcabun$Colony_size2[which(Form_funcabun$Colony_size=='Hundreds')] <- 'Many'
Form_funcabun$Colony_size2[which(Form_funcabun$Colony_size=='Thousands')] <- 'Many'
Form_funcabun$Colony_size2[which(Form_funcabun$Colony_size=='Millions')] <- 'Many'
Form_funcabun$Colony_size2[which(Form_funcabun$Colony_size=='Unknown')] <- 'Unknown'

#Create summary data ----
#Exclude foraging site and food preference as these effects are captured by others
head(Form_funcabun,3)
form_funcabun2<-Form_funcabun[,c('Size2','Trophic.level','Food_preference','Nest_site','Colony_size2','Foraging_style','Foraging_site')]
head(form_funcabun2,3)
plot(form_funcabun2)
form_funcabun2$Colony_size2<-as.factor(form_funcabun2$Colony_size2)
form_funcabun3<-data.frame(as.numeric(form_funcabun2$Size2),as.numeric(form_funcabun2$Trophic.level),as.numeric(form_funcabun2$Food_preference),as.numeric(form_funcabun2$Nest_site),as.numeric(form_funcabun2$Colony_size2),as.numeric(form_funcabun2$Foraging_style),as.numeric(form_funcabun2$Foraging_site))
head(form_funcabun3,3)
colnames(form_funcabun3)<-colnames(form_funcabun2)
corrffabund<-cor(form_funcabun3)
corrplot(corrffabund,method='circle',diag=F)
form_funcabun4<-form_funcabun3[,-which(colnames(form_funcabun3)%in%c('Foraging_site','Food_preference'))]
corrffabund2<-cor(form_funcabun4)

Form_funcabun$Size2 <- as.factor(Form_funcabun$Size2)
levels(Form_funcabun$Size2)
table(Form_funcabun$Size2)
Form_funcabun$Trophic.level <- as.factor(Form_funcabun$Trophic.level)
levels(Form_funcabun$Trophic.level)
table(Form_funcabun$Trophic.level)
Form_funcabun$Nest_site <- as.factor(Form_funcabun$Nest_site)
levels(Form_funcabun$Nest_site)
table(Form_funcabun$Nest_site)
Form_funcabun$Colony_size <- as.factor(Form_funcabun$Colony_size2)
levels(Form_funcabun$Colony_size)
table(Form_funcabun$Colony_size)
Form_funcabun$Foraging_style <- as.factor(Form_funcabun$Foraging_style)
levels(Form_funcabun$Foraging_style)
table(Form_funcabun$Foraging_style)
Form_funcabun$Size_Trophic <- paste(Form_funcabun$Size2,Form_funcabun$Trophic.level,sep="_")
Form_funcabun$Size_Trophic <- as.factor(Form_funcabun$Size_Trophic)
levels(Form_funcabun$Size_Trophic)
table(Form_funcabun$Size_Trophic)
Form_funcabun$Size_Nest <- paste(Form_funcabun$Size2,Form_funcabun$Nest_site,sep="_")
Form_funcabun$Size_Nest <- as.factor(Form_funcabun$Size_Nest)
levels(Form_funcabun$Size_Nest)
table(Form_funcabun$Size_Nest)
Form_funcabun$Size_Colony <- paste(Form_funcabun$Size2,Form_funcabun$Colony_size2,sep="_")
Form_funcabun$Size_Colony <- as.factor(Form_funcabun$Size_Colony)
levels(Form_funcabun$Size_Colony)
table(Form_funcabun$Size_Colony)
Form_funcabun$Size_Fstyle <- paste(Form_funcabun$Size2,Form_funcabun$Foraging_style,sep="_")
Form_funcabun$Size_Fstyle <- as.factor(Form_funcabun$Size_Fstyle)
levels(Form_funcabun$Size_Fstyle)
table(Form_funcabun$Size_Fstyle)
Form_funcabun$Trophic_Nest <- paste(Form_funcabun$Trophic.level,Form_funcabun$Nest_site,sep="_")
Form_funcabun$Trophic_Nest <- as.factor(Form_funcabun$Trophic_Nest)
levels(Form_funcabun$Trophic_Nest)
table(Form_funcabun$Trophic_Nest)
Form_funcabun$Trophic_Colony <- paste(Form_funcabun$Trophic.level,Form_funcabun$Colony_size2,sep="_")
Form_funcabun$Trophic_Colony <- as.factor(Form_funcabun$Trophic_Colony)
levels(Form_funcabun$Trophic_Colony)
table(Form_funcabun$Trophic_Colony)
Form_funcabun$Trophic_Fstyle <- paste(Form_funcabun$Trophic.level,Form_funcabun$Foraging_style,sep="_")
Form_funcabun$Trophic_Fstyle <- as.factor(Form_funcabun$Trophic_Fstyle)
levels(Form_funcabun$Trophic_Fstyle)
table(Form_funcabun$Trophic_Fstyle)
Form_funcabun$Nest_Colony <- paste(Form_funcabun$Nest_site,Form_funcabun$Colony_size2,sep="_")
Form_funcabun$Nest_Colony <- as.factor(Form_funcabun$Nest_Colony)
levels(Form_funcabun$Nest_Colony)
table(Form_funcabun$Nest_Colony)
Form_funcabun$Nest_Fstyle <- paste(Form_funcabun$Nest_site,Form_funcabun$Foraging_style,sep="_")
Form_funcabun$Nest_Fstyle <- as.factor(Form_funcabun$Nest_Fstyle)
levels(Form_funcabun$Nest_Fstyle)
table(Form_funcabun$Nest_Fstyle)
Form_funcabun$Colony_Fstyle <- paste(Form_funcabun$Colony_size2,Form_funcabun$Foraging_style,sep="_")
Form_funcabun$Colony_Fstyle <- as.factor(Form_funcabun$Colony_Fstyle)
levels(Form_funcabun$Colony_Fstyle)
table(Form_funcabun$Colony_Fstyle)

head(Form_funcabun);dim(Form_funcabun)
Form_sited <- Form2[,1:6]
head(Form_sited);dim(Form_sited)
Form_spd <- Form2[,which(colnames(Form2)=="Amblyoponinae1"):ncol(Form2)]
head(Form_spd[,1:6])

Big <- Form_funcabun$Morphospecies[which(Form_funcabun$Size2=="Big")]
Big_d <- Form_spd[,which(colnames(Form_spd) %in% Big)]
head(Big_d);dim(Big_d)
Big_sum <- rowSums(Big_d)
length(Big_sum)

Small <- Form_funcabun$Morphospecies[which(Form_funcabun$Size2=="Small")]
Small_d <- Form_spd[,which(colnames(Form_spd) %in% Small)]
head(Small_d);dim(Small_d)
Small_sum <- rowSums(Small_d)
length(Small_sum)

Carnivore <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic.level=="Carnivore")]
Carnivore_d <- Form_spd[,which(colnames(Form_spd) %in% Carnivore)]
head(Carnivore_d);dim(Carnivore_d)
Carnivore_sum <- rowSums(Carnivore_d)
length(Carnivore_sum)

Omnivore <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic.level=="Omnivore")]
Omnivore_d <- Form_spd[,which(colnames(Form_spd) %in% Omnivore)]
head(Omnivore_d);dim(Omnivore_d)
Omnivore_sum <- rowSums(Omnivore_d)
length(Omnivore_sum)

Herbivore <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic.level=="Herbivore")]
Herbivore_d <- Form_spd[,which(colnames(Form_spd) %in% Herbivore)]
head(Herbivore_d);dim(Herbivore_d)
Herbivore_sum <- rowSums(Herbivore_d)
length(Herbivore_sum)

Mixed <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_site=="Mixed")]
Mixed_d <- Form_spd[,which(colnames(Form_spd) %in% Mixed)]
head(Mixed_d);dim(Mixed_d)
Mixed_sum <- rowSums(Mixed_d)
length(Mixed_sum)

Soil <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_site=="Soil")]
Soil_d <- Form_spd[,which(colnames(Form_spd) %in% Soil)]
head(Soil_d);dim(Soil_d)
Soil_sum <- rowSums(Soil_d)
length(Soil_sum)

Trees <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_site=="Trees")]
Trees_d <- Form_spd[,which(colnames(Form_spd) %in% Trees)]
head(Trees_d);dim(Trees_d)
Trees_sum <- Trees_d
length(Trees_sum)

Few <- Form_funcabun$Morphospecies[which(Form_funcabun$Colony_size2=="Few")]
Few_d <- Form_spd[,which(colnames(Form_spd) %in% Few)]
head(Few_d);dim(Few_d)
Few_sum <- rowSums(Few_d)
length(Few_sum)

Many <- Form_funcabun$Morphospecies[which(Form_funcabun$Colony_size2=="Many")]
Many_d <- Form_spd[,which(colnames(Form_spd) %in% Many)]
head(Many_d);dim(Many_d)
Many_sum <- rowSums(Many_d)
length(Many_sum)

Cooperative <- Form_funcabun$Morphospecies[which(Form_funcabun$Foraging_style=="Cooperative")]
Cooperative_d <- Form_spd[,which(colnames(Form_spd) %in% Cooperative)]
head(Cooperative_d);dim(Cooperative_d)
Cooperative_sum <- rowSums(Cooperative_d)
length(Cooperative_sum)

Solitary <- Form_funcabun$Morphospecies[which(Form_funcabun$Foraging_style=="Solitary")]
Solitary_d <- Form_spd[,which(colnames(Form_spd) %in% Solitary)]
head(Solitary_d);dim(Solitary_d)
Solitary_sum <- rowSums(Solitary_d)
length(Solitary_sum)

#Mixed traits

Big_carn <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Trophic=="Big_Carnivore")]
Big_carn_d <- Form_spd[,which(colnames(Form_spd) %in% Big_carn)]
head(Big_carn_d);dim(Big_carn_d)
Big_carn_sum <- rowSums(Big_carn_d)
length(Big_carn_sum)

Big_omn <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Trophic=="Big_Omnivore")]
Big_omn_d <- Form_spd[,which(colnames(Form_spd) %in% Big_omn)]
head(Big_omn_d);dim(Big_omn_d)
Big_omn_sum <- rowSums(Big_omn_d)
length(Big_omn_sum)

Small_carn <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Trophic=="Small_Carnivore")]
Small_carn_d <- Form_spd[,which(colnames(Form_spd) %in% Small_carn)]
head(Small_carn_d);dim(Small_carn_d)
Small_carn_sum <- rowSums(Small_carn_d)
length(Small_carn_sum)

Small_herb <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Trophic=="Small_Herbivore")]
Small_herb_d <- Form_spd[,which(colnames(Form_spd) %in% Small_herb)]
head(Small_herb_d);dim(Small_herb_d)
Small_herb_sum <- rowSums(Small_herb_d)
length(Small_herb_sum)

Small_omn <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Trophic=="Small_Omnivore")]
Small_omn_d <- Form_spd[,which(colnames(Form_spd) %in% Small_omn)]
head(Small_omn_d);dim(Small_omn_d)
Small_omn_sum <- rowSums(Small_omn_d)
length(Small_omn_sum)

Big_mixed <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Nest=="Big_Mixed")]
Big_mixed_d <- Form_spd[,which(colnames(Form_spd) %in% Big_mixed)]
head(Big_mixed_d);dim(Big_mixed_d)
Big_mixed_sum <- rowSums(Big_mixed_d)
length(Big_mixed_sum)

Big_soil <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Nest=="Big_Soil")]
Big_soil_d <- Form_spd[,which(colnames(Form_spd) %in% Big_soil)]
head(Big_soil_d);dim(Big_soil_d)
Big_soil_sum <- rowSums(Big_soil_d)
length(Big_soil_sum)

Small_mixed <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Nest=="Small_Mixed")]
Small_mixed_d <- Form_spd[,which(colnames(Form_spd) %in% Small_mixed)]
head(Small_mixed_d);dim(Small_mixed_d)
Small_mixed_sum <- rowSums(Small_mixed_d)
length(Small_mixed_sum)

Small_soil <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Nest=="Small_Soil")]
Small_soil_d <- Form_spd[,which(colnames(Form_spd) %in% Small_soil)]
head(Small_soil_d);dim(Small_soil_d)
Small_soil_sum <- rowSums(Small_soil_d)
length(Small_soil_sum)

Small_trees <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Nest=="Small_Trees")]
Small_trees_d <- Form_spd[,which(colnames(Form_spd) %in% Small_trees)]
head(Small_trees_d);dim(Small_trees_d)
Small_trees_sum <- Small_trees_d
length(Small_trees_sum)

Big_few <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Colony=="Big_Few")]
Big_few_d <- Form_spd[,which(colnames(Form_spd) %in% Big_few)]
head(Big_few_d);dim(Big_few_d)
Big_few_sum <- Big_few_d
length(Big_few_sum)

Big_many <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Colony=="Big_Many")]
Big_many_d <- Form_spd[,which(colnames(Form_spd) %in% Big_many)]
head(Big_many_d);dim(Big_many_d)
Big_many_sum <- rowSums(Big_many_d)
length(Big_many_sum)

Small_few <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Colony=="Small_Few")]
Small_few_d <- Form_spd[,which(colnames(Form_spd) %in% Small_few)]
head(Small_few_d);dim(Small_few_d)
Small_few_sum <- rowSums(Small_few_d)
length(Small_few_sum)

Small_many <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Colony=="Small_Many")]
Small_many_d <- Form_spd[,which(colnames(Form_spd) %in% Small_many)]
head(Small_many_d);dim(Small_many_d)
Small_many_sum <- rowSums(Small_many_d)
length(Small_many_sum)

Big_coop <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Fstyle=="Big_Cooperative")]
Big_coop_d <- Form_spd[,which(colnames(Form_spd) %in% Big_coop)]
head(Big_coop_d);dim(Big_coop_d)
Big_coop_sum <- rowSums(Big_coop_d)
length(Big_coop_sum)

Big_solitary <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Fstyle=="Big_Solitary")]
Big_solitary_d <- Form_spd[,which(colnames(Form_spd) %in% Big_solitary)]
head(Big_solitary_d);dim(Big_solitary_d)
Big_solitary_sum <- Big_solitary_d
length(Big_solitary_sum)

Small_coop <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Fstyle=="Small_Cooperative")]
Small_coop_d <- Form_spd[,which(colnames(Form_spd) %in% Small_coop)]
head(Small_coop_d);dim(Small_coop_d)
Small_coop_sum <- rowSums(Small_coop_d)
length(Small_coop_sum)

Small_solitary <- Form_funcabun$Morphospecies[which(Form_funcabun$Size_Fstyle=="Small_Solitary")]
Small_solitary_d <- Form_spd[,which(colnames(Form_spd) %in% Small_solitary)]
head(Small_solitary_d);dim(Small_solitary_d)
Small_solitary_sum <- rowSums(Small_solitary_d)
length(Small_solitary_sum)

Carn_mixed <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Nest=="Carnivore_Mixed")]
Carn_mixed_d <- Form_spd[,which(colnames(Form_spd) %in% Carn_mixed)]
head(Carn_mixed_d);dim(Carn_mixed_d)
Carn_mixed_sum <- rowSums(Carn_mixed_d)
length(Carn_mixed_sum)

Carn_soil <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Nest=="Carnivore_Soil")]
Carn_soil_d <- Form_spd[,which(colnames(Form_spd) %in% Carn_soil)]
head(Carn_soil_d);dim(Carn_soil_d)
Carn_soil_sum <- rowSums(Carn_soil_d)
length(Carn_soil_sum)

Herb_mixed <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Nest=="Herbivore_Mixed")]
Herb_mixed_d <- Form_spd[,which(colnames(Form_spd) %in% Herb_mixed)]
head(Herb_mixed_d);dim(Herb_mixed_d)
Herb_mixed_sum <- rowSums(Herb_mixed_d)
length(Herb_mixed_sum)

Omn_mixed <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Nest=="Omnivore_Mixed")]
Omn_mixed_d <- Form_spd[,which(colnames(Form_spd) %in% Omn_mixed)]
head(Omn_mixed_d);dim(Omn_mixed_d)
Omn_mixed_sum <- rowSums(Omn_mixed_d)
length(Omn_mixed_sum)

Omn_soil <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Nest=="Omnivore_Soil")]
Omn_soil_d <- Form_spd[,which(colnames(Form_spd) %in% Omn_soil)]
head(Omn_soil_d);dim(Omn_soil_d)
Omn_soil_sum <- rowSums(Omn_soil_d)
length(Omn_soil_sum)

Omn_trees <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Nest=="Omnivore_Trees")]
Omn_trees_d <- Form_spd[,which(colnames(Form_spd) %in% Omn_trees)]
head(Omn_trees_d);dim(Omn_trees_d)
Omn_trees_sum <- Omn_trees_d
length(Omn_trees_sum)

Carn_few <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Colony=="Carnivore_Few")]
Carn_few_d <- Form_spd[,which(colnames(Form_spd) %in% Carn_few)]
head(Carn_few_d);dim(Carn_few_d)
Carn_few_sum <- Carn_few_d
length(Carn_few_sum)

Carn_many <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Colony=="Carnivore_Many")]
Carn_many_d <- Form_spd[,which(colnames(Form_spd) %in% Carn_many)]
head(Carn_many_d);dim(Carn_many_d)
Carn_many_sum <- rowSums(Carn_many_d)
length(Carn_many_sum)

Omn_few <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Colony=="Omnivore_Few")]
Omn_few_d <- Form_spd[,which(colnames(Form_spd) %in% Omn_few)]
head(Omn_few_d);dim(Omn_few_d)
Omn_few_sum <- rowSums(Omn_few_d)
length(Omn_few_sum)

Omn_many <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Colony=="Omnivore_Many")]
Omn_many_d <- Form_spd[,which(colnames(Form_spd) %in% Omn_many)]
head(Omn_many_d);dim(Omn_many_d)
Omn_many_sum <- rowSums(Omn_many_d)
length(Omn_many_sum)

Carn_coop <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Fstyle=="Carnivore_Cooperative")]
Carn_coop_d <- Form_spd[,which(colnames(Form_spd) %in% Carn_coop)]
head(Carn_coop_d);dim(Carn_coop_d)
Carn_coop_sum <- rowSums(Carn_coop_d)
length(Carn_coop_sum)

Carn_solitary <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Fstyle=="Carnivore_Solitary")]
Carn_solitary_d <- Form_spd[,which(colnames(Form_spd) %in% Carn_solitary)]
head(Carn_solitary_d);dim(Carn_solitary_d)
Carn_solitary_sum <- rowSums(Carn_solitary_d)
length(Carn_solitary_sum)

Omn_coop <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Fstyle=="Omnivore_Cooperative")]
Omn_coop_d <- Form_spd[,which(colnames(Form_spd) %in% Omn_coop)]
head(Omn_coop_d);dim(Omn_coop_d)
Omn_coop_sum <- rowSums(Omn_coop_d)
length(Omn_coop_sum)

Omn_solitary <- Form_funcabun$Morphospecies[which(Form_funcabun$Trophic_Fstyle=="Omnivore_Solitary")]
Omn_solitary_d <- Form_spd[,which(colnames(Form_spd) %in% Omn_solitary)]
head(Omn_solitary_d);dim(Omn_solitary_d)
Omn_solitary_sum <- rowSums(Omn_solitary_d)
length(Omn_solitary_sum)

Mixed_few <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_Colony=="Mixed_Few")]
Mixed_few_d <- Form_spd[,which(colnames(Form_spd) %in% Mixed_few)]
head(Mixed_few_d);dim(Mixed_few_d)
Mixed_few_sum <- Mixed_few_d
length(Mixed_few_sum)

Mixed_many <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_Colony=="Mixed_Many")]
Mixed_many_d <- Form_spd[,which(colnames(Form_spd) %in% Mixed_many)]
head(Mixed_many_d);dim(Mixed_many_d)
Mixed_many_sum <- rowSums(Mixed_many_d)
length(Mixed_many_sum)

Soil_few <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_Colony=="Soil_Few")]
Soil_few_d <- Form_spd[,which(colnames(Form_spd) %in% Soil_few)]
head(Soil_few_d);dim(Soil_few_d)
Soil_few_sum <- rowSums(Soil_few_d)
length(Soil_few_sum)

Soil_many <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_Colony=="Soil_Many")]
Soil_many_d <- Form_spd[,which(colnames(Form_spd) %in% Soil_many)]
head(Soil_many_d);dim(Soil_many_d)
Soil_many_sum <- rowSums(Soil_many_d)
length(Soil_many_sum)

Trees_many <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_Colony=="Trees_Many")]
Trees_many_d <- Form_spd[,which(colnames(Form_spd) %in% Trees_many)]
head(Trees_many_d);dim(Trees_many_d)
Trees_many_sum <- Trees_many_d
length(Trees_many_sum)

Mixed_coop <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_Fstyle=="Mixed_Cooperative")]
Mixed_coop_d <- Form_spd[,which(colnames(Form_spd) %in% Mixed_coop)]
head(Mixed_coop_d);dim(Mixed_coop_d)
Mixed_coop_sum <- rowSums(Mixed_coop_d)
length(Mixed_coop_sum)

Mixed_solitary <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_Fstyle=="Mixed_Solitary")]
Mixed_solitary_d <- Form_spd[,which(colnames(Form_spd) %in% Mixed_solitary)]
head(Mixed_solitary_d);dim(Mixed_solitary_d)
Mixed_solitary_sum <- rowSums(Mixed_solitary_d)
length(Mixed_solitary_sum)

Soil_coop <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_Fstyle=="Soil_Cooperative")]
Soil_coop_d <- Form_spd[,which(colnames(Form_spd) %in% Soil_coop)]
head(Soil_coop_d);dim(Soil_coop_d)
Soil_coop_sum <- rowSums(Soil_coop_d)
length(Soil_coop_sum)

Soil_solitary <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_Fstyle=="Soil_Solitary")]
Soil_solitary_d <- Form_spd[,which(colnames(Form_spd) %in% Soil_solitary)]
head(Soil_solitary_d);dim(Soil_solitary_d)
Soil_solitary_sum <- rowSums(Soil_solitary_d)
length(Soil_solitary_sum)

Trees_coop <- Form_funcabun$Morphospecies[which(Form_funcabun$Nest_Fstyle=="Trees_Cooperative")]
Trees_coop_d <- Form_spd[,which(colnames(Form_spd) %in% Trees_coop)]
head(Trees_coop_d);dim(Trees_coop_d)
Trees_coop_sum <- Trees_coop_d
length(Trees_coop_sum)

Few_coop <- Form_funcabun$Morphospecies[which(Form_funcabun$Colony_Fstyle=="Few_Cooperative")]
Few_coop_d <- Form_spd[,which(colnames(Form_spd) %in% Few_coop)]
head(Few_coop_d);dim(Few_coop_d)
Few_coop_sum <- Few_coop_d
length(Few_coop_sum)

Few_solitary <- Form_funcabun$Morphospecies[which(Form_funcabun$Colony_Fstyle=="Few_Solitary")]
Few_solitary_d <- Form_spd[,which(colnames(Form_spd) %in% Few_solitary)]
head(Few_solitary_d);dim(Few_solitary_d)
Few_solitary_sum <- rowSums(Few_solitary_d)
length(Few_solitary_sum)

Many_coop <- Form_funcabun$Morphospecies[which(Form_funcabun$Colony_Fstyle=="Many_Cooperative")]
Many_coop_d <- Form_spd[,which(colnames(Form_spd) %in% Many_coop)]
head(Many_coop_d);dim(Many_coop_d)
Many_coop_sum <- rowSums(Many_coop_d)
length(Many_coop_sum)

Many_solitary <- Form_funcabun$Morphospecies[which(Form_funcabun$Colony_Fstyle=="Many_Solitary")]
Many_solitary_d <- Form_spd[,which(colnames(Form_spd) %in% Many_solitary)]
head(Many_solitary_d);dim(Many_solitary_d)
Many_solitary_sum <- rowSums(Many_solitary_d)
length(Many_solitary_sum)

form_groups <- cbind(Form_sited,data.frame(Big=Big_sum),data.frame(Small=Small_sum),data.frame(Carnivore=Carnivore_sum),data.frame(Herbivore=Herbivore_sum),data.frame(Omnivore=Omnivore_sum),data.frame(Mixed=Mixed_sum),data.frame(Soil=Soil_sum),data.frame(Trees=Trees_sum),data.frame(Few=Few_sum),data.frame(Many=Many_sum),data.frame(Cooperative=Cooperative_sum),data.frame(Solitary=Solitary_sum),data.frame(Big_carn=Big_carn_sum),data.frame(Big_omn=Big_omn_sum),data.frame(Small_carn=Small_carn_sum),data.frame(Small_herb=Small_herb_sum),data.frame(Small_omn=Small_omn_sum),data.frame(Big_mixed=Big_mixed_sum),data.frame(Big_soil=Big_soil_sum),data.frame(Small_mixed=Small_mixed_sum),data.frame(Small_soil=Small_soil_sum),data.frame(Small_trees=Small_trees_sum),data.frame(Big_few=Big_few_sum),data.frame(Big_many=Big_many_sum),data.frame(Small_few=Small_few_sum),data.frame(Small_many=Small_many_sum),data.frame(Big_coop=Big_coop_sum),data.frame(Big_solitary=Big_solitary_sum),data.frame(Small_coop=Small_coop_sum),data.frame(Small_solitary=Small_solitary_sum),data.frame(Carn_mixed=Carn_mixed_sum),data.frame(Carn_soil=Carn_soil_sum),data.frame(Herb_mixed=Herb_mixed_sum),data.frame(Omn_mixed=Omn_mixed_sum),data.frame(Omn_soil=Omn_soil_sum),data.frame(Omn_trees=Omn_trees_sum),data.frame(Carn_few=Carn_few_sum),data.frame(Carn_many=Carn_many_sum),data.frame(Omn_few=Omn_few_sum),data.frame(Omn_many=Omn_many_sum),data.frame(Carn_coop=Carn_coop_sum),data.frame(Carn_solitary=Carn_solitary_sum),data.frame(Omn_coop=Omn_coop_sum),data.frame(Omn_solitary=Omn_solitary_sum),data.frame(Mixed_few=Mixed_few_sum),data.frame(Mixed_many=Mixed_many_sum),data.frame(Soil_few=Soil_few_sum),data.frame(Soil_many=Soil_many_sum),data.frame(Trees_many=Trees_many_sum),data.frame(Mixed_coop=Mixed_coop_sum),data.frame(Mixed_solitary=Mixed_solitary_sum),data.frame(Soil_coop=Soil_coop_sum),data.frame(Soil_solitary=Soil_solitary_sum),data.frame(Trees_coop=Trees_coop_sum),data.frame(Few_coop=Trees_coop_sum),data.frame(Few_solitary=Few_solitary_sum),data.frame(Many_coop=Many_coop_sum),data.frame(Many_solitary=Many_solitary_sum))
form_groups$Yr <- form_groups$Year-min(form_groups$Year)
form_groups <- form_groups[,c(1:6,which(colnames(form_groups)=="Yr"),which(colnames(form_groups)=="Big"):which(colnames(form_groups)=="Many_solitary"))]
form_groups$Site <- as.factor(form_groups$Site)
form_groups$Plot <- as.factor(form_groups$Plot)
form_groups$Treatment <- as.factor(form_groups$Treatment)
form_groups$Replicate <- as.factor(form_groups$Replicate)
head(form_groups);dim(form_groups)

head(Form_funcabun);dim(Form_funcabun)
Form_funcabun[Form_funcabun$Size2=='Big',]

form_groups_summary1 <- apply(form_groups[,which(colnames(form_groups)=="Big"):ncol(form_groups)],MARGIN = 2,FUN=function(x)table(x==0)[2]/sum(table(x==0)))
#Getting NA's in the summary dataset - specifically in groups where there are no zeros
#This is resulting in models not running for groups where there are no zeros
formgroups_summary <- data.frame(group=names(form_groups_summary1),propzero=form_groups_summary1)
rownames(formgroups_summary) <- 1:nrow(formgroups_summary)
formgroups_summary[is.na(formgroups_summary)] = 0
#table(form_groups$big==0)[2]/sum(table(form_groups$big==0))

formgroups_summary$abun <- apply(form_groups[,which(colnames(form_groups)=="Big"):ncol(form_groups)],MARGIN = 2,FUN=function(x)sum(x))
formgroups_summary$fit_binom <- ifelse(formgroups_summary$propzero<0.2|formgroups_summary$propzero>0.9,'no','yes')
formgroups_summary$fit_abun <- ifelse(formgroups_summary$abun<80,'no','yes')

#Removes groups that have less than the number of plots per reserve (16), update to 80 to remove groups with less than the total number of plots per year
head(formgroups_summary);dim(formgroups_summary)
formgroups_summary <- formgroups_summary[-which(formgroups_summary$abun<16),]
formgroups_summary<-droplevels(formgroups_summary)
rownames(formgroups_summary)<-1:nrow(formgroups_summary)

#Close summary data ----

head(form_groups);dim(form_groups)
form_gr <- colnames(form_groups)[which(colnames(form_groups)=="Big"):ncol(form_groups)]
formgroups_summary
formgroups_summary$p_binom_int <- NA
formgroups_summary$p_abun_int <- NA
formgroup_binom <- list()
formgroup_bicoef <- list()
formgroup_bipred <- list()
formgroup_abun <- list()
formgroup_abcoef <- list()
formgroup_abpred <- list()

for(i in 1:length(form_gr)){
  group_this_run <- form_gr[i]
  sum_this_run <- formgroups_summary[which(formgroups_summary$group==group_this_run),]
  dat_this_run <- form_groups[,c(1:7,which(colnames(form_groups)==group_this_run))]
  dg_this_run <- dat_this_run[,which(colnames(dat_this_run)==group_this_run)]
  head(dat_this_run);dim(dat_this_run)
  #place the remove site code here - issue with models running after sites removed
  
  #close remove sites
  
  if(sum_this_run$fit_binom=='yes'){
    dg_binom <- ifelse(dg_this_run>0,1,0)
    binom_this_run <- glmer(dg_binom ~ Treatment+Yr+Treatment:Yr+(1|Site/Replicate),family=binomial,data=dat_this_run)
    binom_sum<-summary(binom_this_run)$coefficients
    formgroup_binom[[i]]<-binom_this_run
    formgroup_bicoef[[i]]<-binom_sum
    formgroups_summary$p_binom_int[i]<-round(binom_sum[which(rownames(binom_sum)=='TreatmentRock:Yr'),which(colnames(binom_sum)=='Pr(>|z|)')],4)
    
    binom_nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(dat_this_run$Treatment),2),levels=levels(dat_this_run$Treatment)))
    binom_pr <- predictSE(mod=binom_this_run,newdata=binom_nd,se.fit=T,type='link')
    binom_pr<-data.frame(binom_nd, fit.link=binom_pr$fit, se=binom_pr$se.fit)
    binom_pr$lci.link<-binom_pr$fit-(1.96*binom_pr$se)
    binom_pr$uci.link<-binom_pr$fit+(1.96*binom_pr$se)
    binom_pr$fit.resp<-invlogit(binom_pr$fit.link)
    binom_pr$lci.resp<-invlogit(binom_pr$lci.link)
    binom_pr$uci.resp<-invlogit(binom_pr$uci.link)
    binom_pr
    formgroup_bipred[[i]]<-binom_pr
    
  } #close fit_binom
  
  if(sum_this_run$fit_abun=='yes'){
    abun_this_run<-glmmadmb(dg_this_run~Treatment+Yr+Treatment:Yr+(1|Site/Replicate), family="nbinom", data=dat_this_run,admb.opts=admbControl(impSamp=200,maxfn=5000,imaxfn=1000,maxph=5,shess=FALSE,noinit=FALSE))
    abun_sum<-summary(abun_this_run)$coefficients
    formgroup_abun[[i]]<-abun_this_run
    formgroup_abcoef[[i]]<-abun_sum
    formgroups_summary$p_abun_int[i]<-round(abun_sum[which(rownames(abun_sum)=='TreatmentRock:Yr'),which(colnames(abun_sum)=='Pr(>|z|)')],4)
    
    abun_nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(dat_this_run$Treatment),2),levels=levels(dat_this_run$Treatment)))
    abun_pr <- predict(abun_this_run,newdata=abun_nd,se.fit=T,type='link')
    abun_pr<-data.frame(abun_nd, fit.link=abun_pr$fit, se=abun_pr$se.fit)
    abun_pr$lci.link<-abun_pr$fit-(1.96*abun_pr$se)
    abun_pr$uci.link<-abun_pr$fit+(1.96*abun_pr$se)
    abun_pr$fit.resp<-exp(abun_pr$fit.link)
    abun_pr$lci.resp<-exp(abun_pr$lci.link)
    abun_pr$uci.resp<-exp(abun_pr$uci.link)
    abun_pr
    formgroup_abpred[[i]]<-abun_pr
  } #close fit_abun
  
} #close i

#Remove sites without data (if needed)
site_sum<-tapply(dat_this_run[,which(colnames(dat_this_run)==group_this_run)],INDEX=dat_this_run$Site,FUN=sum)
site_to_remove<-names(which(site_sum==0))
if(length(site_to_remove)>0){
  dat_this_run<-dat_this_run[-which(dat_this_run$Site %in% site_to_remove),]
  dat_this_run<-droplevels(dat_this_run)
  rownames(dat_this_run)<-1:nrow(dat_this_run)}

formgroup_abun
summary(formgroup_abun[[14]])
formgroup_abcoef
summary(formgroup_abcoef[[1]])
formgroup_abpred
summary(formgroup_abpred[[1]])

formgroup_binom
summary(formgroup_binom[[15]])
formgroup_bicoef
formgroup_bipred

#Contasts ----
#Binomial contrasts
form_bisignif <- data.frame(group=formgroups_summary$group[which(formgroups_summary$p_binom_int<0.05)])
dim(formgroups_summary)
form_bisignif$index <- which(formgroups_summary$p_binom_int<0.05)
small_carncoeff <- formgroup_bicoef[[form_bisignif$index[which(form_bisignif$group=='Small_carn')]]]
carn_soilcoeff <- formgroup_bicoef[[form_bisignif$index[which(form_bisignif$group=='Carn_soil')]]]
small_carnmod <- formgroup_binom[[form_bisignif$index[which(form_bisignif$group=='Small_carn')]]]
carn_soilmod <- formgroup_binom[[form_bisignif$index[which(form_bisignif$group=='Carn_soil')]]]

#Abundance contrasts
form_absignif <- data.frame(group=formgroups_summary$group[which(formgroups_summary$p_abun_int<0.05)])
dim(formgroups_summary)
form_absignif$index <- which(formgroups_summary$p_abun_int<0.05)
big_coeff <- formgroup_abcoef[[form_absignif$index[which(form_absignif$group=='Big')]]]
bc_coeff <- formgroup_abcoef[[form_absignif$index[which(form_absignif$group=='Big_carn')]]]
bo_coeff <- formgroup_abcoef[[form_absignif$index[which(form_absignif$group=='Big_omn')]]]
bmix_coeff <- formgroup_abcoef[[form_absignif$index[which(form_absignif$group=='Big_mixed')]]]
bmany_coeff <- formgroup_abcoef[[form_absignif$index[which(form_absignif$group=='Big_many')]]]
bcoop_coeff <- formgroup_abcoef[[form_absignif$index[which(form_absignif$group=='Big_coop')]]]

big_mod <- formgroup_abun[[form_absignif$index[which(form_absignif$group=='Big')]]]
bc_mod <- formgroup_abun[[form_absignif$index[which(form_absignif$group=='Big_carn')]]]
bo_mod <- formgroup_abun[[form_absignif$index[which(form_absignif$group=='Big_omn')]]]
bmix_mod <- formgroup_abun[[form_absignif$index[which(form_absignif$group=='Big_mixed')]]]
bmany_mod <- formgroup_abun[[form_absignif$index[which(form_absignif$group=='Big_many')]]]
bcoop_mod <- formgroup_abun[[form_absignif$index[which(form_absignif$group=='Big_coop')]]]

#There are 6 contrasts for four categories (c16:r16,c16:c19, C16:r19, r16:c19, r16:r19, c19:r19)
form_c<-data.frame(Year=rep(unique(form_groups$Year)[order(unique(form_groups$Year))],rep(2,2)),Treatment=c('C','R'))
form_c$Year_Treatment <- paste(form_c$Year,form_c$Treatment,sep='_')
form_contrast<-data.frame(contrast=paste(combn(form_c$Year_Treatment,2)[1,],combn(form_c$Year_Treatment,2)[2,],sep=':'))

#Create unique model matrix
mm_form <- lm(dg_binom ~ Treatment+Yr+Treatment:Yr,data=form_groups,x=T)$x
umm_form <- unique(mm_form)

#WARNING NUMERIC SUBSETS - put them in the natural order 2016-2019, c to r
umm_form <- umm_form[c(4,3,2,1),]
rownames(umm_form) <- 1:nrow(umm_form)

#Create a difference matrix
#Each row must be a vector with a length equal to the number of rows in the unique model matrix (umm), e.g. four rows in umm_form matrix will give 6 contrasts. Each row will specify one contrast.
diffm_form <- rbind(
  c(-1,1,0,0),
  c(-1,0,1,0),
  c(-1,0,0,1),
  c(0,-1,1,0),
  c(0,-1,0,1),
  c(0,0,-1,1)
)

#Now we have a unique model matrix
umm_form

#and we have a difference matrix
diffm_form

#and we have the names for the contrast
form_contrast

#calculate the differences and CI's (binom) - when redone cs not significant
sc_diff<-data.frame(contrast=form_contrast,diff.est(model = small_carnmod,unique.mod.mat = umm_form,diff.matrix = diffm_form))
sc_diff$diff <- ifelse(sign(sc_diff$lci)==sign(sc_diff$uci),1,0)

cs_diff<-data.frame(contrast=form_contrast,diff.est(model = carn_soilmod,unique.mod.mat = umm_form,diff.matrix = diffm_form))
cs_diff$diff <- ifelse(sign(cs_diff$lci)==sign(cs_diff$uci),1,0)

dev.new(width=18,height=8,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,2),mar=c(5,5,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))

sc_plot<-formgroup_bipred[[15]]
plot(1:4,sc_plot$fit.resp,type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),ylim=c(0,1.2),las=1,cex=1.5,xaxt='n',yaxt='n',ylab='Probability of Occurance',xlab='Year',main=formgroups_summary$group[15],font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
axis(side=2,at=seq(0,1,by=0.2),las=1)
arrows(1:4,sc_plot$lci.resp,1:4,sc_plot$uci.resp,length=0.05,angle=90,code=3)
scp <- round(summary(formgroup_binom[[15]])$coefficients[3,'Pr(>|z|)'],3)
scp <- ifelse(scp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',scp,sep=''))
legend('bottomleft',legend = c(paste('Treat. P=',round(summary(formgroup_binom[[15]])$coefficients[2,'Pr(>|z|)'],3),sep=''),scp,paste('Int. P=',round(summary(formgroup_binom[[15]])$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
text(x=1:4, y=max(sc_plot$uci.resp)+0.4,labels=c(rep("ab"),rep("ab"),rep(letters[c(1)],1),rep(letters[c(2)],1)))
cs_plot<-formgroup_bipred[[32]]
plot(1:4,cs_plot$fit.resp,ylim=c(0,max(cs_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Probability of Occurance',xlab='Year',main=formgroups_summary$group[32],font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,cs_plot$lci.resp,1:4,cs_plot$uci.resp,length=0.05,angle=90,code=3)
csp <- round(summary(formgroup_binom[[32]])$coefficients[3,'Pr(>|z|)'],3)
csp <- ifelse(csp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',csp,sep=''))
legend('bottomleft',legend = c(paste('Treat. P=',round(summary(formgroup_binom[[32]])$coefficients[2,'Pr(>|z|)'],3),sep=''),csp,paste('Int. P=',round(summary(formgroup_binom[[32]])$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
text(x=1:4, y=max(cs_plot$uci.resp)+0.4,labels=c(rep("ab"),rep("ab"),rep(letters[c(1)],1),rep(letters[c(2)],1)))
par(xpd=NA)
legend(x=4.75, y=2.5, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#calculate the differences and CI's (abun)
big_diff<-data.frame(contrast=form_contrast,diff.est(model = big_mod,unique.mod.mat = umm_form,diff.matrix = diffm_form))
big_diff$diff <- ifelse(sign(big_diff$lci)==sign(big_diff$uci),1,0)

bc_diff<-data.frame(contrast=form_contrast,diff.est(model = bc_mod,unique.mod.mat = umm_form,diff.matrix = diffm_form))
bc_diff$diff <- ifelse(sign(bc_diff$lci)==sign(bc_diff$uci),1,0)

bo_diff<-data.frame(contrast=form_contrast,diff.est(model = bo_mod,unique.mod.mat = umm_form,diff.matrix = diffm_form))
bo_diff$diff <- ifelse(sign(bo_diff$lci)==sign(bo_diff$uci),1,0)

bmix_diff<-data.frame(contrast=form_contrast,diff.est(model = bmix_mod,unique.mod.mat = umm_form,diff.matrix = diffm_form))
bmix_diff$diff <- ifelse(sign(bmix_diff$lci)==sign(bmix_diff$uci),1,0)

bmany_diff<-data.frame(contrast=form_contrast,diff.est(model = bmany_mod,unique.mod.mat = umm_form,diff.matrix = diffm_form))
bmany_diff$diff <- ifelse(sign(bmany_diff$lci)==sign(bmany_diff$uci),1,0)

bcoop_diff<-data.frame(contrast=form_contrast,diff.est(model = bcoop_mod,unique.mod.mat = umm_form,diff.matrix = diffm_form))
bcoop_diff$diff <- ifelse(sign(bcoop_diff$lci)==sign(bcoop_diff$uci),1,0)

dev.new(width=6,height=4,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(1,1),mar=c(5,5,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))

for(i in abun.plot){
  
  pred.this.run<-formgroup_abpred[[1]]
  plot(1:4,pred.this.run$fit.resp,ylim=c(0,max(pred.this.run$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='',main=formgroups_summary$group[1],font.main=1)
  axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'),mgp=c(2,0.8,0))
  arrows(1:4,pred.this.run$lci.resp,1:4,pred.this.run$uci.resp,length=0.05,angle=90,code=3)
  text(x=1, y=(par('usr')[4])-par('usr')[4]/10,labels=paste('Int.p=',round(formgroups_summary$p_abun_int[1],3),sep=''),adj=0)
  title(xlab='Year',mgp=c(2,0.8,0))
  
} #close plot loop

par(xpd=NA)
legend(x=5, y=4, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Big plot (big, bo, bmix, bmany, bcoop), plot all five and add letters

dev.new(width=14,height=8,dpi=80,pointsize=20,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(5,5,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))

big_plot<-formgroup_abpred[[1]]
plot(1:4,big_plot$fit.resp,ylim=c(0,max(big_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main=formgroups_summary$group[1],font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,big_plot$lci.resp,1:4,big_plot$uci.resp,length=0.05,angle=90,code=3)
bigp <- round(summary(formgroup_abun[[1]])$coefficients[3,'Pr(>|z|)'],3)
bigp <- ifelse(bigp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',bigp,sep=''))
legend('topleft',inset=0.05,legend = c(paste('Treat. P=',round(summary(formgroup_abun[[1]])$coefficients[2,'Pr(>|z|)'],3),sep=''),bigp,paste('Int. P=',round(summary(formgroup_abun[[1]])$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
text(x=1:4, y=max(big_plot$uci.resp)+0.4,labels=c(rep(letters[c(1)],1),rep(letters[c(2)],3)))
bigomn_plot<-formgroup_abpred[[14]]
plot(1:4,bigomn_plot$fit.resp,ylim=c(0,max(bigomn_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main=formgroups_summary$group[14],font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,bigomn_plot$lci.resp,1:4,bigomn_plot$uci.resp,length=0.05,angle=90,code=3)
bop <- round(summary(formgroup_abun[[14]])$coefficients[3,'Pr(>|z|)'],3)
bop <- ifelse(bop<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',bop,sep=''))
legend('topleft',inset=0.05,legend = c(paste('Treat. P=',round(summary(formgroup_abun[[14]])$coefficients[2,'Pr(>|z|)'],3),sep=''),bop,paste('Int. P=',round(summary(formgroup_abun[[14]])$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
text(x=1:4, y=max(bigomn_plot$uci.resp)+0.4,labels=c(rep(letters[c(1)],1),rep(letters[c(2)],3)))
bigmany_plot<-formgroup_abpred[[24]]
plot(1:4,bigmany_plot$fit.resp,ylim=c(0,max(bigmany_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main=formgroups_summary$group[24],font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,bigmany_plot$lci.resp,1:4,bigmany_plot$uci.resp,length=0.05,angle=90,code=3)
bmp <- round(summary(formgroup_abun[[24]])$coefficients[3,'Pr(>|z|)'],3)
bmp <- ifelse(bmp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',bmp,sep=''))
legend('topleft',inset=0.05,legend = c(paste('Treat. P=',round(summary(formgroup_abun[[24]])$coefficients[2,'Pr(>|z|)'],3),sep=''),bmp,paste('Int. P=',round(summary(formgroup_abun[[24]])$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
text(x=1:4, y=max(bigmany_plot$uci.resp)+0.4,labels=c(rep(letters[c(1)],1),rep(letters[c(2)],3)))

par(xpd=NA)
legend(x=5, y=6, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

bigmix_plot<-formgroup_abpred[[18]]
plot(1:4,bigmix_plot$fit.resp,ylim=c(0,max(bigmix_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main=formgroups_summary$group[18],font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,bigmix_plot$lci.resp,1:4,bigmix_plot$uci.resp,length=0.05,angle=90,code=3)
bmxp <- round(summary(formgroup_abun[[18]])$coefficients[3,'Pr(>|z|)'],3)
bmxp <- ifelse(bmxp<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',bmxp,sep=''))
legend('topleft',inset=0.05,legend = c(paste('Treat. P=',round(summary(formgroup_abun[[18]])$coefficients[2,'Pr(>|z|)'],3),sep=''),bmxp,paste('Int. P=',round(summary(formgroup_abun[[18]])$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
text(x=1:4, y=max(bigmix_plot$uci.resp)+0.4,labels=c(rep(letters[c(1)],1),rep(letters[c(2)],3)))
bigcoop_plot<-formgroup_abpred[[27]]
plot(1:4,bigcoop_plot$fit.resp,ylim=c(0,max(bigcoop_plot$uci.resp)+0.5),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='Year',main=formgroups_summary$group[27],font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,bigcoop_plot$lci.resp,1:4,bigcoop_plot$uci.resp,length=0.05,angle=90,code=3)
bcop <- round(summary(formgroup_abun[[27]])$coefficients[3,'Pr(>|z|)'],3)
bcop <- ifelse(bcop<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',bcop,sep=''))
legend('topleft',inset=0.05,legend = c(paste('Treat. P=',round(summary(formgroup_abun[[27]])$coefficients[2,'Pr(>|z|)'],3),sep=''),bcop,paste('Int. P=',round(summary(formgroup_abun[[27]])$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
text(x=1:4, y=max(bigcoop_plot$uci.resp)+0.4,labels=c(rep(letters[c(1)],1),rep(letters[c(2)],3)))

#Close contrast ----

#Abundance binomial plots
#Groups excluded due to errors (errors can be avoided but should they occur, can be worked back in)
formgroups_summary$p_binom_int[which(formgroups_summary$group=='Herbivore')] <- NA
formgroups_summary$p_binom_int[which(formgroups_summary$group=='Small_herb')] <- NA
formgroups_summary$p_binom_int[which(formgroups_summary$group=='Big_mixed')] <- NA
formgroups_summary$p_binom_int[which(formgroups_summary$group=='Big_coop')] <- NA
formgroups_summary$p_binom_int[which(formgroups_summary$group=='Herb_mixed')] <- NA

formgroups_summary$p_b_int_adj<-p.adjust(formgroups_summary$p_binom_int,method="hochberg",n=nrow(formgroups_summary)-length(which(is.na(formgroups_summary$p_binom_int))))
formgroups_summary$p_a_int_adj<-p.adjust(formgroups_summary$p_abun_int,method="hochberg",n=nrow(formgroups_summary)-length(which(is.na(formgroups_summary$p_abun_int))))

length(which(!is.na(formgroups_summary$p_binom_int)))
binom.plot <- which(!is.na(formgroups_summary$p_binom_int))

dev.new(width=26,height=26,dpi=100,pointsize=20,noRStudioGD = T)
par(mfrow=c(5,5),mar=c(4,4,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))

for(i in binom.plot){
  
  pred.this.run<-formgroup_bipred[[i]]
  plot(1:4,pred.this.run$fit.resp,ylim=c(0,1),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Prob. occurence',xlab='',main=formgroups_summary$group[i],font.main=1)
  
  axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'),mgp=c(2,0.8,0))
  arrows(1:4,pred.this.run$lci.resp,1:4,pred.this.run$uci.resp,length=0.05,angle=90,code=3)
  text(x=2.25, y=0.97,labels=paste('Int.p=',round(formgroups_summary$p_binom_int[i],2),sep=''),adj=0)
  title(xlab='Year',mgp=c(2,0.8,0))
  
} #close plot loop

par(xpd=NA)
legend(x=7, y=1, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Abundance model plots
length(which(!is.na(formgroups_summary$p_abun_int)))
abun.plot <- which(!is.na(formgroups_summary$p_abun_int))

dev.new(width=38,height=38,dpi=80,pointsize=20,noRStudioGD = T)
par(mfrow=c(7,7),mar=c(4,4,1,1),oma=c(0,0,0,5),mgp=c(2.5,1,0))

for(i in abun.plot){
  
  pred.this.run<-formgroup_abpred[[i]]
  plot(1:4,pred.this.run$fit.resp,ylim=c(0,max(pred.this.run$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Abundance',xlab='',main=formgroups_summary$group[i],font.main=1)
  
  axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'),mgp=c(2,0.8,0))
  arrows(1:4,pred.this.run$lci.resp,1:4,pred.this.run$uci.resp,length=0.05,angle=90,code=3)
  text(x=1, y=(par('usr')[4])-par('usr')[4]/10,labels=paste('Int.p=',round(formgroups_summary$p_abun_int[i],3),sep=''),adj=0)
  title(xlab='Year',mgp=c(2,0.8,0))
  
} #close plot loop

par(xpd=NA)
legend(x=5, y=25, legend=c("Control", "Rock"), pch = c(16,18), cex=1, box.lty=0)
par(xpd=F)

#Close Formicidae ----

#Beta diversity ----

#Calculate gamma
head(taxrich_full2,3);dim(taxrich_full2)
taxrich_full2$ttl_rich<-rowSums(taxrich_full2[,c("Ara_rich","Bla_rich","Col_rich","Form_rich","Ort_rich","Other_rich")])

#Treatment within reserve - gamma
rs <- aggregate(Ara_rich~Site+Year+Treatment,data=taxrich_full2,FUN = sum)
rs$Col_rich <- aggregate(Col_rich~Site+Year+Treatment,data=taxrich_full2,FUN = sum)$Col_rich
rs$Form_rich <- aggregate(Form_rich~Site+Year+Treatment,data=taxrich_full2,FUN = sum)$Form_rich
sum(taxrich_full2$Ara_rich[taxrich_full2$Year==2016 & taxrich_full2$Site=="CR" & taxrich_full2$Treatment=="Control"])

rs<-rs[order(rs$Site,rs$Year,rs$Treatment),]

barplot(rs$Ara_rich,col=c("grey20","grey40","grey60","grey80"))
barplot(rs$Col_rich,col=c("grey20","grey40","grey60","grey80"))
barplot(rs$Form_rich,col=c("grey20","grey40","grey60","grey80"))

#Reserve - gamma
rs2 <- aggregate(Ara_rich~Site+Year,data=taxrich_full2,FUN = sum)
rs2$Col_rich <- aggregate(Col_rich~Site+Year,data=taxrich_full2,FUN = sum)$Col_rich
rs2$Form_rich <- aggregate(Form_rich~Site+Year,data=taxrich_full2,FUN = sum)$Form_rich

rs2<-rs2[order(rs2$Site,rs2$Year),]
rs2$Site_Year<-paste(rs2$Site,rs2$Year,sep='_')

barplot(rs2$Ara_rich,col=c("grey20","grey40"))
barplot(rs2$Col_rich,col=c("grey20","grey40"))
barplot(rs2$Form_rich,col=c("grey20","grey40"))

#Beta diversity
beta_dat<-taxrich_full2[,c("Pit_code","Year","Yr","Site","Plot","Treatment","Pitfall","Replicate","Ara_rich","Col_rich","Form_rich")]
head(beta_dat,5);dim(beta_dat)
beta_dat$Site_Year<-paste(beta_dat$Site,beta_dat$Year,sep='_')
table(rs2$Site_Year %in% beta_dat$Site_Year)
str(rs2)
str(beta_dat)

rs3<-rs2[,3:ncol(rs2)]
colnames(rs3)[1:3]<-c("Ara_gamma","Col_gamma","Form_gamma")
beta_dat<-merge(beta_dat,rs3,by="Site_Year",all.x=T,all.y=F)

beta_dat$Ara_beta<-beta_dat$Ara_gamma/beta_dat$Ara_rich
beta_dat$Col_beta<-beta_dat$Col_gamma/beta_dat$Col_rich
beta_dat$Form_beta<-beta_dat$Form_gamma/beta_dat$Form_rich

#Model beta diversity - issues with modelling with INF values (need to consider how to pick only those that have values)

which(beta_dat$Ara_beta==Inf)

head(beta_dat);dim(beta_dat)
beta_dat$b_int <- NA
b_mod <- list()
b_pred <- list()
b_coeff <- list()

#Replace INF with 0.1

beta_groups <- colnames(beta_dat)[grep("_beta",colnames(beta_dat))]
beta_dat[,beta_groups] <- apply (beta_dat[,beta_groups],MARGIN = 2,FUN = function(x)replace(x,which(x=='Inf'),0.1))

#Beta models

for(i in 1:length(beta_groups)){
  group_this_run <- beta_groups[i]
  resp_this_run <- beta_dat[,group_this_run]
  mod_this_run <- glmer(resp_this_run ~ Treatment+Yr+Treatment:Yr+(1|Site/Replicate),family=Gamma(link='log'),data=beta_dat)
  mod_sum<-summary(mod_this_run)$coefficients
  b_mod[[i]]<-mod_this_run
  b_coeff[[i]]<-mod_sum
  beta_dat$b_int[i]<-round(mod_sum[which(rownames(mod_sum)=='TreatmentRock:Yr'),which(colnames(mod_sum)=='Pr(>|z|)')],4)
  
  mod_nd <- data.frame(Yr=c(0,0,3,3),Treatment=factor(rep(levels(beta_dat$Treatment),2),levels=levels(beta_dat$Treatment)))
  mod_pr <- predictSE(mod=mod_this_run,newdata=mod_nd,se.fit=T,type='response')
  mod_pr<-data.frame(mod_nd, fit.resp=mod_pr$fit, se=mod_pr$se.fit)
  mod_pr$lci.resp<-mod_pr$fit-(1.96*mod_pr$se)
  mod_pr$uci.resp<-mod_pr$fit+(1.96*mod_pr$se)
  mod_pr
  b_pred[[i]]<-mod_pr
} #close fit_binom

b_mod[[]]
b_coeff[[]]
b_pred[[]]

alpha_groups <- colnames(beta_dat)[grep("_rich",colnames(beta_dat))]
group_names <- c('Araneae', 'Coleoptera', 'Formicidae')

plot(1:4,Arainvdiv_pr$fit,ylim=c(min(Arainvdiv_pr$lci),max(Arainvdiv_pr$uci)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Species diversity',xlab='Year',main='Araneae',font.main=1)
axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'))
arrows(1:4,Arainvdiv_pr$lci,1:4,Arainvdiv_pr$uci,length=0.1,angle=90,code=3)
ap <- round(summary(Arainvdiv_mod1)$coefficients[3,'Pr(>|z|)'],3)
ap <- ifelse(ap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',ap,sep=''))
legend('topright',legend = c(paste('Treat. P=',round(summary(Arainvdiv_mod1)$coefficients[2,'Pr(>|z|)'],3),sep=''),ap,paste('Int. P=',round(summary(Arainvdiv_mod1)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')

dev.new(width=14,height=8,dpi=80,pointsize=20,noRStudioGD = T)
par(mfrow=c(2,3),mar=c(5,5,1,1))

for(i in 1:length(beta_groups)){
  
  pred.beta.run<-b_pred[[i]]
  mod.beta.run<-b_mod[[i]]
  plot(1:4,pred.beta.run$fit.resp,ylim=c(0,max(pred.beta.run$uci.resp)),type='p',pch=c(16,18,16,18),xlim=c(0.5,4.5),las=1,cex=1.5,xaxt='n',ylab='Beta diversity',xlab='',main=group_names[i],font.main=1)
  
  axis(side=1,at=c(1.5,3.5),labels=c('2016','2019'),mgp=c(2,0.8,0))
  arrows(1:4,pred.beta.run$lci.resp,1:4,pred.beta.run$uci.resp,length=0.05,angle=90,code=3)
  betap <- round(summary(mod.beta.run)$coefficients[3,'Pr(>|z|)'],3)
  betap <- ifelse(betap<0.001,paste('Yr.P<','0.001',sep=''),paste('Yr.P=',betap,sep=''))
  legend('bottomleft',legend = c(paste('Treat. P=',round(summary(mod.beta.run)$coefficients[2,'Pr(>|z|)'],3),sep=''),betap,paste('Int. P=',round(summary(mod.beta.run)$coefficients[4,'Pr(>|z|)'],3),sep='')),pch="", adj = 0,bty = 'n')
  title(xlab='Year',mgp=c(2,0.8,0))
  
} #close plot loop

for(i in 1:length(beta_groups)){
  
  bgroup.this.run<-beta_groups[i]
  agroup.this.run<-alpha_groups[i]
  bdat.this.run<-beta_dat[,bgroup.this.run]
  adat.this.run<-beta_dat[,agroup.this.run]
  plot(adat.this.run,bdat.this.run,type='p',pch=c(16),las=1,cex=1.5,ylab='Beta diversity',xlab='Alpha diversity (richness)',main=group_names[i],font.main=1)
  
} #close plot loop

signif <- data.frame(group=beta_dat$b_int[which(beta_dat$b_int<0.05)])
signif$index <- which(beta_dat$b_int<0.05)

#Close Beta diverity ----
