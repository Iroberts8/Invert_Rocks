#Species data from 2016 and 2019
#Five reserves

#Five target orders
#1 Araneae (Spiders) - Family
#2 Blattodea (Cockroaches) - Family
#3 Coleoptera (Beetles) - Family
#4 Orthoptera (Crickets and Grasshoppers) - Sub-family
#5 Formicidae (Ants) - Sub-family

#6 Other invertebrates (identified to Order only - does not include Hemiptera, Hymenoptera)

#Species information
head(Morphospecies);dim(Morphospecies)

#Site data (Site_data)
Site <- read.csv('Data/Site_data.csv',header=T)
head(Site);dim(Site)

head(Araneae[,1:10],3);dim(Araneae)
colnames(Araneae)[6:ncol(Araneae)] %in% Morphospecies$Morphospecies

table(Morphospecies$Order)
Morphospecies[grep(pattern = 'Larvae',x = Morphospecies$Morphospecies),]

#Create taxanomic richness data
basedata <- Araneae[,1:5]
basedata$Pit_code <- paste(basedata$Site,basedata$Plot,basedata$Treatment,basedata$Pitfall,basedata$Year,sep='_')
head(basedata);dim(basedata)

taxrich <- basedata
head(taxrich);dim(taxrich)

Ara_rich <- data.frame(Pit_code=basedata$Pit_code, Ara_rich=apply(X = Araneae[,6:ncol(Araneae)], MARGIN = 1, FUN = function(x) length(which(x>0))))
head(Ara_rich);dim(Ara_rich)

taxrich2 <- merge(taxrich,Ara_rich,by='Pit_code',all.x=T,all.y=F)
head(taxrich2);dim(taxrich2)

#My summary experiments

ara2 <- Araneae[,c(-3,-5)]
Ara_summ <- ara2 %>% group_by(Year,Site,Treatment) %>% summarise_all(mean, sum)
write.csv(Ara_summ,file='Araneae summary')
#Lycosidae 1 vs Treatment
boxplot(ara2[,28]~ara2$Treatment)
#Lycosidae 6 vs Year
boxplot(ara2[,28]~ara2$Year)

Bla2 <- Blattodea[,c(-3,-5)]
Bla_summ <- Bla2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)
write.csv(Bla_summ,file='Blattodea summary')

Col2 <- Coleoptera[,c(-3,-5)]
Col_summ <- Col2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)
write.csv(Col_summ,file='Coleoptera summary')

Form2 <- Formicidae[,c(-3,-5)]
Form_summ <- Form2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)
write.csv(Form_summ,file='Formicidae summary')
#Dolichoderinae 1 vs treatment
boxplot(Form2[,5]~Form2$Treatment)
#Dolichoderinae 1 vs Year
boxplot(Form2[,5]~Form2$Year)

Ort2 <- Orthoptera[,c(-3,-5)]
Ort_summ <- Ort2 %>% group_by(Year,Site,Treatment) %>% summarise_all(sum)
write.csv(Ort_summ,file='Orthoptera summary')

Morpho2 <- Morphospecies[,c(-2,-3)]
Morpho2 <- group_by(Morphospecies)
unique(Morpho2$Morphospecies)
unique(Morpho2$Order)
Morpho_summ <- sapply(Morphospecies, function(Order) length(unique(Order)))

full_summ <- cbind(Ara_summ, Bla_summ, Col_summ, Form_summ, Ort_summ)
write.csv(full_summ,file='Target summary')
