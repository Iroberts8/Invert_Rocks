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
