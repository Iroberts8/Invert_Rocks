ara3 <- ara[,c(-3,-5)]
summ <- ara3 %>% group_by(Year,Site,Treatment) %>% summarise_all(mean)
boxplot(ara3[,28]~ara3$Treatment)
boxplot(ara3[,28]~ara3$Year)
