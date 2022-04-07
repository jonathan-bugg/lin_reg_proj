install.packages("ggplot2")
library(ggplot2)

logError <- read.csv(file = 'data/train_2016_v2.csv')
head(logError)
properties_2016 <- read.csv(file = 'data/properties_2016.csv')
head(properties_2016)

ggplot(properties_2016,aes(x=bathroomcnt))+geom_histogram(binwidth=1)
ggplot(properties_2016,aes(x=bedroomcnt))+geom_histogram(binwidth=1)
ggplot(properties_2016,aes(x=buildingqualitytypeid))+geom_histogram(binwidth=1)
ggplot(properties_2016,aes(x=calculatedbathnbr))+geom_histogram(binwidth=1)
ggplot(properties_2016,aes(x=calculatedfinishedsquarefeet))+geom_histogram(binwidth=500)+xlim(-250,10000)
ggplot(properties_2016,aes(x=finishedsquarefeet12))+geom_histogram(binwidth=500)+xlim(-250,10000)
ggplot(properties_2016,aes(x=fullbathcnt))+geom_histogram(binwidth=1)
ggplot(properties_2016,aes(x=heatingorsystemtypeid))+geom_histogram(binwidth=1)
ggplot(properties_2016,aes(x=lotsizesquarefeet))+geom_histogram(binwidth=5000)+xlim(-2500,50000)

ggplot(properties_2016,aes(x=propertylandusetypeid))+geom_histogram(binwidth=1)+xlim(30,48)+ylim(0,1000000)
ggplot(properties_2016,aes(x=propertylandusetypeid))+geom_histogram(binwidth=1)+xlim(245,249)+ylim(0,1000000)
ggplot(properties_2016,aes(x=propertylandusetypeid))+geom_histogram(binwidth=1)+xlim(259,276)+ylim(0,1000000)

ggplot(properties_2016,aes(x=roomcnt))+geom_histogram(binwidth=5)
ggplot(properties_2016,aes(x=unitcnt))+geom_histogram(binwidth=1)+xlim(0,20)
ggplot(properties_2016,aes(x=yearbuilt))+geom_histogram(binwidth=10)+xlim(1850,2020)
ggplot(properties_2016,aes(x=structuretaxvaluedollarcnt))+geom_histogram(binwidth=50000)+xlim(-25000,2000000)
ggplot(properties_2016,aes(x=taxvaluedollarcnt))+geom_histogram(binwidth=50000)+xlim(-25000,4000000)
ggplot(properties_2016,aes(x=assessmentyear))+geom_histogram(binwidth=1)
ggplot(properties_2016,aes(x=landtaxvaluedollarcnt))+geom_histogram(binwidth=50000)+xlim(-25000,2000000)
ggplot(properties_2016,aes(x=taxamount))+geom_histogram(binwidth=1000)+xlim(-500,50000)

joined <- merge(properties_2016,logError,by = "parcelid")

modelx4 <- lm(logerror~bathroomcnt,data=joined)
summary(modelx4)
ggplot(joined,aes(bathroomcnt,logerror))+geom_point()+geom_smooth(method='lm')

modelx5 <- lm(logerror~bedroomcnt,data=joined)
summary(modelx5)
ggplot(joined,aes(bedroomcnt,logerror))+geom_point()+geom_smooth(method='lm')

modelx6 <- lm(logerror~buildingqualitytypeid,data=joined)
summary(modelx6)
ggplot(joined,aes(buildingqualitytypeid,logerror))+geom_point()+geom_smooth(method = 'lm')

modelx8 <- lm(logerror~calculatedbathnbr,data=joined)
summary(modelx8)
ggplot(joined,aes(calculatedbathnbr,logerror))+geom_point()+geom_smooth(method='lm')

modelx11 <- lm(logerror~calculatedfinishedsquarefeet,data=joined)
summary(modelx11)
ggplot(joined,aes(calculatedfinishedsquarefeet,logerror))+geom_point()+geom_smooth(method='lm')

modelx12 <- lm(logerror~finishedsquarefeet12,joined)
summary(modelx12)
ggplot(joined,aes(finishedsquarefeet12,logerror))+geom_point()+geom_smooth(method='lm')

modelx19 <- lm(logerror~fullbathcnt,joined)
summary(modelx19)
ggplot(joined,aes(fullbathcnt,logerror))+geom_point()+geom_smooth(method='lm')

modelx23 <- lm(logerror~heatingorsystemtypeid,joined)
summary(modelx23)
ggplot(joined,aes(heatingorsystemtypeid,logerror))+geom_point()+geom_smooth(method='lm')

modelx26 <- lm(logerror~lotsizesquarefeet,joined)
summary(modelx26)
ggplot(joined,aes(lotsizesquarefeet,logerror))+geom_point()+geom_smooth(method='lm')

modelx33 <- lm(logerror~propertylandusetypeid,joined)
summary(modelx33)
ggplot(joined,aes(propertylandusetypeid,logerror))+geom_point()+geom_smooth(method='lm')

modelx40 <- lm(logerror~roomcnt,joined)
summary(modelx40)
ggplot(joined,aes(roomcnt,logerror))+geom_point()+geom_smooth(method='lm')

modelx44 <- lm(logerror~unitcnt,joined)
summary(modelx44)
ggplot(joined,aes(unitcnt,logerror))+geom_point()+geom_smooth(method='lm')

modelx47 <- lm(logerror~yearbuilt,joined)
summary(modelx47)
ggplot(joined,aes(yearbuilt,logerror))+geom_point()+geom_smooth(method='lm')

modelx50 <- lm(logerror~structuretaxvaluedollarcnt,joined)
summary(modelx50)
ggplot(joined,aes(structuretaxvaluedollarcnt,logerror))+geom_point()+geom_smooth(method='lm')

modelx51 <- lm(logerror~taxvaluedollarcnt,joined)
summary(modelx51)
ggplot(joined,aes(taxvaluedollarcnt,logerror))+geom_point()+geom_smooth(method='lm')

modelx53 <- lm(logerror~landtaxvaluedollarcnt,joined)
summary(modelx53)
ggplot(joined,aes(landtaxvaluedollarcnt,logerror))+geom_point()+geom_smooth(method='lm')

modelx54 <- lm(logerror~taxamount,joined)
summary(modelx54)
ggplot(joined,aes(taxamount,logerror))+geom_point()+geom_smooth(method='lm')
