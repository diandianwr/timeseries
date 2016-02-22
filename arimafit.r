library(dplyr)
library(tscount)
library(zoo)
library(forecast)


# setwd("C:/Users/212421898/Desktop/Weiwei/UW/Thesis")
# setwd("/Users/weiran/Google Drive/WeiweiThesis/BicycleCount/bikecounts")
setwd("~/Google Drive/WeiweiThesis/BicycleCount/bikecounts")

bcraw.df <- read.csv("data/weatherbike2yr.csv",stringsAsFactors=F)


#select model data
bc.df <- bcraw.df %>%
  select(Date,count,precipIntensityMax,temperatureMax,holiday,uw,dow) %>%
  mutate(dow=as.numeric(as.factor(dow)))


#change to Data type
bc.df$Date <- as.Date(bc.df$Date,"%Y-%m-%d") 
bc.zoo <- zoo(bc.df[,-1],bc.df[,1])
bc.ts <- as.ts(bc.zoo)

#----------------------model fitting
#arima pretest
count.ts <- bc.ts[,1]
auto.arima(count.ts)  

# Model1: without day of week
model1_reg <- as.matrix(bc.df[,3:5])  # select regressor and convert to matrix
rownames(model1_reg) <- seq(1,nrow(model1_reg))

model1 <- Arima(count.ts,order=c(3,1,3),xreg=model1_reg)   

# Model 2: add day of week as regressor
dow_dumi<-model.matrix(~as.factor(bc.df$dow))[,2:7]  # prepare model 2 regressors
colnames(dow_dumi) <- c('Mon','Sat','Sun','Thurs','Tues','Wed')
dow_dumi <- dow_dumi[,c(2,3,1,5,6,4)]
model2_reg <- cbind(model1_reg,dow_dumi)

model2 <- Arima(count.ts,order=c(3,1,3),xreg=model2_reg)


#compare model with dow and without dow as regressors
model1
model2

tsdiag(model1)
tsdiag(model2)

# plot actual observation vs fitted value

ggplot(data.frame(actual=bc.df$count, predicted=fitted.values(model1)),
       aes(x=actual, y=predicted)) +
  geom_abline(intercept=0, slope=1, color="red") +
  geom_point() + theme_bw() + ggtitle("ARIMA")

ggplot(data.frame(actual=bc.df$count, predicted=fitted.values(model2)),
       aes(x=actual, y=predicted)) +
  geom_abline(intercept=0, slope=1, color="red") +
  geom_point() + theme_bw() + ggtitle("ARIMA")

# plot fitted value of model2 over time 
plot(count)
lines(fitted(model2),col=2)
