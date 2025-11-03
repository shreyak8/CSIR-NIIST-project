install.packages("aTSA")
library(aTSA)
library(tseries)
library(vars)
library(forecast)
library(TSA)
growth_con_agri=diff(dfcon[,2],lag=1)
cor(growth_con_agri[11:72],data_gdp$Annual...Change[-length(data_gdp$Annual...Change)])
cum_dep=y$`JUN-SEP DEPT`
cum_dep_1=cum_dep[-1]
cum_dep_2=cum_dep[-c(1,2)]

#Binding cumulative departures and growth rates
growth_dep_bind=data.frame(cum_dep_1,growth_con_agri)
#Ordering cumulative departures
growth_dep_bind=growth_dep_bind[order(growth_dep_bind$cum_dep_1),]

#Plotting growth rates and cumulative departures
plot(growth_dep_bind,type="l")

#Correlation
cor(growth_dep_bind)

#Modelling growth rate and departures
model_1par=lm(growth_con_agri~cum_dep_1)
summary(model_1par)

#Plotting both the series separately

#Plotting cumulative departures
plot(y$YEAR,y$`JUN-SEP DEPT`,type="l")
cor(y$YEAR,y$`JUN-SEP DEPT`)

#Plotting growth rate
plot(y$YEAR[-1],growth_con_agri,type="l")
abline(v=2018)
cor(y$YEAR[-1],growth_con_agri)
write.csv(cbind(y$YEAR[-1],growth_con_agri),"df_agri.csv")

#Now we divide growth rate for agriculture into 2 parts 
#First part is the trend that we will model separately
#Second part the detrended variation which we will try to explain with variation in rainfall,oni,casualties

#Detrending growth rate
detrend_growth = diff(growth_con_agri , lag = 1)

#Binding cumulative departures and detrended growth rates
detrend_dep_bind=data.frame(cum_dep_2,detrend_growth)
#Ordering cumulative departures
detrend_dep_bind=detrend_dep_bind[order(detrend_dep_bind$cum_dep_2),]

#Plotting cumulative departures and detrended growth rates
plot(detrend_dep_bind,type="l")
cor(detrend_dep_bind)
  
#Linear model with ONI and casualties(flood drought)
oni_djf_new=oni_djf[-c(1,2)]
indi_new=indi[-c(1,2),]
length(indi_new)
model_4par=lm(detrend_growth~cum_dep_2+oni_djf_new+indi_new)
summary(model_4par)


#Using auto.arima to find p,d,q parameters
mod_ar=auto.arima(detrend_growth)
summary(mod_ar)

#ARIMAX
mod_arx=arimax(x=detrend_growth,seasonal=list(order=c(1,0,2),period=1),xreg=cbind(cum_dep_2,oni_djf_new,indi_new,oni_djf_new*indi_new))
summary(mod_arx)
mod_arx1=arimax(x=detrend_growth[1:65],seasonal=list(order=c(0,0,0),period=1),xreg=cbind(cum_dep_2[1:65],oni_djf_new[1:65],indi_new[1:65,]))
summary(mod_arx1)
x1=predict(mod_arx1,newxreg=cbind(cum_dep_2[66:71],oni_djf_new[66:71],indi_new[66:71,]))
plot(1:71,x1$pred,type="l")
length(x1)
x1$pred
library(forecast)
fitted(mod_arx1)
par(mfrow=c(1,1))
checkresiduals(mod_arx1)

lines(1:71,fitted(mod_arx1),col="red",lty=2)
plot(1:71,detrend_growth,type="l")
lines(predict(mod_svm),type="l",col="blue")

a1=(fitted(mod_arx1)[1:65]*1.7+predict(mod_svm))/2
b=c()
a=seq(0,2.5,by=0.1)
j=1:25
for(i in 1:length(a)){
b[i]=(sqrt(mean((detrend_growth[-1]-fitted(mod_arx1)[-71]*a[i])^2)))
  }
  
plot(b)

for(i in a){
print(sqrt(mean((detrend_growth[1:65]-predict(mod_svm)*i)^2)))}

sqrt(mean((detrend_growth[1:65]-a1)^2))

##mod_arx1
xreg1=rbind(cbind(10,-2,0,0),cbind(20,-3,1,0))
colnames(xreg1)=c("cum_dep_2","oni_djf_new","flood","drought")
help()
predict(mod_arx1,newxreg=xreg1,h=2)
predict(mod_ar,h=1)

acc=accuracy(mod_arx1)
rmse=acc['Training set','RMSE']



rsquare=1-(rmse^2 *71)/sum((detrend_growth-mean(detrend_growth))^2)
rsquare
#for seasonal=(p,d,q), p is for pacf, d is number times to differentiate, q is for acf
#here used auto.arima for getting the best estimates


#Validating assumptions

#Normality
qqnorm(mod_arx1$residuals)
qqline(mod_arx$residuals)
shapiro.test(mod_arx1$residuals)

plot(mod_arx$residuals,type="p")
#Heteroscedasticity
ArchTest(mod_arx1$residuals)


#autocorrelation
acf(mod_arx$residuals)
