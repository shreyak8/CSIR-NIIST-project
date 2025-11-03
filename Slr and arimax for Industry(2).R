install.packages("aTSA")
install.packages("fpp2")
install.packages("rugarch")
install.packages("FinTS")
library(aTSA)
library(TSA)
library(forecast)
library(fpp2)
library(rugarch)
library(FinTS)
growth_con_ind=diff(dfcon[,3],lag=1)
cum_dep=y$`JUN-SEP DEPT`
cum_dep_1=cum_dep[-1]
cum_dep_2=cum_dep[-c(1,2)]

#Binding cumulative departures and growth rates
growth_dep_ind=data.frame(cum_dep_1,growth_con_ind)
#Ordering cumulative departures
growth_dep_ind=growth_dep_ind[order(growth_dep_ind$cum_dep_1),]

#Plotting growth rates and cumulative departures
plot(growth_dep_ind,type="l")

#Correlation
cor(growth_dep_ind)

#Modelling growth rate and departures
model_1par=lm(growth_con_ind~cum_dep_1)
summary(model_1par)

#Plotting both the series separately

#Plotting cumulative departures
plot(y$YEAR,y$`JUN-SEP DEPT`,type="l")
cor(y$YEAR,y$`JUN-SEP DEPT`)

#Plotting growth rate
plot(y$YEAR[-c(1,69)],(growth_con_ind[-69]),type="l")
plot(y$YEAR[-1],(growth_con_ind)^2,type="l")
cor(y$YEAR[-1]^2,(growth_con_ind))

plot(y$YEAR[-c(1,69)],log((growth_con_ind[-69])^2))
cor(y$YEAR[-1],log((growth_con_ind)^2))

plot(y$YEAR[-c(1,69)],log(growth_con_ind[-69]+10000),type="l")
cor(y$YEAR[-c(1,69)],log(growth_con_ind[-69]+10000))

mod=lm(log(growth_con_ind[-69]+10000)~y$YEAR[-c(1,69)])
summary(mod)

#We can use some sort of curve model for this, makes sense because it is the time when technological advancements peaked


#Now we divide growth rate for industry into 2 parts 
#First part is the trend that we will model separately(curvilinear trend)
#Second part the detrended variation which we will try to explain with variation in rainfall,oni,casualties


#Detrending growth rate
detrend_growth_ind = diff(log(growth_con_ind[-69]+10000) , lag = 1)

#69th value is outlier
cum_dep_2out=cum_dep_2[-68]

#Binding cumulative departures and detrended growth rates
detrend_dep_ind=data.frame(cum_dep_2out,detrend_growth_ind)
#Ordering cumulative departures
detrend_dep_ind=detrend_dep_ind[order(detrend_dep_ind$cum_dep_2out),]

#Plotting cumulative departures and detrended growth rates
plot(detrend_dep_ind,type="l")
cor(detrend_dep_ind)

#Linear model with ONI and casualties(flood drought)
oni_djf_new=oni_djf[-c(1,2)]
indi_new=indi[-c(1,2),]
length(indi_new)
model_4par=lm(detrend_growth_ind~cum_dep_2+oni_djf_new+indi_new)
summary(model_4par)
#The hypothesis is being rejected, means there is no practical relation between 
#Makes sense because apart from a few kinks here and there, the line remains pretty constant


#Using auto.arima to find p,d,q parameters
mod_ar=auto.arima(detrend_growth_ind)
help(auto.arima)
summary(mod_ar)
plot(mod_ar$residuals)
mod_ar2=auto.arima(log(detrend_growth_ind))

#ARIMAX
mod_arx=arimax(x=detrend_growth_ind,seasonal=list(order=c(1,0,2),period=1),xreg=cbind(cum_dep_2out,oni_djf_new[-69],indi_new[-69,],oni_djf_new[-69]*indi_new)[-69])
summary(mod_arx)

#USING THIS MODEL
mod_arx1=arimax(x=detrend_growth_ind,seasonal=list(order=c(4,0,1),period=1),xreg=cbind(cum_dep_2out,oni_djf_new[-68],indi_new[-68,]))
summary(mod_arx1)
length(detrend_growth_ind)
mod_arx2=arimax(x=detrend_growth_ind,seasonal=list(order=c(3,0,1),period=1),xreg=cbind(cum_dep_2[-68]))
summary(mod_arx2)
cor(detrend_growth_ind,indi_new)

pacf(detrend_growth_ind)

acc=accuracy(mod_arx1)
rmse=acc['Training set','RMSE']

rsquare=1-(rmse^2*70)/sum((detrend_growth_ind-mean(detrend_growth_ind))^2)
rsquare
#for seasonal=(p,d,q), p is for pacf, d is number times to differentiate, q is for acf
#here used auto.arima for getting the best estimates

#Validating assumptions

#Normality
qqnorm(mod_arx1$residuals)
qqline(mod_arx1$residuals)
shapiro.test(mod_arx1$residuals)

plot(mod_arx1$residuals,type="p")
#Heteroscedasticity
ArchTest(mod_arx1$residuals)


#autocorrelation
acf(mod_arx1$residuals)



#GARCH Modelling
help("ugarchspec")
garch=ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm")
model_fit=ugarchfit(spec=garch,data=mod_arx1$residuals)
checkresiduals(model_fit@fit$residuals)

rmse2=sqrt(mean(model_fit@fit$residuals^2))
rsquare=1-(rmse2^2 *70)/sum((detrend_growth_ind-mean(detrend_growth_ind))^2)
rsquare

ArchTest(model_fit@fit$residuals)

#63.78% accuracy before
#63.54% accuracy after test but no reduction in heteroscedasticity
