install.packages("aTSA")
library(aTSA)
growth_con_manu=diff(dfcon[,5],lag=1)
cum_dep=y$`JUN-SEP DEPT`
cum_dep_1=cum_dep[-1]
cum_dep_2=cum_dep[-c(1,2)]

#Binding cumulative departures and growth rates
growth_dep_manu=data.frame(cum_dep_1,growth_con_manu)
#Ordering cumulative departures
growth_dep_manu=growth_dep_manu[order(growth_dep_manu$cum_dep_1),]

#Plotting growth rates and cumulative departures
plot(growth_dep_manu,type="l")

#Correlation
cor(growth_dep_manu)

#Modelling growth rate and departures
model_1par=lm(growth_con_manu~cum_dep_1)
summary(model_1par)

#Plotting both the series separately

#Plotting cumulative departures
plot(y$YEAR,y$`JUN-SEP DEPT`,type="l")
cor(y$YEAR,y$`JUN-SEP DEPT`)

#Plotting growth rate
plot(y$YEAR[-c(1,69,70,72)],growth_con_manu[-c(69,70,72)],type="l")
cor(y$YEAR[-c(1,69,70,72)],growth_con_manu[-c(69,70,72)])
plot(y$YEAR[-c(1,69,70,72)],log((growth_con_manu[-c(69,70,72)])^2),type="l")
cor(y$YEAR[-c(1,69,70,72)],log(growth_con_manu[-c(69,70,72)]^2))


plot(y$YEAR[-c(1,69,70,72)],log(growth_con_manu[-c(69,70,72)]+8535.72),type="l")
cor(y$YEAR[-1],(growth_con_manu))
cor(y$YEAR[-c(1,69,70,72)],log(growth_con_manu[-c(69,70,72)]+8535.72))
mod=lm(log(growth_con_manu[-c(69,70,72)]+21000)~y$YEAR[-c(1,69,70,72)])
summary(mod)


#Now we divide growth rate for agriculture into 2 parts 
#First part is the trend that we will model separately
#Second part the detrended variation which we will try to explain with variation in rainfall,oni,casualties

#Detrending growth rate
detrend_growth_manu = diff((growth_con_manu[-c(69,70,72)]) , lag = 1)
min(growth_con_manu[-c(69,70,72)])

#outlier departures
cum_dep_2out=cum_dep_2[-c(68,69,71)]
length(detrend_growth_manu)

#Binding cumulative departures and detrended growth rates
detrend_dep_manu=data.frame(cum_dep_2out,detrend_growth_manu)
#Ordering cumulative departures
detrend_dep_manu=detrend_dep_manu[order(detrend_dep_manu$cum_dep_2),]

#Plotting cumulative departures and detrended growth rates
plot(detrend_dep_manu,type="l")
cor(cbind(detrend_dep_manu,oni_djf_new[-c(68,69,71)],indi_new[-c(68,69,71),]))

#Linear model with ONI and casualties(flood drought)
oni_djf_new=oni_djf[-c(1,2)]
indi_new=indi[-c(1,2),]
length(indi_new)
model_4par=lm(detrend_growth~cum_dep_2+oni_djf_new+indi_new)
summary(model_4par)


#Using auto.arima to find p,d,q parameters
mod_ar=auto.arima(log(detrend_growth_manu+100000))
summary(mod_ar)

#ARIMAX
mod_arx=arimax(x=log(detrend_growth_manu+10000),seasonal=list(order=c(0,0,1),period=1),xreg=cbind(cum_dep_2out,oni_djf_new[-c(68,69,71)],indi_new[-c(68,69,71),],oni_djf_new[-c(68,69,71)]*indi_new[-c(68,69,71),]))
summary(mod_arx)
mod_arx1=arimax(x=log(detrend_growth_manu+100000),seasonal=list(order=c(1,0,2),period=1),xreg=cbind(cum_dep_2out,oni_djf_new[-c(68,69,71)],indi_new[-c(68,69,71),1]))
#change the value q=2 later if you want, if the model doesnt fit well
length(cum_dep_2out)
summary(mod_arx1)

pacf(log(detrend_growth_manu+100000))
acc=accuracy(mod_arx1)
rmse=acc['Training set','RMSE']

rsquare=1-(rmse^2 *68)/sum((log(detrend_growth_manu+100000)-mean(log(detrend_growth_manu+100000)))^2)
rsquare
#for seasonal=(p,d,q), p is for pacf, d is number times to differentiate, q is for acf
#here used auto.arima for getting the best estimates

#Validating assumptions

#Normality
qqnorm(mod_arx1$residuals)
qqline(mod_arx1$residuals)
acf(mod_arx1$residuals)
shapiro.test(mod_arx1$residuals)

plot(mod_arx1$residuals,type="p")
#Heteroscedasticity
ArchTest(mod_arx1$residuals)


#autocorrelation
acf(mod_arx1$residuals)
garch1=ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm")
model_fit=ugarchfit(spec=garch,data=mod_arx1$residuals)
checkresiduals(model_fit@fit$residuals)

ArchTest(model_fit@fit$residuals)
qqnorm(model_fit@fit$residuals)
shapiro.test(model_fit@fit$residuals[-69])

rmse2=sqrt(mean(model_fit@fit$residuals^2))
rsquare=1-(rmse2^2 *68)/sum((log(detrend_growth_manu+100000)-mean(log(detrend_growth_manu+100000)))^2)
rsquare

#WHAT TO DO ABOUT THESE ANOMALIES?
#CAN WE REMOVE THEM?
#DUE TO THE FALL OF ILFS, THERE WAS A REDUCTION FOR THE YEAR 2019-2020, SO CAN WE JUSTIFY THAT, THAT VALUE ISNT AN ANOMALY CAUSED BY RAINFALL AND REMOVE IT????
#further for year 2022-2023, India's manufacturing sector shrank by 1.1% year-on-year in the third quarter,
#a second straight contraction reflecting lower profit margins and weaker exports. External demand was weak as central banks globally continued monetary tightening to tame inflation.

#So rain wasnt the reason it was affected