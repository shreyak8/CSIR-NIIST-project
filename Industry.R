library(TSA)
library(aTSA)
library(forecast)
library(lmtest)
library(car)
tt=65
detrend_growth_ind_train=detrend_growth_ind1[1:tt]
detrend_growth_ind_test=detrend_growth_ind1[(tt+1):70]
length(detrend_growth_ind)

cum_dep_2train=cum_dep_1[1:(tt+1)]
cum_dep_2test=cum_dep_1[c((tt+2):72)]

oni_djf_newout=oni_djf_new[-68]
oni_djf_new_train=oni_djf_new[1:(tt+1)]
oni_djf_new_test=oni_djf_new[c((tt+2):72)]

indi_newout=indi_new[-68,]
indi_new_train=indi_new[1:(tt+1),]
indi_new_test=indi[c((tt+3):73),]

mod_auto=auto.arima(x=detrend_growth_ind_train)
summary(mod_auto)
predict(mod_auto,h=6)
mod_arx12=arimax(x=detrend_growth_ind_train,seasonal=list(order=c(0,0,1),period=1),xreg=cbind(cum_dep_2train,oni_djf_new_train,indi_new_train))
summary(mod_arx12)
fitted(mod_arx12)


lines(detrend_growth_ind_train,type="l",col="red")
lines(3:67,fitted(mod_arx12),type="l",col="blue")
xreg1=cbind(20,-3,1,0)
colnames(xreg1)=c("cum_dep_2","oni_djf_new","flood","drought")
help()
predict(mod_arx12,newxreg=cbind(cum_dep_2test,oni_djf_new_test,indi_new_test),h=5)

library(e1071)
indi_new_train1=indi_new_train[,1]
indi_new_train2=indi_new_train[,2]

detrend_growth_ind1=mod3$residuals
length(cum_dep_2train)
mod_svm=svm(detrend_growth_ind1~cum_dep_2train+oni_djf_new_train+indi_new_train1+indi_new_train2,gamma=0.2,cost=10,epsilon=0.05)
summary(mod_svm)

predy=predict(mod_svm,newdata=cbind(cum_dep_2train=cum_dep_2test,oni_djf_new_train=oni_djf_new_test,indi_new_train1=indi_new_test[,1],indi_new_train2=indi_new_test[,2]),h=6)
predy
length(indi_new_test[,2])
predict(mod_svm)
plot(predict(mod_svm),type="l")

growth_transformed_log=sign(growth_con_ind[-c(69)])*(abs(growth_con_ind[-c(69)]))^(1/3)
mod1=lm(growth_transformed_log[-c(1:9,69)]~y$YEAR[-c(1:11,69)])
summary(mod1)
par(mfrow=c(1,1))
bptest(mod1)
plot(mod1$residuals)
durbinWatsonTest(mod1)
shapiro.test()

length(cons$India[-69])
year=y$YEAR
plot(y$YEAR[-c(1,69,29)],growth_transformed_log)
cor(y$YEAR[-c(1,69)],growth_transformed)
plot(y$YEAR[-c(1,69,29)],log((growth_con_ind[-c(69,29)])+6900))
cor(y$YEAR[-c(1,69,29)],log((growth_con_ind[-c(69,29)])+6900))

growth_transformed_log=log((growth_con_ind[-c(69,29)])+6900)
length(growth_transformed_log)
growth_transformed_log_train=growth_transformed_log[1:65]
growth_transformed_log_test=growth_transformed_log[66:70]

library(MASS)
library(car)
year=y$YEAR[-c(1,30,70)]
length(year)
year_train=year[1:65]
year_test=year[66:70]
mod=lm(growth_transformed_log_train~year_train)
summary(mod)
bptest(mod)
durbinWatsonTest(mod)
shapiro.test(mod$residuals)
plot(mod$residuals)

predict(mod,newdata=data.frame(year_train=year_test,h=5))



#Weighted least squares
wt=1/mod$fitted.values^2
wls_mod=lm(growth_transformed_log_train~year_train,weights=wt)

predict()
summary(wls_mod)
durbinWatsonTest(wls_mod)
bptest(wls_mod)
shapiro.test(wls_mod$residuals)

#robust regression model
help(rlm)
year=y$YEAR[-c(1,69,29)]
rmod=rlm(growth_transformed_log_train~year_train,weights=wt)
summary(rmod)

predict(wls_mod,newdata=data.frame(year_train=year_test))

durbinWatsonTest(mod1)
shapiro.test(mod1$residuals)
plot(mod1$residuals)
bptest(mod1)

rmod=rlm(growth_transformed~y$YEAR[-c(1,69)])
summary(rmod)
bptest(rmod)

#
mod=lm(log((growth_con_ind)^2)~y$YEAR[-1])
summary(mod)
bptest(mod)
durbinWatsonTest(mod)
shapiro.test(mod$residuals)

rmod1=rlm(log((growth_con_ind)^2)~y$YEAR[-1])
summary(rmod1)
bptest(rmod)

predict(wls_mod,newdata=data.frame(year=2019))
predict()

#
growth_con_ind_train=growth_con_ind[1:66]
growth_con_ind_test=growth_con_ind[c(67:68,70:72)]

year=1951:2022
year_train=year[1:66]
year_test=year[c(67:72)]
sq=I(year_train^2)
mod3=lm(growth_con_ind_train~year_train+sq)
summary(mod3)

qqnorm(mod3$residuals)
qqline(mod3$residuals)
acf(mod3$residuals,main="ACF plot for 2nd order Industry linear regression")
ggplot(mod3,aes(x=mod3$fitted.values,y=mod3$residuals))+
  geom_point(color="darkblue")+
  theme_bw()+
  labs(x="Model fitted values (Industry)",y="Residuals",title="Residual Plot")+
  geom_hline(yintercept=0)

predict(mod3,newdata = data.frame(year_train=year_test,sq=year_test^2))

library(lmtest)
bptest(mod3)
plot(mod3$residuals)
wt=1/mod3$residuals^2

#gives r^2 of 0.98 and bp test of 1, so not using this
mod5=lm(growth_con_ind_train~year_train+sq,weights=wt)
summary(mod5)
shapiro.test(mod5$residuals)

predict(mod5,newdata = data.frame(year_train=year_test,sq=year_test^2))
predict(mod5)
bptest(mod5)
plot(mod5$residuals)
durbinWatsonTest(mod3)
