library(MASS)
library(orcutt)
library(car)
install.packages("glmnet")
library(glmnet)
#Building model
oni_djf
score_rain
indi
dfcon[,2]
model1_agri=lm(dfcon[,2]~score_rain)
summary(model1_agri)

#USING COCHRANE TO GET BETTER MODEL AUTOCORRELATION 
model2_agri=lm(dfcon[,2]~score_rain+oni_djf+indi[,1]+indi[,2])
a1=summary(model2_agri)
coch=cochrane.orcutt(model2_agri)
coch
summary(coch)

model3_agri=lm(dfcon[,2]~score_rain+oni_djf+indi[,1]+indi[,2]+indi[,1]*oni_djf+indi[,2]*oni_djf+indi[,1]*score_rain+indi[,2]*score_rain)
a2=summary(model3_agri)
coch1=cochrane.orcutt(model3_agri)
coch1
summary(coch1)

#RESIDUAL VS FITTED
plot(model3_agri$residuals,model3_agri$fitted.values)
#boxcox
bx1=boxcox(model3_agri,seq(-4,3,0.01))
k=which.max(bx1$y)
bx1
pow=bx1$x[k]
plot(model4_agri$residuals,model4_agri$fitted.values)
coch2=cochrane.orcutt(model4_agri)
coch2
summary(coch2)

#
dis=cooks.distance(model4_agri)
which(dis>0.05797)

model4_agri=lm((dfcon[,2])^(-0.16)~score_rain+oni_djf+indi[,1]+indi[,2]+score_rain*oni_djf+indi[,1]*oni_djf+indi[,2]*oni_djf+indi[,1]*score_rain+indi[,2]*score_rain)
summary(model4_agri)


acf(model2_agri$residuals)

step(model2_agri,scope=list(upper=~score_rain+oni_djf+indi[,1]+indi[,2]+score_rain*oni_djf+indi[,1]*oni_djf+indi[,2]*oni_djf+indi[,1]*score_rain+indi[,2]*score_rain,lower=~1),direction = c("both"))

a=lm(formula = dfcon[, 2] ~ score_rain)
summary(a)


plot(score_rain,dfcon[,2])
cor(score_rain,indi)

#
plot(score_rain,dfcon[,2])
plot(score_rain[-c(33,45,65,66)],dfcon[-c(33,45,65,66),2])

model5_agri=lm(dfcon[,2]~score_rain)
a5=summary(model5_agri)
plot(model5_agri$residuals,model5_agri$fitted.values)
acf(model5_agri$residuals)

dis=cooks.distance(model5_agri)
which(dis>0.05797)
plot(score_rain[-17],dfcon[-17,2])
model5_agri=lm(dfcon[-17,2]~score_rain[-17])
summary(model5_agri)
plot(model5_agri)

coch2=cochrane.orcutt(model5_agri)
coch2
summary(coch2)
par(mfrow=c(1,1))
x=seq(-20,20,0.1)
plot(x,exp(-(log(x)/0.16)))

bx1=boxcox(model5_agri,seq(-4,3,0.01))
k=which.max(bx1$y)
bx1
pow=bx1$x[k]

model6_agri=lm(dfcon[,2]^(-0.16)~score_rain)
plot(model6_agri$residuals,model6_agri$fitted.values)
acf(model6_agri$residuals)
summary(model6_agri)
coch2=cochrane.orcutt(model6_agri)
coch2
summary(coch2)


#best model yet 
model3_agri=lm(dfcon[-c(33,45,65,66),2]~score_rain[-c(33,45,65,66)]+oni_djf[-c(33,45,65,66)]+indi[-c(33,45,65,66),1]+indi[-c(33,45,65,66),2]+indi[-c(33,45,65,66),1]*oni_djf[-c(33,45,65,66)]+indi[-c(33,45,65,66),2]*oni_djf[-c(33,45,65,66)]+indi[-c(33,45,65,66),1]*score_rain[-c(33,45,65,66)]+indi[-c(33,45,65,66),2]*score_rain[-c(33,45,65,66)])
a2=summary(model3_agri)
coch1=cochrane.orcutt(model3_agri)
coch1
summary(coch1)

vif(model3_agri)
dist=cooks.distance(model3_agri)
which(dist>0.05797)

x=data.matrix(cbind(score_rain[-c(33,45,65,66)],oni_djf[-c(33,45,65,66)],indi[-c(33,45,65,66),1],indi[-c(33,45,65,66),2],indi[-c(33,45,65,66),1]*oni_djf[-c(33,45,65,66)],indi[-c(33,45,65,66),2]*oni_djf[-c(33,45,65,66)],indi[-c(33,45,65,66),1]*score_rain[-c(33,45,65,66)],indi[-c(33,45,65,66),2]*score_rain[-c(33,45,65,66)]))

model3_rid=glmnet(x,dfcon[-c(33,45,65,66),2],alpha=0)
a7=summary(model3_rid)

cv_model=cv.glmnet(x,dfcon[-c(33,45,65,66),2],alpha=0)
best_lam=cv_model$lambda.min
plot(cv_model)

best_model=glmnet(x,dfcon[-c(33,45,65,66),2],alpha=0,lambda=best_lam)
coef(best_model)

ypred=predict(model3_rid,s=best_lam,newx=x)

y1=dfcon[-c(33,45,65,66),2]
sst=sum((y1-mean(y1))^2)
sse=sum((ypred-y1)^2)

rsq=1-sse/sst
rsq

#jittering
dfcon_jittered <- dfcon
dfcon_jittered[, 2] <- jitter(dfcon[, 2])

# Plotting the jittered data
plot(score_rain[-17],dfcon_jittered[-17, 2], xlab = "X", ylab = "Y with Jitter")

# Fitting a linear model
model <- lm(dfcon_jittered[-17, 2] ~ dfcon_jittered[-17, 1])

# Adding the regression line to the plot
abline(model, col = "red")

#
x=seq(-20,20,0.1)
bin=cbind(score_rain[-17],dfcon[-17,2])
bin=bin[order(score_rain[-17]),]
plot(y$YEAR[-17],score_rain[-17],type="l")
plot(bin,type="l")
#method 1 detrending
diff_scrain=diff( score_rain, lag = 1)
diff2_scrain=diff(diff_scrain,lag=1)
plot(y$YEAR[-c(1,2,17)],diff2_scrain[-17],type="l")


#mothode 2 detrending
mod=lm((score_rain)~y$YEAR)
a1=summary(mod)

pred_scrain=(score_rain)-mod$fitted.values
plot(y$YEAR,pred_scrain,type="l")
plot(pred_scrain,dfcon[,2],type="l")
bin2=cbind(pred_scrain,dfcon[,2])
bin2=bin2[order(pred_scrain),]
plot(bin2,type="l")

#plotting trend
install.packages("forecast")
library(forecast)
library(lmtest)

#using moving averages to get the trend
sma_res <- TTR::SMA(score_rain[-17], n = 7)
ewma_res=TTR::EMA(score_rain[-17],ratio=0.2)
help(SMA)
plot(score_rain[-17],type="l")
plot(sma_result,col="red",type="p")
plot(ewma_res,col="red",type="p")
plot(ewma_res,dfcon[-17,2])
cor(as.numeric(ewma_res[9:69]),as.numeric(dfcon[-c(1:8,17),2]))
mod=lm(dfcon[-c(1:8,17),2]~ewma_res[9:69])
summary(mod)
plot(mod$fitted.values,mod$residuals)
acf(mod$residuals)

plot(mod)
bptest(mod)

write.csv(cbind(score_rain,dfcon[,2]),"dftest.csv")
unique(score_rain)
