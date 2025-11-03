#only rain
model1_agri=lm(dfcon[,2]~score_rain)
summary(model1_agri)

#removing outliers
dis=cooks.distance(model1_agri)
which(dis>0.05797)
plot(score_rain[-17],dfcon[-17,2],type="l")
model5_agri=lm(dfcon[-17,2]~score_rain[-17])
summary(model5_agri)
plot(model5_agri)

coch2=cochrane.orcutt(model5_agri)
coch2
summary(coch2)

#considering the model without interactions
model2_agri=lm(dfcon[,2]~score_rain+oni_djf+indi[,1]+indi[,2])
a1=summary(model2_agri)
coch=cochrane.orcutt(model2_agri)
coch
summary(coch)

#with interactions
model3_agri=lm(dfcon[,2]~score_rain+oni_djf+indi[,1]+indi[,2]+indi[,1]*oni_djf+indi[,2]*oni_djf+indi[,1]*score_rain+indi[,2]*score_rain)
a2=summary(model3_agri)
coch1=cochrane.orcutt(model3_agri)
coch1
summary(coch1)



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

bin2=cbind(pred_scrain,dfcon[,2])
bin2=bin2[order(pred_scrain),]
plot(bin2,type="l")

#using moving averages to get the trend
sma_res <- TTR::SMA(score_rain[-17], n = 7)
ewma_res=TTR::EMA(score_rain[-17],ratio=0.2)
help(SMA)
plot(score_rain[-17],type="l")

plot(sma_result,col="red",type="l")
plot(ewma_res,col="red",type="l")
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