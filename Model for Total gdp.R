library(TSA)
library(readxl)
data_gdp=read.csv("C:/Users/Shreya/OneDrive/Documents/NIIST/Total gdp.csv")
data_gdp


date=c(1960:2022)
gdp_growth=data.frame(date,data_gdp[,4])
plot(gdp_growth,type="l")
cor(gdp_growth)

pop_data=read.csv("C:/Users/Shreya/OneDrive/Documents/NIIST/popoulation.csv")

war_impact=data.matrix(read_xlsx("C:/Users/Shreya/OneDrive/Documents/NIIST/war impact.xlsx"))

pop_growth1960=pop_data[11:nrow(pop_data),3]
mod=lm(gdp_growth$data_gdp...4.[-61]~gdp_growth$date[-61])
summary(mod)
mod1=lm(gdp_growth$data_gdp...4.[-61]~gdp_growth$date[-61]+cum_dep[c(11:60,62:73)])
summary(mod1)
mod2=lm(gdp_growth$data_gdp...4.[-61]~gdp_growth$date[-61]+cum_dep[c(11:60,62:73)]+pop_growth1960[-61])
summary(mod2)
cum_dep[11:73]
length(gdp_growth)

consumption=read_xlsx("C:/Users/Shreya/OneDrive/Documents/NIIST/consumption.xlsx")
cons_growth=consumption$`Growth Rate`*100
length(cons_growth)

mod3=lm(gdp_growth$data_gdp...4.[-61]~gdp_growth$date[-61]+cum_dep[c(11:60,62:73)]+(war_impact[-61,2]))
summary(mod3)

mod4=lm(gdp_growth$data_gdp...4.[-61]~gdp_growth$date[-61]+cum_dep[c(11:60,62:73)]+cons_growth[-61])
summary(mod4)

cor(war_impact[-61,2],gdp_growth$data_gdp...4.[-61])
cor(gdp_growth$data_gdp...4.[-61],cons_growth[-61])

#arimax
detrend_gdp=diff(gdp_growth$data_gdp...4.,lag = 1)
detrend_gdp
plot(gdp_growth$date[-1],detrend_gdp,type="l")

mod_gdp.ari=auto.arima(detrend_gdp)
summary(mod_gdp.ari)

cum_dep_2out=cum_dep[c(12:73)]
pacf(detrend_gdp)
mod_gdp.arix=arimax(detrend_gdp,seasonal=list(order=c(1,0,2),period=1),xreg=cbind(cum_dep_2out,war_impact[-1,2],indi[12:73,]))
summary(mod_gdp.arix)
length(detrend_gdp)
length(war_impact)

help(auto.arima)
auto.arima(detrend_gdp,seasonal=TRUE,xreg=cbind(cum_dep_2out,war_impact[-1,2],indi[12:73,]))
summary(auto.arima(detrend_gdp,xreg=cbind(cum_dep_2out,war_impact[-1,2],indi[12:73,])))
rmse=3.224

1-(rmse^2 *62)/sum((detrend_gdp-mean(detrend_gdp))^2)      




#
cons=read.csv("C:/Users/Shreya/OneDrive/Documents/NIIST/consumption expenditure.csv",header=TRUE)
cons_expen=cons$India
cor(cons)

abline(v=-4.566)

inv=read.csv("C:/Users/Shreya/OneDrive/Documents/NIIST/Investments.csv",header=TRUE)
inv_exp=inv$Annualr.growth.of.gcf.in..

income=read.csv("C:/Users/Shreya/OneDrive/Documents/NIIST/india-gdp-per-capita.csv",header=TRUE)
income_expen=income$Annual.Growth.Rate....[-1]

plot(gdp_growth$data_gdp...4.[-1],cons_expen)
cor(gdp_growth$data_gdp...4.[-1],cons_expen)
length(cons_expen)

plot(gdp_growth$data_gdp...4.[-1],(inv_exp))
cor(gdp_growth$data_gdp...4.[-1],exp(inv_exp))

plot(gdp_growth$data_gdp...4.[-1],income_expen)
cor(gdp_growth$data_gdp...4.[-1],income_expen)

mod=lm(gdp_growth$data_gdp...4.[-1]~cons_expen+inv_exp+income_expen)
summary(mod)

plot(inv$Year,inv_exp,type="l")
plot(cons$Year,cons_expen)
cor(cons_expen,inv_exp)

mod1=lm(gdp_growth$data_gdp...4.[-1]~cons_expen+cons_expen^2+inv_exp^2+income_expen^2)
summary(mod1)

cor(gdp_growth$data_gdp...4.[-1],pop_growth1960[-1])
mod2=lm(gdp_growth$data_gdp...4.[-1]~cons_expen+cons_expen^2+inv_exp+income_expen+pop_growth1960[-1])
summary(mod2)

vif(mod2)
library(car)
mod3=lm(gdp_growth$data_gdp...4.[-1]~pop_growth1960[-1])
summary(mod3)

length(gdp_growth$data_gdp...4.[-1])

#Considering cons_expen increases at a constant rate every year
mod4=lm(gdp_growth$data_gdp...4.[-1]~(cons_expen)+I(cons_expen^2)+inv_exp+I(inv_exp^2)+(income_expen)+I(income_expen^2))
summ=summary(mod4)
vif(lm(gdp_growth$data_gdp...4.[-1]~(cons_expen)+inv_exp+I(inv_exp^2)+(income_expen)+I(income_expen^2)))
summ
vif(mod4)
max(gdp_growth$data_gdp...4.[-1])
median(gdp_growth$data_gdp...4.[-1])
help(seq)
cons_expen_inc=seq(-40,40,length=62)
inv_inc=seq(-40,40,length=62)
income_expen_inc=seq(-40,40,length=62)
coeff=(summ$coefficients)
gdp_est=coeff[1,'Estimate']+coeff[2,'Estimate']*cons_expen_inc+coeff[3,'Estimate']*I(cons_expen_inc^2)+coeff[4,'Estimate']*inv_exp+coeff[5,'Estimate']*I(inv_exp^2)+(summ$coefficients)[5,'Estimate']*income_expen+coeff[6,'Estimate']*I(income_expen^2)
gdp_est2=coeff[1,'Estimate']+coeff[2,'Estimate']*cons_expen+coeff[3,'Estimate']*I(cons_expen^2)+(summ$coefficients)[4,'Estimate']*(inv_inc)+coeff[5,'Estimate']*I(inv_inc^2)+(summ$coefficients)[5,'Estimate']*income_expen+coeff[6,'Estimate']*I(income_expen^2)
gdp_est3=coeff[1,'Estimate']+coeff[2,'Estimate']*cons_expen+coeff[3,'Estimate']*I(cons_expen^2)+coeff[4,'Estimate']*inv_exp+coeff[5,'Estimate']*I(inv_exp^2)+(summ$coefficients)[5,'Estimate']*income_expen_inc+coeff[6,'Estimate']*I(income_expen_inc^2)
gdp_est
plot(gdp_est,type="l",ylim=c(-150,100))

help(pch)
plot(inv_inc,gdp_est2,type="l",lwd=2)
par(mfrow=c(1,1))
plot(seq(-40,40,length=62),gdp_est,type="o",pch=19,ylim=c(-150,100),lwd=3,xlab="Growth Rate",ylab="GDP Growth Rate",col="blue3")
lines(seq(-40,40,length=62),gdp_est2,type="o",lwd=3,pch=22,col="red3")
lines(seq(-40,40,length=62),gdp_est3,type="o",lwd=3,pch=17,col=3)
text(x=3, y=-140, labels = paste("5%"), pos = 3, col = "black")
text(x=17.5, y=-140, labels = paste("15%"), pos = 3, col = "black")
abline(v= 5,lty=2,lwd=2)
abline(v=15,lty=2,lwd=2)

legend(x = -100  ,   # Position
       legend = c("Consumption Expenditure","Investments","Income per capita"),  # Legend texts
       pch = c(19,22,17),           # Line types
       col=c("blue3","red3",3) ,   # Line colors
       lwd = 2,bty="n",inset=c(-0.2,0.006)) 
sequence=as.data.frame(seq(-40,40,length=62))
gdp_est_df=as.data.frame(as.numeric(gdp_est))
data1=data.frame(sequence,gdp_est_df)
colnames(data1)=c("Rate","GDP est")
help(ggplot)
data2=data.frame(sequence,as.data.frame(as.numeric(gdp_est2)))
colnames(data2)=c("Rate1","GDP est1")
data3=data.frame(sequence,as.data.frame(as.numeric(gdp_est3)))
colnames(data3)=c("Rate1","GDP est1")

ggplot()+
  geom_point(data=data1,mapping=aes(x=Rate,y=`GDP est`),size=1.5)+
  geom_line(data=data1,mapping=aes(x=Rate,y=`GDP est`),linewidth=0.75)+
  geom_point(data=data2,mapping=aes(x=Rate1,y=`GDP est1`),size=1.5,shape=22)+
  geom_line(data=data2,mapping=aes(x=Rate1,y=`GDP est1`),linewidth=0.75)+
  geom_point(data=data3,mapping=aes(x=Rate1,y=`GDP est1`),size=1.5,shape=17)+
  geom_line(data=data3,mapping=aes(x=Rate1,y=`GDP est1`),linewidth=0.75)+
  geom_vline(xintercept=20.327,lty=5)+
  geom_vline(xintercept=-1.967,lty=5)+
  theme_bw()+
  labs(title="Sensitivity Plot",x="Growth Rate for the factors",y="Estimates for GDP growth rate")+
  theme(legend.position = "bottomright")+
  annotate("text", x=23.5, y=-150, label= "20.33%")+
  annotate("text",x=1,y=-150,label="-1.97%")


ggplot()+
  geom_point(data=data1,mapping=aes(x=Rate,y=`GDP est`),size=1.5,shape="Dataset1")+
  geom_line(data=data1,mapping=aes(x=Rate,y=`GDP est`),linewidth=0.75)+
  geom_point(data=data2,mapping=aes(x=Rate1,y=`GDP est1`),size=1.5,shape="Dataset2")+
  geom_line(data=data2,mapping=aes(x=Rate1,y=`GDP est1`),linewidth=0.75)+
  geom_point(data=data3,mapping=aes(x=Rate1,y=`GDP est1`),size=1.5,shape="Dataset3")+
  geom_line(data=data3,mapping=aes(x=Rate1,y=`GDP est1`),linewidth=0.75)+
  geom_vline(xintercept=20.327,lty=5)+
  geom_vline(xintercept=-1.967,lty=5)+
  theme_bw()+
  labs(title="Sensitivity Plot",x="Growth Rate for the factors",y="Estimates for GDP growth rate")+
  theme(legend.position = "bottomright")+
  annotate("text", x=23.5, y=-150, label= "20.33%")+
  annotate("text",x=1,y=-150,label="-1.97%")+
  scale_shape_manual(name="tf",values = c("Dataset1" = 16, "Dataset2" = 22, "Dataset3" = 17))
 

help("scale_shape_manual")
plot(data)

plot(income_expen_inc,gdp_est3)
cons_expen_inc
gdp_est[30]
gdp_est3[47]


cor(gdp_growth$data_gdp...4.[-1],inv_exp)
plot(gdp_growth$data_gdp...4.[-1],inv_exp)
mod4=lm(gdp_growth$data_gdp...4.[-1]~inv_exp+I(inv_exp))
summary(mod4)
a=abs(inv_exp)^(1/3)*sign(inv_exp)
which(gdp_est==max(gdp_est))
cons_expen_inc[44]
sma