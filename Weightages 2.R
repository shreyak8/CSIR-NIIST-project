install.packages("DirichletReg")
install.packages("compositions")
library(compositions)
library(DirichletReg)
library(readxl)
data_diri=read_xlsx("C:/Users/Shreya/OneDrive/Documents/NIIST/GDP coeffecients theta.xlsx",sheet="Sheet1")
View(data_diri)

theta_data=data_diri[,11:13]
theta_data1=DR_data(theta_data)

Time <- 1:nrow(theta_data)
length(Time)

weights <- exp(seq(log(1), log(100), length.out=74))
plot(weights)

lambda=0.3
weights <- exp(-lambda * (73:1))
# Ensure weights are numeric and normalized

weights<- weights / sum(weights)
plot(weights)
wts <- as.numeric(weights / sum(weights))
plot(wts)

diri_mod=DirichReg(theta_data1~Time,model=c("alternative"),weights=wts)
summary(diri_mod)

future_time1 <- c(max(Time) + 1:73)
future_time2 <- c(max(Time) + 74:146)
future_thetas1 <- predict(diri_mod, newdata = data.frame(Time = future_time1))
future_thetas1

future_thetas2 <- predict(diri_mod, newdata = data.frame(Time = future_time2))
future_thetas2

#agriculture
agri=append((future_thetas1[,1]),future_thetas2[1:26,1])
agri=append((data_diri$`Theta Weight AGRI`)[-74],agri)
length(agri)

plot(1:150,agri[1:150],type="l")

#industry
ind=append((future_thetas1[,2]),future_thetas2[1:26,2])
ind=append((data_diri$`Theta Weight IND`)[-74],ind)
length(ind)

plot(1:150,ind[1:150],type="l")

#services
serv=append((future_thetas1[,3]),future_thetas2[1:26,3])
serv=append((data_diri$`Theta Weight SERV`)[-74],serv)

plot(1:150,serv[1:150],type="l")



#### PLOTTING ####
weight_df=data.frame(agri[1:150],ind[1:150],serv[1:150])
colnames(weight_df)=c("Agriculture","Industry","Services")

library(tidyverse)

x2=c()
for(i in 1951:2100){
  x2=c(x2,rep(i,3))}
x1=as.data.frame(x2)
colnames(x1)="seq"

data_long <- weight_df %>%
  pivot_longer(cols = c("Agriculture","Industry","Services"), names_to = "Sectors", values_to = "value")

ggplot(data_long, aes(x = x1$seq, y = value, color = Sectors, shape = Sectors, linetype = Sectors)) +
  geom_point(size = 0.75) +
  geom_line() +
  theme_bw() +
  labs(title = "% Weightages for sectors till 2100", x = "Years", y = "% Weightages") +
  theme(legend.position = "right")
