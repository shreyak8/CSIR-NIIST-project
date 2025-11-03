install.packages("DirichletReg")
install.packages("compositions")
library(compositions)
library(DirichletReg)
library(readxl)
data_diri=read_xlsx("C:/Users/Shreya/OneDrive/Documents/NIIST/GDP coeffecients theta.xlsx",sheet="Sheet1")
View(data_diri)

Time <- 1:nrow(theta_data)
theta_data=data_diri[,11:13]
theta_data1=DR_data(theta_data)
length(Time)

weights <- exp(seq(log(0.001), log(1), length.out=74))
plot(weights)

# Normalize weights to sum to 1 (optional but often recommended)
wts <- as.numeric(weights / sum(weights))
plot(wts)
length(data_diri[1:50,c(8)])
help("DirichReg")
diri_mod=DirichReg(theta_data1~Time,model=c("alternative"),weights=wts)
summary(diri_mod)
diri_mod$d
str(wts)

length(wts)
future_time1 <- c(max(Time) + 1:74)
future_time2 <- c(max(Time) + 75:148)
future_thetas1 <- predict(diri_mod, newdata = data.frame(Time = future_time1))
future_thetas1

future_thetas2 <- predict(diri_mod, newdata = data.frame(Time = future_time2))
future_thetas2

plot(predict(diri_mod)[,1])
help(predict)

pmax(future_thetas[,1],0.09)

serv=rbind((data_diri$`Theta Weight SERV`[-74]),t(future_thetas[,3]))
serv=append((future_thetas1[,1]),future_thetas2[1:26,1])
serv=append((data_diri$`Theta Weight AGRI`),serv)
length(serv)
plot(1:174,serv)
plot(1:73,serv[1:73])
length(serv)

help(pmax)
data_diri[66,c(9,10,11)]

plot(data_diri$Year,(data_diri$`Theta Weight AGRI`))
plot(data_diri$Year,(data_diri$`Theta Weight IND`))
plot(1:(173),cbind(data_diri$`Theta Weight SERV`[-74],future_thetas[,3]),xlim=c(1,200),ylim=c(0.2,1))
length(rbind(data_diri$`Theta Weight SERV`[-74],future_thetas[,1]))

mod=lm((data_diri$`Theta Weight SERV`)^(1/2)~data_diri$Year)
summary(mod)
library(MASS)
bx=boxcox(mod)
k=which.max(bx$y)
la=bx$x[k]
la


#
theta_data_ilr <- ilr(theta_data)
ilr_model <- lm(theta_data_ilr ~ Time)
summary(ilr_model)

future_time <- data.frame(Time = max(Time) + 1)
future_ilr <- predict(ilr_model, newdata = future_time)

# Transform the predictions back to the original scale using the inverse ilr transformation
future_thetas <- as.data.frame(ilrInv(future_ilr, orig = theta_data))

theta_data_val<- logistic_transform(future_thetas[,1])

#
transform_params <- function(x) {
  return(0.1 + 0.8 * (exp(x) / (1 + exp(x))))
}
transform_params(0.09)

#
lambda=0.8
weights <- exp(-lambda * (52:1))

# Ensure weights are numeric and normalized
weights <- weights / sum(weights)
plot(weights)
x <- seq(0, 1, length.out = 52)
alpha <- 2.8   # Adjust alpha for growth/decay rate

# Exponential weights normalized to 1
weights <- exp(alpha * x) / exp(alpha)
plot(weights)
weights
#
lambda=0.4
weights <- exp(-lambda * (74:1))

# Ensure weights are numeric and normalized
weights<- weights / sum(weights)
plot(weights)
wts <- as.numeric(weights / sum(weights))
plot(wts)


diri_mod=DirichReg(theta_data1~Time,model=c("alternative"),weights=wts)
summary(diri_mod)

future_time1 <- c(max(Time) + 1:74)
future_time2 <- c(max(Time) + 75:148)
future_thetas1 <- predict(diri_mod, newdata = data.frame(Time = future_time1))
future_thetas1

future_thetas2 <- predict(diri_mod, newdata = data.frame(Time = future_time2))
future_thetas2


serv=append((future_thetas1[,3]),future_thetas2[1:26,3])
serv=append((data_diri$`Theta Weight SERV`),serv)
length(serv)
plot(1:174,serv)


######################################
###*********THETA WEIGHTAGE***********
library(compositions)
library(DirichletReg)
library(readxl)
data_diri=read_xlsx("C:/Users/Shreya/OneDrive/Documents/NIIST/GDP coeffecients theta.xlsx",sheet="Sheet1")
View(data_diri)

theta_data=data_diri[-74,11:13]
theta_data1=DR_data(theta_data)
Time <- 1:nrow(theta_data)

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

