library(readxl)

#Getting the gdp data
df=read_xlsx("C:/Users/Shreya/OneDrive/Documents/NIIST/gdp 2011 base new(1950-2022).xlsx")
View(df)

col=colnames(df)
vec=c()

for(i in 1:length(col))
{
  paste(col[i])
  if(grepl("Constant Prices",col[i])==TRUE)
  {
    vec=c(vec,col[i])
    paste(col[i])
  }
}
vec

#New table only having constant prices
dfcon=cbind(df$Year,df[,vec])
dfcon=dfcon[order(dfcon$`df$Year`),]
write.csv(dfcon,"GDP Constant prices.csv")
cor(dfcon[,2:12])
View(dfcon)


#Rainfall data
#y matrix
y=read_xlsx("C:/Users/Shreya/OneDrive/Documents/NIIST/Rainfall 1950-2022.xlsx")
View(y)

#Only Cumulative rain
y_rain=as.numeric((y$`JUN-SEP`))

#Only cumulative departures
cum_dep=y$`JUN-SEP DEPT`

dataset_rain_gdp=cbind(dfcon$`df$Year`,y_rain,dfcon[,2:ncol(dfcon)])
colnames(dataset_rain_gdp)[1:2]=c("Year","Rainfall")
View(dataset_rain_gdp)


#El Nino- La Nina effect data
oni=(read_xlsx("C:/Users/Shreya/OneDrive/Documents/NIIST/El Nino-La Nina effect.xlsx"))
View(oni)
oni_djf=(oni$DJF)
length(oni_djf)

#Flood Drought Data
mu=mean(y_rain)
fdn1=c()
for(i in 1:length(y_rain))
{
  if ((rev(y_rain))[i]>(mu+0.10*mu))
  {
    fdn1[i]="flood"
  }
  else if((rev(y_rain))[i]<(mu-0.10*mu))
  {
    fdn1[i]="drought"
  }
  else
  {
    fdn1[i]="normal"
  }
}
fdn1

#
flood=ifelse(fdn1 == 'flood', 1, 0)
drought=ifelse(fdn1 == 'drought', 1, 0)
indi=(cbind(flood, drought))
indi

exogen=data.frame(cum_dep_2,oni_djf_new,indi_new)
write.csv(exogen,"exogenoue_var.csv")

