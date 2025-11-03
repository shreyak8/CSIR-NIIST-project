library(readxl)

#FLOOD DROUGHT DATA
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

