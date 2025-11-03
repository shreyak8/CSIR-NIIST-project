#ggplot learning
library(ggplot2)
ggplot(diamonds)
ggplot(diamonds,aes(x=carat))
ggplot(diamonds,aes(x=carat,y=price))
ggplot(diamonds,aes(x=carat,y=price,color=cut))+geom_point()+geom_smooth()
ggplot(diamonds)+geom_point(aes(x=carat,y=price,color=cut))+geom_smooth(aes(x=carat,y=price,color=cut))

dfcon=data.frame(dfcon)
colnames(dfcon)[1]="Year"

growth_transformed_log=sign(growth_con_ind)*(abs(growth_con_ind))^(1/3)
mod_cube=lm(unlist(growth_transformed_log)~y$YEAR[-1])
summary(mod_cube)
predict(mod_cube,data.frame(y$YEAR[-1]))
ggplot(dfcon)
growth_con_agri=as.data.frame(growth_con_agri)
growth_con_ind=as.data.frame(growth_con_ind)
growth_con_serv=as.data.frame(growth_con_serv)

#SLR plots
#AGRICULTURE
ggplot(growth_con_agri,aes(x=1951:2022,y=(growth_con_agri)),color="black")+geom_point(color="red3",size=4,shape=20)+geom_smooth(aes(x=1951:2022,y=growth_con_agri),color="red4",method="lm",se=TRUE)+labs(title="Yearly growth of Agricultural GVA",x="Year",y="Agricultural growth of GVA")+theme_bw()
#INDUSTRY
ggplot(growth_con_ind,aes(x=1951:2022,y=(growth_con_ind)))+
  geom_point(color="blue4",size=4,shape=20)+
  geom_smooth(aes(x=1951:2022,y=growth_con_ind),color="blue4",method="lm",se=TRUE)+
  labs(title="Yearly growth of Industrial GVA",x="Year",y="Industrial growth of GVA")+
  theme_bw() 
#SERVICES
ggplot(growth_con_serv,aes(x=1951:2022,y=(growth_con_serv)))+
  geom_point(color="purple3",size=4,shape=20)+
  geom_smooth(aes(x=1951:2022,y=growth_con_serv),color="purple3",method="loess",se=TRUE)+
  labs(title="Yearly growth of Industrial GVA",x="Year",y="Industrial growth of GVA")+
  theme_bw() 



       
  