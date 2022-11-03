#Loading tidyverse and ggrepel#
library(tidyverse)
library(ggrepel)

#Loading the dataset#
usa<-read.csv("C:/Users/nisha/OneDrive/Desktop/R Project/gun.csv",header = TRUE)
View(usa)

#We will plot the bargraph between states and the respective Murders#
ggplot(usa,aes(x = abb,y = deaths2017, fill=region))+
  geom_bar(stat = "identity")+
  labs(title = "Murders by States",
       x = "State Abbrevations",
       y = "Murders")

#We will plot the bargraph between states and the respective Gun Ownership#
ggplot(usa,aes(x = abb,y = nguns, fill=region))+
  geom_bar(stat = "identity")+
  labs(title = "Gun Ownership by States",
       x = "State Abbrevations",
       y = "Gun Ownership")

# We will define two new variables muders rate and gun ownership rate#
mrate<-usa$deaths2017/usa$pop*100000
gunrate<-usa$nguns/usa$pop*100000

#State with the highest and the lowest murders and gun ownership#
usa$State[which.max(mrate)]
usa$State[which.min(mrate)]
usa$State[which.max(gunrate)]
usa$State[which.min(gunrate)]

#We will plot the bargraph between states and the respective Murder rate#
ggplot(usa,aes(x = abb,y = mrate, fill=region))+
  geom_bar(stat = "identity")+
  labs(title = "Murders by States",
       x = "State Abbrevations",
       y = "Murder Rate")

#We will plot the bargraph between states and the respective Gun Ownership rate#
ggplot(usa,aes(x = abb,y = gunrate, fill=region))+
  geom_bar(stat = "identity")+
  labs(title = "Gun Ownership by States",
       x = "State Abbrevations",
       y = "Gun Ownership Rates")

#we will estimate the effect of population on the murder rate#
reg1=lm(mrate~pop,data=usa)
summary(reg1)

#we will plot the murder rate against population along with the regression line#
usa%>%
  ggplot(aes(pop/10^5,mrate,label=abb))+
  geom_text_repel(nudge_x = 0.1,max.overlaps = 60)+
  geom_point(aes(colour=region),size=3)+
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")+
  geom_smooth(method=lm)+
  labs(x="Population/10^6",
       y="Murders",
       title="Population of  states vs Murders in States")

#we will estimate the effect of gun ownership rate on the murder rate#
reg2<-lm(mrate~gunrate,data=usa)
summary(reg2)

#we will plot the murder rate against gun ownership rate along with the regression line#
usa%>%
  ggplot(aes(gunrate,mrate,label=abb))+
  geom_text_repel(nudge_x = 0.1,max.overlaps = 60)+
  geom_point(aes(colour=region),size=3)+
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")+
  geom_smooth(method=lm)+
  labs(y="Murders",
       x="Gun Ownership",
       title="Gun Ownership in states vs Murders in States")