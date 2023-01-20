install.packages("trend")
library(trend)
p<-read.csv('BARC_monthly_mean.csv')
Time<-p$Time
p$Time1<-as.numeric(p$Time)
PM2.5=p$PM2_5
date<-as.Date(p$Time1, origin = "1899-12-30")
data_2<-data.frame(date,PM2.5)
data_2<-na.omit(data_2)
plot<-ggplot(data_2,aes(x=date, y=PM2.5))+geom_line(size=.5)+geom_smooth(method="loess", span=.20, se=FALSE, size=1, col="red")+theme_minimal()+theme(axis.line=element_line(size=1))+xlab("Time")+ ylab("PM2.5(ug/cubicmeter)") #theme(panel.grid.major =element_blank()
plot
plot+theme(panel.border =element_rect(colour = "black", fill=NA, size=1))+geom_vline(xintercept= data_2$date[16], col='blue', size=1.5)+geom_vline(xintercept= data_2$date[46], col='blue',size=1.5)+geom_vline(xintercept= data_2$date[96], col='blue',size=1.5)


#############################  Time series ############################

data_1<-data.frame(PM2.5)
ts<-ts(data_1,start = c(2012, 11),end= c(2021, 12), frequency = 12)
ts2<-window(ts, start= c(2019, 1),end= c(2021,12))
TS<-na.StructTS(ts2)
ts4=stl(na.StructTS(ts2), t.window =9, s.window = "periodic")
plot(decompose(na.StructTS(ts2),type = "additive"))
plot(ts4)

############################# Mann-Kendell-Test ########################
mk.test(p$PM2_5)
sens.slope(p$PM2_5)

p<-br.test(TS, m=20000)
plot(p)
plot(na.StructTS(ts2))

p<-br.test()

TS<-na.StructTS(ts)
############################ GAM For Association ######################

library(mgcv)
data(p)
pm25=p$PM2_5
temp=p$Temperature
precip=p$Rain.total.
wind=p$WindSpeed

# Fit the model
gam_model <- gam(pm25 ~ lo(temp) + lo(precip) + lo(wind), data = p)

# Summarize the model
summary(gam_model)

# Plot the model
plot(gam_model)
#########################################################################

library(mgcv)
library(gam)
data <- read.csv("BARC_monthly_mean.csv")
time=data$Time
PM2.5=data$PM2_5
temperature=data$Temperature
precipitation=data$Rain.total.
wind_speed=data$WindSpeed
model <- gam(PM2.5 ~ s(time)+s(temperature) + s(precipitation) + s(wind_speed), data = data, method = "REML")
model <- gam(PM2.5 ~lo(temperature), data = data)

summary(model)

predictions <- predict(model)

plot(model)
par(mfrow=c(1,3),mar=c(5,5,2,2),cex.lab=3,cex.axis=2)
plot(model,residuals=TRUE,se=TRUE,pch="*")
