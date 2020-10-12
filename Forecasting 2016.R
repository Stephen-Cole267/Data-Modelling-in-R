data=read.table("projectdata.txt")
y=log(data[,32])
energy_cons = list()
energy_cons$data = y
energy_cons$data = ts(energy_cons$data[1:(9*12)],start = c(2006,1),frequency = 12)
energy_cons$monthly = c(rep(1:12,9))

#Adjusting data for seasonailty and trend
energy_cons$trend = filter(energy_cons$data, c(1/24,2/24,2/24,2/24,2/24,2/24,2/24,2/24,2/24,2/24,2/24,2/24,1/24))
energy_cons$Sdev = energy_cons$data - energy_cons$trend
energy_cons$Smeans = tapply(energy_cons$Sdev, energy_cons$monthly,mean,na.rm=T)
energy_cons$Ssd = tapply(energy_cons$Sdev,energy_cons$monthly,sd,na.rm=T)
energy_cons$Seffects = energy_cons$Smeans-mean(energy_cons$Sdev,na.rm=T)
energy_cons$Sadj = energy_cons$data - c(rep(energy_cons$Seffects,9))
energy_cons$TSadj = energy_cons$Sadj - energy_cons$trend

#MA(9)
(energy_cons$ma9=arima(energy_cons$Sadj,order=c(0,0,9)))

#Forecasts with MA(9)
par(mfrow=c(1,1))
energy_cons$ma9F = predict(energy_cons$ma9,n.ahead=12)
(energy_cons$ma9FU = energy_cons$ma9F$pred+qt(.975,106)*energy_cons$ma9F$se)
(energy_cons$ma9FL = energy_cons$ma9F$pred-qt(.975,106)*energy_cons$ma9F$se)

plot(energy_cons$data,ylab="Energy Consumption (10^6kWh)",xlab="Year",xlim=c(2012,2016),ylim = c(2.6,3.9),main="Forecast with MA(9) for 2015")
points(energy_cons$data,bg=1,pch=21)
lines(energy_cons$ma9F$pred+energy_cons$Seffects,lty=2,col=2)
points(energy_cons$ma9F$pred+energy_cons$Seffects,bg=2,pch=21)
lines(energy_cons$ma9FL+energy_cons$Seffects,col=4,lty=2)
lines(energy_cons$ma9FU+energy_cons$Seffects,col=4,lty=2)
legend("topleft",legend=c("Original data","95% confidence interval","Forecast","Trend"),lty=c(1,2,2,2),col=c(1,4,2,3))
abline(v=c(2013,2014,2015),lty=2)

y=log(data[,32])
energy_cons = list()
energy_cons$data = y
energy_cons$data = ts(energy_cons$data[1:(10*12)],start = c(2006,1),frequency = 12)

plot(energy_cons$data,ylab="Energy Consumption (10^6kWh)",xlab="Year",xlim=c(2012,2016),ylim = c(2.6,3.9),main="Zoomed in plot for energy consumption for 2006-2015")
points(energy_cons$data,bg=1,pch=21)
abline(v=c(2013,2014,2015),lty=2)
