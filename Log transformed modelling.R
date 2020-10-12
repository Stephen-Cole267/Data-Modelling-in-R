library(lmtest)
library(timeSeries)
library(forecast)
library(tseries)

data=read.table("projectdata.txt")
y=log(data[,32])
energy_cons = list()
energy_cons$data = y
energy_cons$data = ts(y,start = c(2006,1),frequency = 12)
energy_cons$monthly = c(rep(1:12,10))

plot(decompose(energy_cons$data))
#Clear evidence of seasonality and trend

#test for stationarity 
adf.test(energy_cons$data, alternative = "stationary")

time = c(1:120)
season=gl(12,1,120)
fit = lm(energy_cons$data~time+season)

dwtest(fit) #p-value very small implying significant positive autocorrelation
residt = timeSeries(residuals(fit),time)
turnsStats(residt)
abs(65-78.66667)/sqrt(21.01111) #greater than 1.96 ==> reject randomness

#Adjusting data for seasonailty and trend
energy_cons$trend = filter(energy_cons$data, c(1/24,2/24,2/24,2/24,2/24,2/24,2/24,2/24,2/24,2/24,2/24,2/24,1/24))
energy_cons$Sdev = energy_cons$data - energy_cons$trend
energy_cons$Smeans = tapply(energy_cons$Sdev, energy_cons$monthly,mean,na.rm=T)
energy_cons$Ssd = tapply(energy_cons$Sdev,energy_cons$monthly,sd,na.rm=T)
energy_cons$Seffects = energy_cons$Smeans-mean(energy_cons$Sdev,na.rm=T)
energy_cons$Sadj = energy_cons$data - c(rep(energy_cons$Seffects,10))
(energy_cons$TSadj = energy_cons$Sadj - energy_cons$trend)
ni = 9
energy_cons$SE = energy_cons$Ssd/sqrt(ni)
t = abs(energy_cons$Seffects)/energy_cons$SE # compare with 1.833
ni_1 = ni-1

#all seasonal effected are significant apart from Apr and Sept
data.frame(energy_cons$Seffects,energy_cons$Ssd,ni,energy_cons$SE,t,ni_1, p= 1-pt(t,8))

adf.test(energy_cons$Sadj, alternative = "stationary")

#Plot of results
par(mfrow=c(1,1))
plot(energy_cons$data,xlab="Year",ylab="Energy Consumption (10^6kWh)")
points(energy_cons$data,pch=21,bg=1)
lines(energy_cons$trend,lty=2,col=3)
legend("topleft",c("Energy Consumption (10^6kWh)","Trend"),pch=c(21,NA),lty=c(1,2), col=c(1,3),pt.bg=c(1,NA))
plot(energy_cons$data,xlab="Year",ylab="Energy Consumption (10^6kWh)")
points(energy_cons$data,pch=21,bg=1)
lines(energy_cons$Sadj, col=2)
points(energy_cons$Sadj, col=2, pch=22, bg=2)
legend("topleft",c("Energy Consumption (10^6kWh)","Seasonally adjusted data"),pch=c(21,22),lty=c(1,1), col=c(1,2),pt.bg=c(1,2))

par(mfrow=c(1,1))
plot(energy_cons$TSadj,ylab="Data adjusted for seasonality and trend",xlab="Year")
points(energy_cons$TSadj,pch=21,bg=1)

energy_cons$acf = acf(energy_cons$Sadj, main="", ci.type="ma", lag.max=30)
energy_cons$pacf = pacf(energy_cons$Sadj,main="",lag.max=30) #ARMA(1,9)

(energy_cons$ar1=arima(energy_cons$Sadj,order=c(1,0,0)))
(t = 0.8839/0.0430)
pt(t,120-2) #significant
tsdiag(energy_cons$ar1,gof.lag=40) #p-values below threshold - not good fit
plot.default(energy_cons$Sadj-energy_cons$ar1$residuals,energy_cons$ar1$residuals,xlab="Fitted Values",ylab="Residuals")


#MA(9)
(energy_cons$ma9=arima(energy_cons$Sadj,order=c(0,0,9)))
(t = 0.1846/0.0896)
pt(t,120-9-1)
(p=2*(1-0.9791338)) #less than 0.05 so beta is significantly different from 0
tsdiag(energy_cons$ma9,gof.lag=40) #very good fit
plot.default(energy_cons$Sadj-energy_cons$ma9$residuals,energy_cons$ma9$residuals,xlab="Fitted Values",ylab="Residuals")
qqnorm(energy_cons$ma9$residuals)
qqline(energy_cons$ma9$residuals)
#Randomly scattered

#MA(9) best possible model

#Forecasts with MA(9)
(energy_cons$ma9F = predict(energy_cons$ma9,n.ahead=6))
(energy_cons$ma9FU = energy_cons$ma9F$pred+qt(.975,118)*energy_cons$ma9F$se)
(energy_cons$ma9FL = energy_cons$ma9F$pred-qt(.975,118)*energy_cons$ma9F$se)

plot(energy_cons$data,ylab="Energy Consumption (10^6kWh)",xlab="Year",xlim=c(2006,2017),ylim = c(2.6,3.9),main="Forecast with MA(9) for 6 months")
points(energy_cons$data,bg=1,pch=21)
lines(energy_cons$ma9F$pred+energy_cons$Seffects[1:6],lty=2,col=2)
points(energy_cons$ma9F$pred+energy_cons$Seffects[1:6],bg=2,pch=21)
lines(energy_cons$ma9FL+energy_cons$Seffects[1:6],col=4,lty=2)
lines(energy_cons$ma9FU+energy_cons$Seffects[1:6],col=4,lty=2)
lines(energy_cons$trend,lty=2,col=3)
legend("topleft",legend=c("Original data","95% confidence interval","Forecast","Trend"),lty=c(1,2,2,2),col=c(1,4,2,3))
abline(v=2016,lty=2)
