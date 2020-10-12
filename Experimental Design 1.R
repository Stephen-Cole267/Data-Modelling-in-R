barleyCRD
barley_yield = barleyCRD[,1] #2 oz. units
N_fert = barleyCRD[,2]       #0=no fert, 1=single dose, 2=double dose

#Anova and treatment effects
Barley_analysis = lm(barley_yield~as.factor(N_fert))
sum_barley = summary(Barley_analysis)
anova(Barley_analysis)#overwhelming evidence that there is a difference between treatments

#Mean and SED table
Replication=16
SE = sum_barley$coefficients[,2]
SED = sqrt(2934*(1/Replication+1/Replication))
Mean = tapply(barley_yield,N_fert,mean)
Treatment = c(0,1,2)
mean_table = data.frame(Treatment,Mean,SE,SED,Replication)
mean_table

#Contrasts
c1 = c(-1,1) #0 uses no fertiliser, 1 uses single, 2 uses double
C1 = sum(c1*coef(Barley_analysis)[-1]) #Omit estimate of mu
varC1=t(c1)%*%vcov(Barley_analysis)[-1,-1]%*%c1 #Variance of contrast
SEC1=sqrt(varC1) 
C1+qt(0.025,45,lower.tail=FALSE)*SEC1*c(-1,1)

T = C1/SEC1
(P = 2*pt(T,45,lower.tail=FALSE))#Therefore, there is a linear dose response.

c2 = c(-1,1/2)
C2 = sum(c2*coef(Barley_analysis[-1]))
varC2=t(c2)%*%vcov(Barley_analysis)[-1,-1]%*%c2 #Variance of contrast
SEC2=sqrt(varC2) 
C2+qt(0.025,45,lower.tail=FALSE)*SEC2*c(-1,1)

T = C2/SEC2
(P = 2*pt(T,45,lower.tail=FALSE))#Therefore, no linear dose response.