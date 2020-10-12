y = rats[,1]
diet = as.factor(rats[,2])
litter = as.factor(rats[,3])

fit1 = lm(y~litter+diet) #litters treated as blocks
anova(fit1) #no evidence of difference in diet, however, the difference in blocks are significant

fit2 = lm(y~diet)  #litter omitted
anova(fit2) #Still no difference in diet

#the total SSq of litter and residuals in CBD is equal to the residual SSq of CRD.
#The MSq for blocks exceeds the residual MSq implies that the residual mean
#square in the CRD analysis should exceed that in the one including blocks.
#Diet MSq is the same in both CBD and CRD.
#Fvalue of diet decreased in CRD as the residuals increased
#p-value increased as we now compare the f-distribution with higher Df.

#Mean weight gain for litter
tapply(y,litter,mean)

c1 = c(-1/4,-1/4,-1/4,1)
C1 = sum(c1*coef(fit2)[-1])
varC1=t(c1)%*%vcov(fit2)[-1,-1]%*%c1 #Variance of contrast
SEC1=sqrt(varC1) 
SEC1
C1+qt(0.025,45,lower.tail=FALSE)*SEC1*c(-1,1)

T = C1/SEC1
(P = 2*pt(T,45,lower.tail=FALSE)) #This contrast is zero.
