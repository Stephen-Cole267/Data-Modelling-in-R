da=read.table("ulcer2.txt",header=T)
da$place = gl(2,4)
da$case = gl(2,2,8)
da$blood = gl(2,1,8)

ulcermod = glm(no~place+case+blood+place*case+case*blood,data=da,family=poisson)
anova(ulcermod,test="Chisq")
#we see that case*blood is significant at 5% level so the probability of having ulcers is associated with blood type.
#place*case must be included due to the minimal model.

#testing interaction terms
anova(update(ulcermod, .~.+blood*place+case*blood*place),test="Chisq")
#three-way interaction not significant, so no evidence that there is a difference between cities.
ulcermod2 = update(ulcermod, .~.+blood*place+case*blood*place)

summary(ulcermod)
summary(ulcermod2)

#case*place is significantly 
6+4+5
3+3+4+4+3
