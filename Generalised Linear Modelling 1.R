snoring=read.table("snoring-all.txt",header=T)
#all variables
model1 = glm(cbind(hyper,total-hyper)~factor(sex)+smoking+obesity+snoring,family=binomial,data=snoring)
summary(model1)

model1.order = glm(cbind(hyper,total-hyper)~obesity+snoring+factor(sex)+smoking,family=binomial,data=snoring)
anova(model1.order,test="Chisq")
#sex and smoking are not significant on their own

#testing smoking interactions
model2.1 = glm(cbind(hyper,total-hyper)~obesity+snoring+factor(sex)+smoking+snoring*smoking,family=binomial,data=snoring)
summary(model2.1)
#only one significant interaction, therefore we keep smoking
anova(model2.1,test="Chisq")
#Can see that the interaction is significant, therefore we keep the interaction in the model as well as smoking

model2.2 = glm(cbind(hyper,total-hyper)~obesity+snoring+smoking+snoring*smoking+factor(sex),family=binomial,data=snoring)
summary(model2.2)
anova(model2.2,test="Chisq")  
  
#testing factor(sex) interactions
model3.1 = glm(cbind(hyper,total-hyper)~obesity+snoring+smoking+snoring*smoking+factor(sex)+factor(sex)*obesity+factor(sex)*snoring+factor(sex)*smoking,family=binomial,data=snoring)
summary(model3.1)
anova(model3.1,test="Chisq")
#none of the terms are significant therefore we can remove factor(sex) from the model

final.model = glm(cbind(hyper,total-hyper)~obesity+snoring+smoking+snoring*smoking,family=binomial,data=snoring)
summary(final.model) #all terms are significant and can see that residual deviance has decreased.
anova(final.model,test="Chisq")
#snoring*smoking is negatively significant