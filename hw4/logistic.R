setwd("/home/datol/cw_CNLP/hw4")
cedegren <- read.table("cedegren.txt", header=T)      
attach(cedegren)
ced.del <- cbind(sDel, sNoDel)
ced.logr <- glm(ced.del ~ cat + follows + factor(class), family=binomial("logit"))
ced.logr
summary(ced.logr)


anova(ced.logr, test="Chisq")
drop1(ced.logr, test="Chisq")

