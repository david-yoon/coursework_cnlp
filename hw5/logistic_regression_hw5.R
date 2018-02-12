setwd("/home/datol/cw_CNLP/hw5")
cedegren2 <- read.csv("cedegren2.csv", header = TRUE, sep = ",", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "")

attach(cedegren2)
cedegren2

ced2.del <- cbind(sDel, sNoDel)

#ced2.logr <- glm(ced2.del ~ cat * follows * class, family=binomial("logit"))

#ced2.logr <- glm(ced2.del ~ cat + follows + class + cat:follows:class, family=binomial("logit"))

#ced2.logr <- glm(ced2.del ~ cat + follows + class + cat:follows, family=binomial("logit"))
#ced2.logr <- glm(ced2.del ~ cat + follows + class + follows:class, family=binomial("logit"))
ced2.logr <- glm(ced2.del ~ cat + follows + class + cat:class, family=binomial("logit"))

#ced2.logr <- glm(ced2.del ~ cat + follows + class, family=binomial("logit"))

ced2.logr
summary(ced2.logr)

fitted(ced2.logr)

anova(ced2.logr, test="Chisq")
drop1(ced2.logr, test="Chisq")


cat <- c("m", "m", "m", "m", "m", "m", "n", "n", "n", "n", "n", "n", "other", "other", "other", "other", "other", "other")
follows <- c("C", "V", "P", "C", "V", "P", "C", "V", "P", "C", "V", "P", "C", "V", "P", "C", "V", "P")
class <- c("L", "L", "L", "H", "H", "H", "L", "L", "L", "H", "H", "H", "L", "L", "L", "H", "H", "H")
sample <- data.frame(cat, follows, class)

estimated <- within(sample, {
  prop <-predict(ced2.logr, sample, type="response")
  odds <- prop/ (1-prop)
  logit <- log(odds)
})

estimated


with(estimated, interaction.plot(cat, follows, logit))


options(scipen=999)
