install.packages("emmeans")
library(emmeans)

data(pigs)
pigs2 <- pigs
pigs2$percent <- as.factor(pigs2$percent)
pigs2$conc <- log(pigs2$conc)
pigs$conc <- log(pigs$conc)

model1 <- lm(conc ~ source * percent, data = pigs2)
anova1 <- anova(model1)

model2 <- lm(conc ~ percent * source, data = pigs2)
anova2 <- anova(model2)

anova3 <- anova1
anova3[1,] <- anova2[2,]

anova4 <- joint_tests(emmeans(model1, specs = c("source", "percent")))
MSE = anova1["Residuals", "Mean Sq"]
MSR = anova4$F.ratio * MSE
SumSq = anova4$df1 * MSR
anova4 <- cbind(Df = anova4$df1, SumSq = SumSq, MeanSq = MSR, Fvalue = anova4$F.ratio, pval = anova4$p.value)
anova4 <- rbind(anova4, unlist(anova3[4,]))
anova4

tapply(pigs2$conc, pigs2$source, mean)
tapply(pigs2$conc, pigs2$percent, mean)

model3 <- lm(conc ~ source * percent, data = pigs)
lm(conc ~ source + percent + source:percent, data = pigs)
anova(model1, model3)

coef <- model3$coefficients
sprintf("fish: y = %2.2f + x * %2.2f", coef[1], coef[4])
sprintf("soy: y = %2.2f + x * %2.2f", coef[1] + coef[2], coef[4] + coef[5])
sprintf("skim: y = %2.2f + x * %2.2f", coef[1] + coef[3], coef[4] + coef[6])