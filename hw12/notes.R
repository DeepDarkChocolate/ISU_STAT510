
library(lme4)
library(dplyr)

d = read.delim("http://dnett.github.io/S510/LeafArea.txt")
o = lmer(LeafArea ~ Dose + (1 + Dose | ResearchStation), data = d, REML = TRUE,
         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

sigma(o)^2

VarCorrdat <- as.data.frame(VarCorr(o))$vcov
Sigma <- matrix(c(VarCorrdat[1], VarCorrdat[3], 
                  VarCorrdat[3], VarCorrdat[2]), nr = 2)
round(Sigma, 6)

d2 <- filter(d, d$ResearchStation == 7)
plot(d2$Dose, d2$LeafArea)
beta <- fixef(o)
b <- ranef(o)$ResearchStation
abline(beta)

beta0 <- unlist(beta + b[7,])
sprintf("%gx +%g", beta0[1], beta0[2])

o2 = lm(LeafArea ~ Dose, data = d2)
beta2 <- coef(o2)
sprintf("%gx +%g", beta2[1], beta2[2])

abline(beta0, col = "red")
abline(beta2, col = "blue")

o = lmer(LeafArea ~ Dose + (1 + Dose | ResearchStation), data = d, REML = TRUE,
         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

o3 <- lmer(LeafArea ~ 1 + (1 + Dose | ResearchStation), data = d, REML = TRUE,
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
anova(o, o3)

AIC(o)

AIC(lmer(LeafArea ~ Dose + (1 | ResearchStation), data = d, REML = TRUE,
         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))

AIC(logLik(lm(LeafArea ~ Dose, data = d), REML = TRUE))

AIC(lmer(LeafArea ~ Dose, data = d, REML = TRUE,
         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))





logLik(lm(LeafArea ~ Dose, data = d))
methods(class= "merMod")
anova.merMod
class(o3)
summary.merMod
merMod
sigma(o)
class(summary_o$varcor)

methods(summary_o$varcor)

cov(summary_o$varcor)

methods(class = "merMod")
anova.merMod
as.data.frame(VarCorr(o))

vcov(o)

library(lmerTest)





as.data.frame(summary_o$varcor)
sdcor2cov()
sqrt( 5.623e-05) * sqrt(1.049e+01) * 0.06

Donner = read.delim("https://dnett.github.io/S510/Donner.txt")
Donner$status<-ifelse(Donner$status=="SURVIVED", 1, 0)
model <- glm(status ~ age + sex, family = binomial(link = "logit"), data = Donner)
summary(model)
confint(model)
plot(model)
logLik(model)
1-pchisq(deviance(model), df.residual(model))
deviance(model)/df.residual(model)

methods(class= "glm")
predict(model)

summary(Donner$age)
15:65
model
conf2 <- function(x){
  b <- coef(model)
  p = 1/(1+exp(-t(x) %*% b))
  sexb = sqrt(t(x)%*% vcov(model) %*% x)
  cixb <- c(t(x) %*% b - 2 *sexb, t(x) %*% b + 2 *sexb)
  bound <- 1 / (1 + exp(-cixb))
  return(c(bound[1], p, bound[2]))
}
conf2(c(1, 20, 1))
age <- 15:65
res <- sapply(age, function(x) conf2(c(1, x, 0)))
boxplot(res, col = "blue")

res2 <- sapply(age, function(x) conf2(c(1, x, 1)))
boxplot(res2, add = T, col = "Red")

legend("topright", c("Male", "Female"), fill = c("red", "blue"))

(9 * 5 +9*5+49*0.95) /(9+9+49) 
