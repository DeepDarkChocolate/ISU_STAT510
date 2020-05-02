library(lme4)
library("lmerTest")

dat <- read.csv("https://dnett.github.io/S510/hw10GenotypeYield.txt", sep = "\t")
dat$genotype <- factor(dat$genotype)
#(a)
(model1 <- lm(yield ~ 0 + genotype, data = dat))
BLUE <- model1$coefficients
length(BLUP)
#(b)
(model2 <- lmer(yield ~ 0 + 1|genotype, data = dat))
#sigma_g^2 = 2.687^2 = 7.22
#sigma_e^2 = 9.669^2 = 93.49

#(c)
BLUP <- unlist(ranef(model2)) + fixef(model2)
names(BLUP) <- names(BLUE)
BLUP

#(d)
plot(BLUE ~ BLUP)
abline(0, 1)

#(e)
sort(BLUE, decreasing = TRUE)[1:5]

#(f)
sort(BLUP, decreasing = TRUE)[1:5]

# 3
y = c(14, 9, 10, 5, 18, 9, 9, 17, 10, 17,18,13, 17, 16)
G = rep(c(1,2),each = 7)
G = as.factor(G)

model1 = glm(y ~ 0 + G, family = "poisson")
s1 = summary(model1)

model2 = glm(y ~ 1, family = "poisson")
s2 = summary(model2)

##(a)
AIC(model1)

##(b)
BIC(model1)

##(c)
AIC(model2)

##(d)
BIC(model2)

##(g)
- 2 * logLik(model2) + 2 * logLik(model1)

##(h)
aov <- anova(model1, model2, test = "LRT")
aov$`Pr(>Chi)`[2]

##(i)
V <- s1$cov.unscaled
thetahat <- s1$coefficients[,1]
(thetahat[2] - thetahat[1]) / sqrt(V[1,1] + V[2,2])

##(j)
aov <- anova(model1, model2, test = "Rao")
aov$`Pr(>Chi)`[2]

## 5


ftn1 <- function(x){
  17 * log(x) + 83 * log(1 - x)
}

lphat = ftn1(phat)
lik2 = lphat - 1/2*qchisq(1- 0.05, 1)

ftn2 <- function(x){
  17 * log(x) + 83 * log(1 - x) - lik2
}

BFfzero(ftn2, 0, phat)
BFfzero(ftn2, phat, 1)
