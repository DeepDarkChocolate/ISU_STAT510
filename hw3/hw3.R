library(Sleuth3)
library(MASS)

data1 <- case0501

boxplot(Lifetime ~ Diet, data = data1, xlab = "Treatment Group")

lm1 <- lm(Lifetime ~ 0 + Diet, data = data1)
anova1 <- anova(lm1)
(SSE1 <- anova1$"Sum Sq"[2])

(sigma <- anova1$"Mean Sq"[2])

GrDiet <- data1$Diet
levels(GrDiet)
levels(GrDiet) <- c("A", "A", "A", "NP", "R/R50", "A")
data1 <- cbind.data.frame(data1, GrDiet)

lm2 <- lm(Lifetime ~ 0 + GrDiet, data = data1)
anova2 <- anova(lm2)
(SSE2 <- anova2$"Sum Sq"[2])

SSR = SSE2 - SSE1
Fstat <- SSR / 3 / (SSE1 / 343)
pf(Fstat, 3, 343, lower.tail = FALSE)

anova(lm2, lm1)

C <- matrix(c(1,0,-1,0,0,0,0,1,0,0,0,-1,1,-1,1,0,0,-1), byrow = TRUE, nrow = 3)
d <- 0
beta <- lm1$coefficients

Cbeta <- C%*%beta - d
X <- model.matrix(~ 0 + Diet, data = data1)
dnum <- drop(t(Cbeta) %*% solve(C%*%ginv(t(X) %*% X)%*%t(C), Cbeta))
dnum / 3 / sigma

