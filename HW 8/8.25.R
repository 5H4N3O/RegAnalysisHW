# Load data
my.data <- read.table("../../datasets/CH06PR09.txt", header = FALSE, sep = "")

colnames(my.data) <-c("Y","X1","X2","X3")
head(my.data)

my.data <- as.data.frame(my.data)
n <- nrow(my.data)

my.data$XC <- my.data$X1 - mean(my.data$X1)
#my.data$X3C <- my.data$X3 - mean(my.data$X3)

my.data$x1 <- my.data$X1 - mean(my.data$X1)

model_c <- lm(Y ~ x1 + I(x1^2) + X3
              + I(x1 * X3) + I(x1^2 * X3),
              data = my.data)
summary(model_c)

#Fit MLR
#model <- lm(formula = Y ~ XC + XC^2 + X3 + I(XC*X3) + I(XC^2*X3), data = my.data)
#summary(model)

modelReduced <- lm(Y~ x1+X3, data=my.data)
summary(modelReduced)

anova(modelReduced)
anova(model_c)

fstat <- qf(.95,3,46)
fstat
1-pf(.022317, 3,46)
