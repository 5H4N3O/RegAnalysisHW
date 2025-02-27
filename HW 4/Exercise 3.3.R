# Load Data
gpa <- read.table("../datasets/CH01PR19.txt", header = FALSE, sep = "")
colnames(gpa) <-c("Y","X")
gpa <- as.data.frame(gpa)

# Summarize Data
model <- lm(formula = Y ~ X, data = gpa)
summary <- summary(model)
summary

boxplot(gpa$X, main="3.3a", ylab="Xi Values", horizontal=TRUE, col="red")

#Dotplot of residuals
# Dot plot of residuals
dotchart(model$residuals, main="3.3b", xlab="Residual Values", col="blue", pch=16)


#Residual vs Fitted
plot(model, main="3.3c", which = 1, pch=16) + abline(h=0, lty=3)

#Residual vs Xi
#plot(gpa$X, model$residuals, pch=16) + abline(h=0, lty=3)

#Brown Forsyth test---------------------
# H0: Error Variance is Constant
# H1: Error Variance is not Constant
# Reject H0 if p value < alpha
residuals <- model$residuals
data.temp <- cbind(gpa, residuals)

g1 <- data.temp[data.temp$X < 26, ]
n1 <- nrow(g1)

g2 <- data.temp[data.temp$X >= 26, ]
n2 <- nrow(g2)

d1 <- abs(g1$residuals - median(g1$residuals))
d2 <- abs(g2$residuals - median(g2$residuals))

d1bar <- mean(d1)
d2bar <- mean(d2)

n <- n1+n2

pool.var  <-( sum((d1 - mean(d1))^2) + sum((d2 - mean(d2))^2) )/(n-2)

test.stat <-(d1bar - d2bar) / ( sqrt(pool.var) * sqrt( (1/n1)+ (1/n2)))

2*pt(test.stat, n-2, lower.tail = TRUE) # p value < alpha = 0.05 reject H0