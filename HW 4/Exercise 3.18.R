# Load Data
mydata <- read.table("../../datasets/CH03PR18.txt", header = FALSE, sep = "")
colnames(mydata) <-c("Y","X")
mydata <- as.data.frame(mydata)

# Summarize Data
model <- lm(formula = Y ~ X, data = mydata)
summary <- summary(model)
summary

plot(mydata$X, mydata$Y, main="3.18a", xlab="X", ylab = "Y")
abline(model, col="blue")

# Apply sqrt Transformation
mydata$sqrt_X <- sqrt(mydata$X)

# Summarize Data with Transformed Y
tmodel <- lm(formula = Y ~ sqrt_X, data = mydata)
summary_sqrt <- summary(tmodel)
print(summary_sqrt)

plot(mydata$sqrt_X, mydata$Y, main="3.18c", xlab="sqrt(X)", ylab = "Y")
abline(tmodel, col="blue")

#Residual vs Fitted
plot(tmodel, which = 1,main="3.18d", pch=16) + abline(h=0, lty=3)

# Plot Normal Probability Plot
qqnorm(tmodel$residuals, main = "3.18d Normal Q-Q Plot")
qqline(tmodel$residuals, col = "red")