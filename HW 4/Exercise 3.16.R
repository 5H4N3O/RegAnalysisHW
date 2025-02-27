# Load Data
solcon <- read.table("../../datasets/CH03PR15.txt", header = FALSE, sep = "")
colnames(solcon) <-c("Y","X")
solcon <- as.data.frame(solcon)

# Summarize Data
model <- lm(formula = Y ~ X, data = solcon)
summary <- summary(model)
summary

plot(solcon$X, solcon$Y, main="3.16a", xlab="X", ylab = "Y")
abline(model, col="blue")

# Apply log10 Transformation
solcon$log10_Y <- log10(solcon$Y)

# Summarize Data with Transformed Y
model_log10 <- lm(formula = log10_Y ~ X, data = solcon)
summary_log10 <- summary(model_log10)
print(summary_log10)

plot(solcon$X, solcon$log10_Y, main="3.16d", xlab="X", ylab = "Log10(Y)")
abline(model_log10, col="blue")

#Residual vs Fitted
plot(model_log10, main="3.16e", which = 1, pch=16) + abline(h=0, lty=3)

# Plot Normal Probability Plot
qqnorm(model_log10$residuals, main = "3.16e Normal Q-Q Plot")
qqline(model_log10$residuals, col = "red")