# Load Data
econsume <- read.table("../../datasets/CH03PR09.txt", header = FALSE, sep = "")
colnames(econsume) <-c("X","Y")
econsume <- as.data.frame(econsume)

# Summarize Data
model <- lm(formula = Y ~ X, data = econsume)
summary <- summary(model)
summary


#Residual vs Fitted
#plot(model, main="3.9", which = 1, pch=16) + abline(h=0, lty=3)

#Residual vs Xi
plot(econsume$X, model$residuals, main="3.9", pch=16) + abline(h=0, lty=3)

# Shift Y to make it positive
econsume$Y_shifted <- econsume$Y + abs(min(econsume$Y)) + 1  

# Load MASS library for Box-Cox transformation
library(MASS)
#boxcox(lm(Y_shifted ~ X, data=econsume), lambda = seq(-2, 2, by = 0.1))

econsume$Y_transformed <- log(econsume$Y_shifted)  # Ensure Y is positive
model_transformed <- lm(Y_transformed ~ X, data=econsume)
summary(model_transformed)

#Residual vs Fitted
#plot(model_transformed, which = 1, pch=16) + abline(h=0, lty=3)