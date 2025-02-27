# Load Data
concentration <- read.table("../../datasets/CH03PR11.txt", header = FALSE, sep = "")
colnames(concentration) <-c("X","Y")
concentration <- as.data.frame(concentration)

# Summarize Data
model <- lm(formula = Y ~ X, data = concentration)
summary <- summary(model)

#Residual vs Fitted
#plot(model, which = 1, pch=16) + abline(h=0, lty=3)

#Residual vs Xi
plot(concentration$X, main="3.11a", model$residuals, pch=16) + abline(h=0, lty=3)

#Breusch-Pagan Test
library(lmtest)
bptest(model,studentize = FALSE) #-> have constant variance