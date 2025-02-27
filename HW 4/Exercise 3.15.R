# Load Data
solcon <- read.table("../../datasets/CH03PR15.txt", header = FALSE, sep = "")
colnames(solcon) <-c("Y","X")
solcon <- as.data.frame(solcon)

# Summarize Data
model <- lm(formula = Y ~ X, data = solcon)
summary <- summary(model)
summary

plot(solcon$X, solcon$Y, xlab="X", ylab = "Y")
abline(model, col="blue")

# Lack of Fit Test
# H0: The relationship assumed in the model is reasonable, i.e., there is no lack of fit.
# H1: The relationship assumed in the model is not reasonable, i.e., there is a lack of fit
# Reject H0 if p value < alpha

reduced.model <- lm(formula = Y ~ X, data = solcon)
full.model    <- lm(formula = Y ~ 0 + as.factor(X), data = solcon)

anova(reduced.model,full.model)