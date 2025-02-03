# Load Data
crimerate <- read.table("CH01PR28.txt", header = FALSE, sep = "")

#View to verify
View(crimerate)

colnames(crimerate) <-c("Y","X")
crimerate <- as.data.frame(crimerate)

# View Data
head(crimerate)

# Summarize Data
model <- lm(formula = Y ~ X, data = crimerate)
summary(model)

# Scatter plot with regression line
plot(crimerate$X,crimerate$Y, xlab="% with HS Diploma",ylab="Crime Rate")
abline(lm(formula = Y ~ X, data = crimerate), col = "blue")

# Get the 10th residual (e10)
residuals <- residuals(model)
residuals[10]

#MSE
sum(model$residuals^2)/(nrow(crimerate)-2)	