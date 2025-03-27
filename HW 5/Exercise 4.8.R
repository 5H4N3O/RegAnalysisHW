# Load Data
mydata <- read.table("../../datasets/CH01PR21.txt", header = FALSE, sep = "")
colnames(mydata) <-c("Y","X")
mydata <- as.data.frame(mydata)

# Summarize Data
model <- lm(formula = Y ~ X, data = mydata)
summary <- summary(model)
summary

n <- nrow(mydata)
alpha <- (1-.95)

qf(1-alpha,2,n-2)
n
xbar <- mean(mydata$X) # Mean of X
xbar

# Calculate Spred
mse <- sum(model$residuals^2)/(n-2) # MSE
mse
numerator <- ((2-xbar)^2)
denominator <- sum((mydata$X-xbar)^2) #summation(Xi-xbar)^2
denominator
Spred <- sqrt(mse)*sqrt(((1/n) + (numerator/denominator)))
Spred

B <- qt(1-alpha/6,n-2)
B

qf(1-alpha,3,n-2)

# Calculate Spred
numerator <- ((2-xbar)^2)
Spredmean <- sqrt(mse)*sqrt(((1/n) + 1 + (numerator/denominator)))
Spredmean
