# Load Data
gpa <- read.table("CH01PR19.txt", header = FALSE, sep = "")
colnames(gpa) <-c("Y","X")
gpa <- as.data.frame(gpa)

# Summarize Data
model <- lm(formula = Y ~ X, data = gpa)
summary <- summary(model)
summary

# Get b0 and b1
b0 <- coef(model)[1]
b1 <- coef(model)[2]

#Confidence interval for Observed X
n<-nrow(gpa)
alpha <- 0.05
tval <- qt(1-alpha/2, n-2)
tval

# Get S(predmean) practice
m <- sum(gpa$X == 28) # Count how many entries with X=28
xbar <- mean(gpa$X) # Mean of X
xbar
mse <- sum(model$residuals^2)/(nrow(gpa)-2) # MSE
mse
numerator <- ((28-xbar)^2)
denominator <- sum((gpa$X-xbar)^2) #summation(Xi-xbar)^2
denominator
Spredmean <- sqrt(mse)*sqrt(((1/n) + (numerator/denominator)))
Spredmean

# Part a
newX <- data.frame(X=28) #Needs to be column name
predict(model, newX, interval = "confidence", level = 0.95)

# Part b
newX <- data.frame(X=28) #Needs to be column name
predict(model, newX, interval = "prediction", level = 0.95)

# F value for confidence band
fval <- qf(0.95, 2, 118)
w <- sqrt(2*fval)
w
yh <- (b0 + b1*28)
yh

# Boundary values for confidence band
lowerb <- (yh-w*Spredmean)
upperb <- (yh+w*Spredmean) 
confband <- paste0("[", lowerb, ", ", upperb, "]")
print(confband)

