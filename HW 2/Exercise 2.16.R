# Load Data
hardness <- read.table("CH01PR22.txt", header = FALSE, sep = "")
colnames(hardness) <-c("Y","X")
hardness <- as.data.frame(hardness)

# Summarize Data
model <- lm(formula = Y ~ X, data = hardness)
summary <- summary(model)
summary

# Get b0 and b1
b0 <- coef(model)[1]
b1 <- coef(model)[2]

# Calculate Spredmean
n<-nrow(hardness)
n
m <- 10 
xbar <- mean(hardness$X) # Mean of X
xbar
mse <- sum(model$residuals^2)/(nrow(hardness)-2) # MSE
mse
numerator <- ((30-xbar)^2)
denominator <- sum((hardness$X-xbar)^2) #summation(Xi-xbar)^2
denominator
Spredmean <- sqrt(mse)*sqrt(((1/n) + (1/m) + (numerator/denominator)))
Spredmean

# Calculate Spred
Spred <- sqrt(mse)*sqrt(((1/n) + (numerator/denominator)))
Spred

# Part a
newX <- data.frame(X=30) #Needs to be column name
predict(model, newX, interval = "confidence", level = 0.98)

# Part b
newX <- data.frame(X=30) #Needs to be column name
predict(model, newX, interval = "prediction", level = 0.98)

# Part c
alpha <- 0.02
tval <- qt(1-alpha/2, n-2)
tval

yh <- (b0+b1*30)
yh

lowerb <- (yh-tval*Spredmean)
upperb <- (yh+tval*Spredmean) 
predintb <- paste0("[", lowerb, ", ", upperb, "]")
print(predintb)

# Part e
# F value for confidence band
fval <- qf(0.98, 2, n-2)
w <- sqrt(2*fval)
w
# Boundary values for confidence band
lowerb <- (yh-w*Spred)
upperb <- (yh+w*Spred) 
confband <- paste0("[", lowerb, ", ", upperb, "]")
print(confband)

plot(hardness$X,hardness$Y, xlab="X",ylab="Y")
abline(lm(formula = Y ~ X, data = hardness), col = "blue")