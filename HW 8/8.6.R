# Load data
mydata <- read.table("../../datasets/CH08PR06.txt", header = FALSE, sep = "")
head(mydata)
#View(mydata)

colnames(mydata) <-c("Y","X")
my.data <- as.data.frame(mydata)
n <- nrow(my.data)
p <- 3 # 3 predictors

my.data$XC <- my.data$X - mean(my.data$X)

#Fit MLR
model <- lm(formula = Y ~ XC + I(XC^2), data = my.data)
summary(model)
qt(1-alpha/2,n-p)

#plot(model)

alpha <- (1-.99)
g <- 3

B <- qt(1-alpha/(2*g),n-p)
B
F <- qf(1-alpha,p,n-p)
W<-sqrt(2*F)
W
xbar <- mean(my.data$X) # Mean of X

# Calculate Spred
mse <- sum(model$residuals^2)/(n-p) # MSE
#mse <-(3.153^2)
mse
numerator <- ((20-xbar)^2)
denominator <- sum((my.data$X-xbar)^2) #summation(Xi-xbar)^2
denominator
#SYhat <- (sqrt(mse))*sqrt(((1/n) + (numerator/denominator)))
SYhat <- sqrt( mse * ( (1/n) + (numerator/denominator) ) )
SYhat

#predict(model, newdata = data.frame("XC" = c(10) - mean(my.data$X)),interval = "predict", level = 0.99) |> round(3)

# Calculate Spred
numerator <- ((15-xbar)^2)
Spred <- sqrt(mse)*sqrt(((1/n) + 1 + (numerator/denominator)))
Spred

predT <- qt(1-(alpha/2),n-p)
predT

modelOrig <- lm(formula = Y ~ X + I(X^2), data = my.data)
summary(modelOrig)



