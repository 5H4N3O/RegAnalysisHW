# Load data
mydata <- read.table("../datasets/CH06PR09.txt", header = FALSE, sep = "")
head(mydata)

colnames(mydata) <-c("Y","X1","X2","X3")
my.data <- as.data.frame(mydata)
n <- nrow(my.data)

#Fit MLR
model <- lm(formula = Y ~ X1+X2+X3, data = my.data)
summary(model)

qf(.95,4,48)
qt(1-.00625,48)

#Get Residuals
model.res <- resid(model)
#sample variance covariance matrix of b
cov_b <- vcov(model)
cov_b
y.new1 <- predict(model,data.frame(X1=230000,X2=7.5,X3=0))
y.new2 <- predict(model,data.frame(X1=250000,X2=7.3,X3=0))

#Sample variance of each prediction
#s^2_pred = MSE + X'_new Cov(b) X_h
MSE <- 143.3^2
x.new1 <- matrix( c(1,230000,7.5,0), nrow=4, byrow = TRUE)
var.pred1 <- MSE+t(x.new1)%*%cov_b%*%x.new1

x.new2 <- matrix( c(1,250000,7.3,0), nrow=4, byrow = TRUE)
var.pred2 <- MSE+t(x.new2)%*%cov_b%*%x.new2

B <- qt(1-(0.05/(2*4)), n-4)
B

y.new1
sqrt(var.pred1)
y.new1 - B*sqrt(var.pred1)
y.new1 + B*sqrt(var.pred1)

#y.new2
#sqrt(var.pred2)
#y.new2 - B*sqrt(var.pred2)
#y.new2 + B*sqrt(var.pred2)
y.new3 <- predict(model,data.frame(X1=282000,X2=7.1,X3=0))

x.new3 <- matrix( c(1,282000,7.1,0), nrow=4, byrow = TRUE)
var.pred3 <- (MSE/3)+t(x.new3)%*%cov_b%*%x.new3

y.new3
sqrt(var.pred3)
y.new3 - B*sqrt(var.pred3)
y.new3 + B*sqrt(var.pred3)

qt(1-(0.05/(2)), n-4)


# Define new shipment predictor values
x_h <- matrix(c(1, 282000, 7.10, 0), nrow=4, byrow=TRUE)

# Compute variance of predicted mean response
var_pred_mean <- (MSE / 3) + t(x_h) %*% cov_b %*% x_h

# Compute standard deviation of predicted mean response
S_pred_mean <- sqrt(var_pred_mean)
S_pred_mean


