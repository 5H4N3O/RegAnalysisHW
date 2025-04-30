# Load data
data1 <- read.table("../../datasets/CH01PR19.txt", header = FALSE, sep = "")
colnames(data1) <-c("Y","X1")

data2 <- read.table("../../datasets/CH08PR16.txt", header = FALSE, sep = "")
colnames(data2) <-c("X2")

my.data <- cbind(data1,data2)

#Fit MLR
model <- lm(formula = Y ~ X1 + X2, data = my.data)
summary(model)

plot( my.data$X1*my.data$X2, model$residuals,
      xlab = "X1X2", 
      ylab = "Residual" , 
      main ="Residuals vs. X1X2 Plot (8.16d)",
      pch=16)+abline(h=0,lty=3)