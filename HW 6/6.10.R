# Load data
mydata <- read.table("../datasets/CH06PR09.txt", header = FALSE, sep = "")
head(mydata)

colnames(mydata) <-c("Y","X1","X2","X3")
my.data <- as.data.frame(mydata)
n <- nrow(my.data)

#Fit MLR
model <- lm(formula = Y ~ X1+X2+X3, data = my.data)
summary(model)
par(mfrow = c(1, 1))

#Get Residuals
model.res <- resid(model)

# Add a column 'i' representing row numbers
my.data$i <- seq_len(nrow(my.data))


boxplot(model.res, main="Box Plot of Residuals", ylab="Residual Values", horizontal=TRUE, col="red")

#Residual vs Fitted
plot(model, which = 1,pch=16)+abline(h=0,lty=3)

# Residuals vs Predictor Plots
plot( my.data$X1, model.res,
      xlab = "X1", 
      ylab = "Residual" , 
      main ="Residuals vs. X1 Plot",
      pch=16)+abline(h=0,lty=3)

plot( my.data$X2, model.res,
      xlab = "X2", 
      ylab = "Residual" , 
      main ="Residuals vs. X2 Plot",
      pch=16)+abline(h=0,lty=3)

plot( my.data$X3, model.res,
      xlab = "X3", 
      ylab = "Residual" , 
      main ="Residuals vs. X3 Plot",
      pch=16)+abline(h=0,lty=3)

#Check for interaction effects
my.data[,4] <- my.data[,1]*my.data[,2]
head(my.data)

colnames(my.data)[4] <- "X1X2"

plot( my.data$X1X2, model.res,
      xlab = "X1X2", 
      ylab = "Residual" , 
      main ="Residuals vs. X1X2 Plot",
      pch=16)+abline(h=0,lty=3)

par(mfrow = c(1, 1))

# QQ-Plot
plot(model, which = 2,pch=16)
#hist(model$residuals,main="Histogram")
time <- my.data$Y
plot(my.data$i, model.res, 
     ylab="Residuals", xlab="Time", 
     main="Time Plot of Residuals") 
abline(0, 0)


# Sort data by a specific column (e.g., Y, assuming it represents time)
sorted_data <- my.data[order(my.data$Y), ]
#View(model.res)
#sorted_res <- model.res[order(model.res), ]

# Recalculate the residuals based on the sorted data
sorted_residuals1 <- head(resid(model),26)
sorted_residuals2 <- tail(resid(model),26)

g1 <- head(sorted_data,26)
n1 <- nrow(g1)
#View(g1)

g2 <- tail(sorted_data,26)
n2 <- nrow(g2)
#View(g2)

#Get Residuals
g2.res <- resid(g2)

d1 <- abs(g1$residuals - median(g1$residuals))
d2 <- abs(g2$residuals - median(g2$residuals))
g2.res
#sorted_residuals1
#sorted_residuals2
d1bar <- mean(d1)
d1bar
d2bar <- mean(d2)

n <- n1+n2

pool.var  <-( sum((d1 - mean(d1))^2) + sum((d2 - mean(d2))^2) )/(n-2)

test.stat <-(d1bar - d2bar) / ( sqrt(pool.var) * sqrt( (1/n1)+ (1/n2)))

test.stat

2*pt(test.stat, n-2, lower.tail = TRUE) # p value < alpha = 0.05 reject H0
qt(1-.005,n-2)
