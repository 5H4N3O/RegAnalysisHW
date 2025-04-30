# Load data
data1 <- read.table("../../datasets/CH01PR19.txt", header = FALSE, sep = "")
colnames(data1) <-c("Y","X1")

data2 <- read.table("../../datasets/CH08PR16.txt", header = FALSE, sep = "")
colnames(data2) <-c("X2")

my.data <- cbind(data1,data2)

#Fit MLR
model <- lm(formula = Y ~ X1 + X2 + I(X1*X2), data = my.data)
summary(model)

qt(.975,116)