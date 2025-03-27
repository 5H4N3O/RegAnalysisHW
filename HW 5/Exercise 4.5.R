# Load Data
mydata <- read.table("../../datasets/CH01PR22.txt", header = FALSE, sep = "")
colnames(mydata) <-c("Y","X")
mydata <- as.data.frame(mydata)

# Summarize Data
model <- lm(formula = Y ~ X, data = mydata)
summary <- summary(model)
summary
n <- nrow(mydata)
alpha <- (1-.9)

qt(1-alpha/4,n-2)

xbar <- mean(mydata$X) # Mean of X
xbar