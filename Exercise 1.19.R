# Load Data
gpa <- read.table("CH01PR19.txt", header = FALSE, sep = "")
colnames(gpa) <-c("Y","X")
gpa <- as.data.frame(gpa)

# View Data
head(gpa)

# Summarize Data
model <- lm(formula = Y ~ X, data = gpa)
summary(model)

# Scatter plot with regression line
plot(gpa$X,gpa$Y, xlab="X",ylab="Y")
abline(lm(formula = Y ~ X, data = gpa), col = "blue")

# Sum of all residuals
sum(model$residuals) 

#MSE
sum(model$residuals^2)/(nrow(gpa)-2)	