# Load Data
gpa <- read.table("../datasets/CH01PR19.txt", header = FALSE, sep = "")
colnames(gpa) <-c("Y","X")
gpa <- as.data.frame(gpa)

# Summarize Data
model <- lm(formula = Y ~ X, data = gpa)
summary <- summary(model)
summary

# Store data in Variables
b0 <- coef(model)[1]
Sb0 <- summary$coefficients[1, 2] 
b1 <- coef(model)[2]
Sb1 <- summary$coefficients[2, 2] 