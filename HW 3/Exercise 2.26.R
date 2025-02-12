# Load Data
hardness <- read.table("../datasets/CH01PR22.txt", header = FALSE, sep = "")
colnames(hardness) <-c("Y","X")
hardness <- as.data.frame(hardness)

# Summarize Data
model <- lm(formula = Y ~ X, data = hardness)
summary <- summary(model)
summary

# Store data in Variables
b0 <- coef(model)[1]
Sb0 <- summary$coefficients[1, 2] 
b1 <- coef(model)[2]
Sb1 <- summary$coefficients[2, 2] 

# Set up ANOVA table
anova(model)

# F(1-alpha,1,n-2) value for F test
qf(1-.01,1,nrow(hardness)-2)