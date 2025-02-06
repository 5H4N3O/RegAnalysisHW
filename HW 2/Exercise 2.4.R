# Load Data
gpa <- read.table("CH01PR19.txt", header = FALSE, sep = "")
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

# Get 99% Confidence Interval for β1 Manually
n<-nrow(gpa)
alpha <- 0.01
tval <- qt(1-alpha/2, n-2)

lowerb <- (b1-tval*Sb1)
upperb <- (b1+tval*Sb1) 
confint <- paste0("[", lowerb, ", ", upperb, "]")
print(confint)

# Get 99% Confidence Interval for β1 with R function
ci_b1 <- confint(model, level = 0.99)
print(ci_b1)

# Hypothesis test for β1 = 0
tstat <- b1/Sb1
tstat
tval