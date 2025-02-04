# Load Data
airfreight <- read.table("CH01PR21.txt", header = FALSE, sep = "")
colnames(airfreight) <-c("Y","X")
airfreight <- as.data.frame(airfreight)

# Summarize Data
model <- lm(formula = Y ~ X, data = airfreight)
summary <- summary(model)
summary

# Store data in Variables
b0 <- coef(model)[1]
Sb0 <- summary$coefficients[1, 2] 

b1 <- coef(model)[2]
Sb1 <- summary$coefficients[2, 2] 

# Get T value for confidence intervals
n<-nrow(airfreight)
alpha <- 0.05
tval <- qt(1-alpha/2, n-2)

# Get 95% Confidence Interval for β1 Manually
lowerb1 <- (b1-tval*Sb1)
upperb1 <- (b1+tval*Sb1) 
confintb1 <- paste0("[", lowerb1, ", ", upperb1, "]")
print(confintb1)

# Verify 95% Confidence Interval for β1 with R function
ci_b1 <- confint(model, level = 0.95)
print(ci_b1)

# Hypothesis test for β1 = 0
tstat <- b1/Sb1
tstat
tval

# Get 95% Confidence Interval for β0 Manually
lowerb0 <- (b0-tval*Sb0)
upperb0 <- (b0+tval*Sb0) 
confintb0 <- paste0("[", lowerb0, ", ", upperb0, "]")
print(confintb0)

# Verify 95% Confidence Interval for β0 with R function
ci_b0 <- confint(model, level = 0.95)
print(ci_b0)

# Hypothesis test for β0 < 9
tstat <- (b0-9)/Sb0
tstat
tval
p_value_one_tailed <- 1 - pt(tstat, n-2)
p_value_one_tailed
