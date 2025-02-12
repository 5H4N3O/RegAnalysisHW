# Load Data
hardness <- read.table("../datasets/CH01PR22.txt", header = FALSE, sep = "")
colnames(hardness) <-c("Y","X")
hardness <- as.data.frame(hardness)

# Summarize Data
model <- lm(formula = Y ~ X, data = hardness)
summary <- summary(model)
summary

# Set up ANOVA table
anova(model)

# F(1-alpha,1,n-2) value for F test
qf(1-.01,1,nrow(hardness)-2)